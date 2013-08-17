#!/usr/bin/python


# This file is part of pylastfp.
# Copyright 2010, Adrian Sampson.
#
# pylastfp is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# pylastfp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with pylastfp. If not, see
# <http://www.gnu.org/licenses/>.

"""Use Gstreamer to decode audio files.

To read an audio file, pass it to the constructor for GstAudioFile()
and then iterate over the contents:

>>> f = GstAudioFile('something.mp3')
>>> try:
>>> for block in f:
>>> ...
>>> finally:
>>> f.close()

Note that there are a few complications caused by Gstreamer's
asynchronous architecture. This module spawns its own Gobject main-
loop thread; I'm not sure how that will interact with other main
loops if your program has them. Also, in order to stop the thread
and terminate your program normally, you need to call the close()
method on every GstAudioFile you create. Conveniently, the file can be
used as a context manager to make this simpler:

>>> with GstAudioFile('something.mp3') as f:
>>> for block in f:
>>> ...

Iterating a GstAudioFile yields strings containing short integer PCM
data. You can also read the sample rate and channel count from the
file:

>>> with GstAudioFile('something.mp3') as f:
>>> print f.samplerate
>>> print f.channels
>>> print f.duration
"""
from __future__ import with_statement

import gst
import sys
import gobject
import threading
import os
import urllib
import Queue

import scipy
import struct

import gtk

from matplotlib.figure import Figure
from numpy import arange, sin, pi
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas

QUEUE_SIZE = 10
BUFFER_SIZE = 10
SENTINEL = '__GSTDEC_SENTINEL__'


# Exceptions.

class UnknownTypeError(Exception):
    """Raised when Gstreamer can't decode the given file type."""
    def __init__(self, streaminfo):
        super(UnknownTypeError, self).__init__(
            "can't decode stream: " + streaminfo
        )
        self.streaminfo = streaminfo

class FileReadError(Exception):
    """Raised when the file can't be read at all."""
    pass

class NoStreamError(Exception):
    """Raised when the file was read successfully but no audio streams
were found.
"""
    def __init__(self):
        super(NoStreamError, self).__init__('no audio streams found')


# Managing the Gobject main loop thread.

_shared_loop_thread = None
_loop_thread_lock = threading.RLock()
gobject.threads_init()
def get_loop_thread():
    """Get the shared main-loop thread. It is automatically acquired;
you must call release() on it later.
"""
    global _shared_loop_thread
    with _loop_thread_lock:
        if _shared_loop_thread and not _shared_loop_thread.running:
            # The thread has been released & stopped and needs to be
            # thrown away.
            _shared_loop_thread = None
        if not _shared_loop_thread:
            # Start a new thread.
            _shared_loop_thread = MainLoopThread()
            _shared_loop_thread.start()
        _shared_loop_thread.acquire()
        return _shared_loop_thread
class MainLoopThread(threading.Thread):
    """A thread encapsulating a Gobject main loop. It uses a primitive
little reference-counter scheme to make sure the loop stops when
everybody's done with it. Call acquire() and release() to start and
stop using the loop.
"""
    def __init__(self):
        super(MainLoopThread, self).__init__()
        self.loop = gobject.MainLoop()
        self.running = False
        self.references = 0

    def run(self):
        self.running = True
        self.loop.run()

    def acquire(self):
        with _loop_thread_lock:
            self.references += 1

    def release(self):
        with _loop_thread_lock:
            self.references -= 1
            if self.references <= 0 and self.running:
                # Stop the loop entirely. This thread is now dead and
                # should be thrown away.
                self.loop.quit()
                self.join()
                self.running = False


# The decoder.

class GstAudioFile(object):
    """Reads raw audio data from any audio file that Gstreamer
knows how to decode.
>>> with GstAudioFile('something.mp3') as f:
>>> print f.samplerate
>>> print f.channels
>>> print f.duration
>>> for block in f:
>>> do_something(block)
Iterating the object yields blocks of 16-bit PCM data. Three
pieces of stream information are also available: samplerate (in Hz),
number of channels, and duration (in nanoseconds).
It's very important that the client call close() when it's done
with the object. Otherwise, the program is likely to hang on exit.
Alternatively, of course, one can just use the file as a context
manager, as shown above.
"""
    def __init__(self, path):
        self.running = False

        # Set up the Gstreamer pipeline.
        self.pipeline = gst.Pipeline()
        self.dec = gst.element_factory_make("uridecodebin")
        self.conv = gst.element_factory_make("audioconvert")
        self.sink = gst.element_factory_make('appsink')

        # Register for bus signals.
        bus = self.pipeline.get_bus()
        bus.add_signal_watch()
        bus.connect("message::eos", self._message)
        bus.connect("message::error", self._message)

        # Configure the input.
        uri = 'file://' + urllib.quote(path)
        self.dec.set_property("uri", uri)
        # The callback to connect the input.
        self.dec.connect("pad-added", self._pad_added)
        self.dec.connect("no-more-pads", self._no_more_pads)
        # And a callback if decoding failes.
        self.dec.connect("unknown-type", self._unkown_type)

        # Configure the output.
        # We want short integer data.
        self.sink.set_property('caps',
            gst.Caps('audio/x-raw-int, width=16, depth=16, signed=true')
        )
        # TODO set endianness?
        # Set up the characteristics of the output. We don't want to
        # drop any data (nothing is real-time here); we should bound
        # the memory usage of the internal queue; and, most
        # importantly, setting "sync" to False disables the default
        # behavior in which you consume buffers in real time. This way,
        # we get data as soon as it's decoded.
        self.sink.set_property('drop', False)
        self.sink.set_property('max-buffers', BUFFER_SIZE)
        self.sink.set_property('sync', False)
        # The callback to receive decoded data.
        self.sink.set_property('emit-signals', True)
        self.sink.connect("new-buffer", self._new_buffer)

        # We'll need to know when the stream becomes ready and we get
        # its attributes. This semaphore will become available when the
        # caps are received. That way, when __init__() returns, the file
        # (and its attributes) will be ready for reading.
        self.ready_sem = threading.Semaphore(0)
#        self.sink.get_pad("sink").connect("notify::caps", self._notify_caps)
        self.caps_handler = self.sink.get_pad("sink").connect(
            "notify::caps", self._notify_caps
        )

        # Link up everything but the decoder (which must be linked only
        # when it becomes ready).
        self.pipeline.add(self.dec, self.conv, self.sink)
        self.conv.link(self.sink)

        # Set up the queue for data and run the main thread.
        self.queue = Queue.Queue(QUEUE_SIZE)
        self.thread = get_loop_thread()

        # This wil get filled with an exception if opening fails.
        self.read_exc = None

        # Return as soon as the stream is ready!
        self.pipeline.set_state(gst.STATE_PAUSED)
        self.ready_sem.acquire()
        if self.read_exc:
            # An error occurred before the stream became ready.
            self.close()
            raise self.read_exc
        self.running = True


    # Gstreamer callbacks.

    def _notify_caps(self, pad, args):
        # The sink has started to receive data, so the stream is ready.
        # This also is our opportunity to read information about the
        # stream.
        info = pad.get_negotiated_caps()[0]

        # Stream attributes.
        self.channels = info['channels']
        self.samplerate = info['rate']

        # Query duration.
        q = gst.query_new_duration(gst.FORMAT_TIME)
        if pad.get_peer().query(q):
            # Success.
            format, length = q.parse_duration()
            if format == gst.FORMAT_TIME:
                self.duration = length
            else:
                # Not sure what happened.
                self.duration = None
        else:
            # Failure.
            self.duration = None

        # Allow constructor to complete.
        self.ready_sem.release()

    _got_a_pad = False
    def _pad_added(self, element, pad):
        # Decoded data is ready. Connect up the decoder, finally.
        name = pad.get_caps()[0].get_name()
        if name.startswith('audio/x-raw-'):
            nextpad = self.conv.get_pad('sink')
            if not nextpad.is_linked():
                self._got_a_pad = True
                pad.link(nextpad)
            self.pipeline.set_state(gst.STATE_PLAYING)

    def _no_more_pads(self, element):
        # Sent when the pads are done adding (i.e., there are no more
        # streams in the file). If we haven't gotten at least one
        # decodable stream, raise an exception.
        if not self.running and not self._got_a_pad:
            self.read_exc = NoStreamError()
            self.ready_sem.release()

    def _new_buffer(self, sink):
#        if self.running:
        # New data is available from the pipeline! Dump it into our
        # queue (or possibly block if we're full).
        buf = sink.emit('pull-buffer')
        self.queue.put(str(buf))

    def _unkown_type(self, uridecodebin, decodebin, caps):
        # This is called *before* the stream becomes ready when the
        # file can't be read.
        streaminfo = caps.to_string()
        if not streaminfo.startswith('audio/'):
            # Ignore non-audio (e.g., video) decode errors.
            return
        self.read_exc = UnknownTypeError(streaminfo)
        self.ready_sem.release()

    def _message(self, bus, message):
        if message.type == gst.MESSAGE_EOS:
            # The file is done. Tell the consumer thread.
            self.queue.put(SENTINEL)
        elif message.type == gst.MESSAGE_ERROR:
            gerror, debug = message.parse_error()
            if 'not-linked' in debug:
                self.read_exc = NoStreamError()
            elif 'No such file' in debug:
                self.read_exc = IOError('resource not found')
            else:
                self.read_exc = FileReadError(debug)
            self.ready_sem.release()

    # Iteration.
    def next(self):
        # Wait for data from the Gstreamer callbacks.
        val = self.queue.get()
        if val == SENTINEL:
            # End of stream.
            raise StopIteration
        return val
    def __iter__(self):
        return self

    # Cleanup.
    def close(self):
        if self.running:
            self.running = False

            # Stop reading the file.
            self.dec.set_property("uri", None)
            # Block spurious signals.
            self.sink.get_pad("sink").disconnect(self.caps_handler)

            # Make space in the output queue to let the decoder thread
            # finish. (Otherwise, the thread blocks on its enqueue and
            # the interpreter hangs.)
            try:
                self.queue.get_nowait()
            except Queue.Empty:
                pass

            # Halt the pipeline (closing file).
            self.pipeline.set_state(gst.STATE_NULL)

            # Clean up the thread.
            self.thread.release()

    def __del__(self):
        self.close()

    # Context manager.
    def __enter__(self):
        return self
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False

def read_as_array(filename):
    """reads audio file as scipy array using gstreamer framework

    return:
        data as scipy array
        duration in seconds
        channels as int
        samplerate
    """
    path = os.path.abspath(os.path.expanduser(filename))
    with GstAudioFile(path) as f:
        samplerate = f.samplerate
        duration = float(f.duration) / 1000000000 # in seconds
        channels = f.channels

        data_left = []
        data_right = []
        data_mono = []
        for s in f:
            # http://docs.python.org/library/struct.html
            # little or big endian is choosen automatically by python
            # if its stereo (2 channels): first one is left channel, second is right channel, third is left channel...
            # every short (h) is 2 bytes long and padding on stereo with two x (xx) for other channel
            if channels == 1:
                data_mono += list(struct.unpack( ("h"*(len(s)/2)), s))
            elif channels == 2:
                data_left += list(struct.unpack( ("hxx"*(len(s)/4)), s))
                data_right += list(struct.unpack( ("xxh"*(len(s)/4)), s))

        if channels == 1:
            data = scipy.array(data_mono)
        elif channels == 2:
            data = scipy.array([data_left, data_right])

    return data, duration, channels, samplerate

# Test
if __name__ == '__main__':

    win = gtk.Window()
    win.connect("destroy", lambda x: gtk.main_quit())
    win.set_default_size(400,300)
    win.set_title("Embedding in GTK")


    data, duration, channels, samplerate = read_as_array("/home/px/gare_du_nord-catchlak.mp3.wav")
    print ("duration: "+str(duration)+" seconds\nchannels: "+str(channels)+"\nsamplerate: "+str(samplerate))

    print "data :", data

    if channels == 1:
        print len(data)
    elif channels == 2:
        # left channel
        print repr(data[0,:])
        print len(data[0,:])
        # right channel
        print repr(data[1,:])
        print len(data[1,:])
        f = Figure(facecolor = 'w')
        g = Figure(facecolor = 'w')
        a = f.add_subplot(111, axisbg='w')
        a.plot(range(len(data[0,:])),data[0,:], color="OrangeRed",  linewidth=0.5, linestyle="-")
        b = g.add_subplot(111, axisbg='w')
        b.plot(range(len(data[1,:])),data[1,:], color="OrangeRed",  linewidth=0.5, linestyle="-")

        f.subplots_adjust(0, 0, 1, 1)
        g.subplots_adjust(0, 0, 1, 1)
        a.axis('off')
        b.axis('off')

    canvas_left = FigureCanvas(f)  # a gtk.DrawingArea
    canvas_right = FigureCanvas(g)  # a gtk.DrawingArea

    vbox = gtk.VBox()
    vbox.pack_start(canvas_left)
    vbox.pack_start(canvas_right)

    win.add(vbox)

    win.show_all()
    gtk.main()
