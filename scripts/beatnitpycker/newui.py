#!/usr/bin/env python

import os, stat, time
import pprint
# import pygtk
import gtk

import pygst
pygst.require('0.10')
import gst
import gobject

from matplotlib.figure import Figure
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas
import scipy.io.wavfile as wavfile

interface = """
<ui>
    <menubar name="MenuBar">
        <menu action="File">
            <menuitem action="New"/>
            <menuitem action="Open"/>
            <menuitem action="Save"/>
            <menuitem action="Quit"/>
        </menu>
        <menu action="Edit">
            <menuitem action="Preferences"/>
        </menu>
        <menu action="Help">
            <menuitem action="About"/>
        </menu>
    </menubar>
</ui>
"""

class Lister(object):
    column_names = ['Name', 'Size', 'Mode', 'Last Changed']

    def button_clicked(list, event):
        # Check for right click
        if event.button == 3:
            sel=list.get_selection_info(event.x,event.y)
            list.select_row(sel[0], sel[1])
            print "Click."
            print list.selection

    def __init__(self, dname = None):

        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)

        listmodel = self.make_list(dname)

        # create the TreeView
        self.treeview = gtk.TreeView()

        # create the TreeViewColumns to display the data
        self.tvcolumn = [None] * len(self.column_names)
        cellpb = gtk.CellRendererPixbuf()
        self.tvcolumn[0] = gtk.TreeViewColumn(self.column_names[0], cellpb)
        self.tvcolumn[0].set_cell_data_func(cellpb, self.file_pixbuf)
        cell = gtk.CellRendererText()
        self.tvcolumn[0].pack_start(cell, False)
        self.tvcolumn[0].set_cell_data_func(cell, self.file_name)
        self.treeview.append_column(self.tvcolumn[0])
        for n in range(1, len(self.column_names)):
            cell = gtk.CellRendererText()
            self.tvcolumn[n] = gtk.TreeViewColumn(self.column_names[n], cell)
            if n == 1:
                cell.set_property('xalign', 1.0)
            self.tvcolumn[n].set_cell_data_func(cell, cell_data_funcs[n])
            self.treeview.append_column(self.tvcolumn[n])

        self.treeview.connect('row-activated', self.open_file)
        self.treeview.set_model(listmodel)
        return

    def make_list(self, dname=None):
        if not dname:
            self.dirname = os.path.expanduser('~')
        else:
            self.dirname = os.path.abspath(dname)
        # self.window.set_title(self.dirname)
        files = [f for f in os.listdir(self.dirname) if f[0] != '.']
        files.sort()
        files = ['..'] + files
        listmodel = gtk.ListStore(object)
        for f in files:
            listmodel.append([f])
        return listmodel

    def open_file(self, treeview, path, column):
        model = treeview.get_model()
        iter = model.get_iter(path)
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            new_model = self.make_list(filename)
            treeview.set_model(new_model)
        else:
            Engine().load_file(filename)
        # return

    def file_pixbuf(self, column, cell, model, iter):
        audioFormats = [ ".wav", ".mp3", ".ogg", ".flac" ]
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            pb = gtk.icon_theme_get_default().load_icon("folder", 24, 0)
        elif filename.endswith(tuple(audioFormats)):
            pb = gtk.icon_theme_get_default().load_icon("audio-volume-medium", 24, 0)
        else:
            pb = gtk.icon_theme_get_default().load_icon("edit-copy", 24, 0)
        cell.set_property('pixbuf', pb)
        return


    def file_name(self, column, cell, model, iter):
        cell.set_property('text', model.get_value(iter, 0))
        return

    def file_size(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        cell.set_property('text', filestat.st_size)
        return

    def file_mode(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        cell.set_property('text', oct(stat.S_IMODE(filestat.st_mode)))
        return


    def file_last_changed(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        cell.set_property('text', time.ctime(filestat.st_mtime))
        return

    def getView():
        return self.treeview


class Engine(object):

    PLAY_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PLAY, gtk.ICON_SIZE_BUTTON)
    PAUSE_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PAUSE, gtk.ICON_SIZE_BUTTON)

    def __init__(self, filename = 'file:////home/px/scripts/beatnitpycker/preview.mp3'):

        self.hbox = gtk.HBox()

        self.play_button = gtk.Button()
        self.slider = gtk.HScale()

        self.play_button.set_name("yea")
        # button_name = gtk.Object.get_name(self.play_button)
        self.mname = self.play_button.get_name()
        print self.mname

        self.hbox.pack_start(self.play_button, False)
        self.hbox.pack_start(self.slider, True, True)
        self.play_button.set_image(self.PLAY_IMAGE)
        self.play_button.connect('clicked', self.on_playx)
        # self.play_button.connect('clicked', self.load_file)

        self.slider.set_range(0, 100)
        self.slider.set_increments(1, 10)
        self.slider.connect('value-changed', self.on_slider_change)

        self.playbin = gst.element_factory_make('playbin')
        self.playbin.set_property('uri', filename)

        self.bus = self.playbin.get_bus()
        self.bus.add_signal_watch()

        self.bus.connect("message::eos", self.on_finish)

        self.is_playing = False


        # gobject.timeout_add(100, self.plop)

        table = gtk.Table(2, 3, True)
        self.label = gtk.Label()
        button_connect = gtk.Button("Connect")
        button_disconnect = gtk.Button("Disconnect")
        button_disconnect.set_sensitive(False)
        self.button_status = gtk.Button("Status")

        button_connect.connect("clicked", self.signal_connected, button_disconnect)
        button_disconnect.connect("clicked", self.signal_disconnected, button_connect)

        self.hbox.pack_start(table, False)
        table.attach(self.label, 0, 3, 0, 1)
        table.attach(button_connect, 0, 1, 1, 2)
        table.attach(button_disconnect, 1, 2, 1, 2)
        table.attach(self.button_status, 2, 3, 1, 2)


    def signal_connected(self, button_connect, button_disconnect):
        self.handler_id = self.button_status.connect("clicked", self.signal_status)
        self.label.set_text("Status Button connected with id of %s" % str(self.handler_id))
        button_connect.set_sensitive(False)
        button_disconnect.set_sensitive(True)

    def signal_disconnected(self, button_disconnect, button_connect):
        if self.button_status.handler_is_connected(self.handler_id):
            self.button_status.disconnect(self.handler_id)
            self.label.set_text("Status Button disconnected")
            button_connect.set_sensitive(True)
            button_disconnect.set_sensitive(False)

    def signal_status(self, button_status):
        print "Status Button connected with id of %s" % str(self.handler_id)

    def signal_status(self, button_status):
        print "Status Button connected with id of %s" % str(self.handler_id)

    def load_file(self, filename):
        self.handler_id = self.play_button.connect("clicked", self.signal_status)
        print "ID :", self.handler_id
        print type(self)
        # print vars(self)
        print filename
        # self.mname.set_image(self.PAUSE_IMAGE)
        if not self.is_playing:
            self.playbin.set_state(gst.STATE_NULL)
            self.play_button.set_image(self.PAUSE_IMAGE)
            self.is_playing = True
            self.playbin.set_property('uri', 'file:///' + filename)

            self.playbin.set_state(gst.STATE_PLAYING)
            gobject.timeout_add(100, self.update_slider)

        else:
            print "not playing"
            # self.play_button.set_image(self.PLAY_IMAGE)
            self.is_playing = False

            self.playbin.set_state(gst.STATE_PAUSED)

    def signal_status(self, button_status):
        print "Status Button connected with id of %s" % str(self.handler_id)

    def on_playx(self, widget):
        # print vars(self)
        self.handler_id = self.play_button.connect("clicked", self.signal_status)
        print "ID :", self.handler_id
        print type(self)
        # for pspec in widget.props:
            # print pspec
            # print widget.get_property(pspec.name)
        name = gtk.Buildable.get_name(widget)
        print "on_playx"
        if not self.is_playing:
            print "playing"
            # self.play_button.set_image(self.PAUSE_IMAGE)
            self.is_playing = True

            self.playbin.set_state(gst.STATE_PLAYING)
            gobject.timeout_add(100, self.update_slider)

        else:
            print "ID :", self.handler_id
            print "not playing"
            # self.play_button.set_image(self.PLAY_IMAGE)
            self.is_playing = False

            self.playbin.set_state(gst.STATE_NULL)

    def on_finish(self, bus, message):
        print "finished"
        self.playbin.set_state(gst.STATE_NULL)
        self.play_button.set_image(self.PLAY_IMAGE)
        self.is_playing = False
        self.playbin.seek_simple(gst.FORMAT_TIME, gst.SEEK_FLAG_FLUSH, 0)
        self.slider.set_value(0)

    def on_play(self, button):
        if not self.is_playing:
            self.play_button.set_image(self.PAUSE_IMAGE)
            self.is_playing = True

            self.playbin.set_state(gst.STATE_PLAYING)
            gobject.timeout_add(100, self.update_slider)

        else:
            self.play_button.set_image(self.PLAY_IMAGE)
            self.is_playing = False

            self.playbin.set_state(gst.STATE_PAUSED)

    def on_slider_change(self, slider):
        print "zz"
        seek_time_secs = slider.get_value()
        self.playbin.seek_simple(gst.FORMAT_TIME, gst.SEEK_FLAG_FLUSH | gst.SEEK_FLAG_KEY_UNIT, seek_time_secs * gst.SECOND)

    def update_slider(self):
        print "called"
        if not self.is_playing:
            return False # cancel timeout
            print "nope"
        print "yep"

        try:
            nanosecs, format = self.playbin.query_position(gst.FORMAT_TIME)
            duration_nanosecs, format = self.playbin.query_duration(gst.FORMAT_TIME)

            # block seek handler so we don't seek when we set_value()
            self.slider.handler_block_by_func(self.on_slider_change)

            self.slider.set_range(0, float(duration_nanosecs) / gst.SECOND)
            self.slider.set_value(float(nanosecs) / gst.SECOND)

            self.slider.handler_unblock_by_func(self.on_slider_change)

            print "plop", nanosecs, duration_nanosecs

        except gst.QueryError:
            # pipeline must not be ready and does not know position
         pass

        return True # continue calling every 30 milliseconds


class GUI(object):

    PLAY_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PLAY, gtk.ICON_SIZE_BUTTON)
    PAUSE_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PAUSE, gtk.ICON_SIZE_BUTTON)

    def delete_event(self, widget, event, data=None):
        Engine().playbin.set_state(gst.STATE_NULL)
        Engine().is_playing = False
        gtk.main_quit()
        return False

    def about_box(self, widget):
        about = gtk.AboutDialog()
        about.set_program_name("BeatNitPycker")
        about.set_version("0.1")
        about.set_copyright("(c) Philippe \"xaccrocheur\" Coatmeur")
        about.set_comments("Simple sound sample auditor")
        about.set_website("https://github.com/xaccrocheur")
        about.set_logo(gtk.icon_theme_get_default().load_icon("gstreamer-properties", 128, 0))

        about.set_license("BeatNitPycker is free software; you can redistribute it and/or modify "
                                  "it under the terms of the GNU General Public License as published by "
                                  "the Free Software Foundation, version 2.\n\n"
                                  "This program is distributed in the hope that it will be useful, "
                                  "GNU General Public License for more details.\n\n"
                                  "You should have received a copy of the GNU General Public License "
                                  "along with this program; if not, write to the Free Software "
                                  "Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA")
        about.set_wrap_license(True);
        about.run()
        about.destroy()

    def __init__(self):
        lister = Lister()
        player = Engine()
        self.window = gtk.Window()
        self.window.set_size_request(300, 600)
        self.window.connect("delete_event", self.delete_event)
        self.window.set_icon(gtk.icon_theme_get_default().load_icon("gstreamer-properties", 128, 0))

        uimanager = gtk.UIManager()
        accelgroup = uimanager.get_accel_group()
        self.window.add_accel_group(accelgroup)

        self.actiongroup = gtk.ActionGroup("uimanager")

        self.actiongroup.add_actions([
            ("New", gtk.STOCK_NEW, "_New", None, "Create a New Document"),
            ("Open", gtk.STOCK_OPEN, "_Open", None, "Open an Existing Document"),
            ("Save", gtk.STOCK_SAVE, "_Save", None, "Save the Current Document"),
            ("Quit", gtk.STOCK_QUIT, "_Quit", None, "Quit the Application", lambda w: gtk.main_quit()),
            ("File", None, "_File"),
            ("Preferences", gtk.STOCK_PREFERENCES, "_Preferences", None, "Edit the Preferences"),
            ("Edit", None, "_Edit"),
            ("About", gtk.STOCK_ABOUT, "_About", None, "yow", self.about_box),
            ("Help", None, "_Help")
        ])

        uimanager.insert_action_group(self.actiongroup, 0)
        uimanager.add_ui_from_string(interface)

        menubar = uimanager.get_widget("/MenuBar")

        vbox = gtk.VBox()

        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.add(lister.treeview)
        vbox.pack_start(menubar, False)
        vbox.pack_start(Engine().hbox, False, False, 1)
        vbox.pack_start(self.scrolledwindow)

        self.window.add(vbox)
        self.window.show_all()
        return

def main():
    gtk.main()


if __name__ == "__main__":
    GUI()
    main()
