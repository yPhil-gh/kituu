#!/usr/bin/python

import os, stat, time
import gst, gtk, gobject

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

class GUI(object):

    PLAY_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PLAY, gtk.ICON_SIZE_BUTTON)
    PAUSE_IMAGE = gtk.image_new_from_stock(gtk.STOCK_MEDIA_PAUSE, gtk.ICON_SIZE_BUTTON)

    OPEN_IMAGE = gtk.image_new_from_stock(gtk.STOCK_ADD, gtk.ICON_SIZE_BUTTON)
    CLOSED_IMAGE = gtk.image_new_from_stock(gtk.STOCK_REFRESH, gtk.ICON_SIZE_BUTTON)
    toggled = True

    column_names = ['Name', 'Size', 'Mode', 'Last Changed']

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

    def __init__(self, dname = None):
        self.window = gtk.Window()
        self.window.set_size_request(300, 600)
        self.window.connect("delete_event", gtk.main_quit)

# player

        self.play_button = gtk.Button()
        self.slider = gtk.HScale()

        self.player_hbox = gtk.HBox()
        self.player_hbox.pack_start(self.play_button, False)
        self.player_hbox.pack_start(self.slider, True, True)

        self.play_button.set_image(self.PLAY_IMAGE)
        self.play_button.connect('clicked', self.the_method, "/home/px/scripts/beatnitpycker/preview.mp3")

        self.slider.set_range(0, 100)
        self.slider.set_increments(1, 10)
        self.slider.connect('value-changed', self.on_slider_change)

        self.playbin = gst.element_factory_make('playbin2')
        self.playbin.set_property('uri', 'file:////home/px/scripts/beatnitpycker/preview.mp3')

        self.bus = self.playbin.get_bus()
        self.bus.add_signal_watch()

        self.bus.connect("message::eos", self.on_finish)

        self.is_playing = False

# end player
# lister

        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)
        listmodel = self.make_list(dname)
        self.treeview = gtk.TreeView()
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
        self.treeview.set_model(listmodel)

# end lister

        vbox = gtk.VBox()


        # Plot image

        self.plot_hbox = gtk.HBox()
        self.pimage = gtk.Image()

        self.button = gtk.Button() # THIS is the button to modify
        self.button.set_image(self.OPEN_IMAGE)

        self.plot_hbox.pack_start(self.pimage, True, True, 1)
        vbox.pack_start(self.plot_hbox, False, False, 1)
        vbox.pack_start(self.player_hbox, False, False, 1)
        vbox.pack_start(self.button, False, False, 1)
        vbox.pack_start(self.treeview, False, False, 1)

        self.treeview.connect('row-activated', self.the_other_wrapper, "plop")
        self.button.connect('clicked', self.the_method, "plop")


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
        vbox.pack_start(menubar, False)

        self.window.add(vbox)
        self.window.show_all()
        return

    def the_other_wrapper(self, treeview, path, button, *args):
        audioFormats = [ ".wav", ".mp3", ".ogg", ".flac" ]
        model = treeview.get_model()
        iter = model.get_iter(path)
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            new_model = self.make_list(filename)
            treeview.set_model(new_model)
        elif filename.endswith(tuple(audioFormats)):
            self.the_method(self, filename)
        else:
            print "# Not an audio file"

    def the_method(self, button, filename):
        print filename
        # print vars(self)
        print self.playbin.get_property('uri')
        print self.playbin.get_state()
        print "------------------------"
        if not self.is_playing:
            self.playbin.set_state(gst.STATE_READY)
            print self.playbin.get_state()
            self.playbin.set_property('uri', 'file:///' + filename)
            self.play_button.set_image(self.PAUSE_IMAGE)
            self.is_playing = True
            self.playbin.set_state(gst.STATE_PLAYING)
            gobject.timeout_add(100, self.update_slider)
            if filename.endswith(".wav"):
                # Plotting
                # if filename.endswith(".wav"):
                rate, data = wavfile.read(open(filename, 'r'))
                f = Figure(figsize=(4.5,0.5), linewidth=0.0, edgecolor='b', facecolor='r', dpi=100)
                self.drawing_area = FigureCanvas(f)
                a = f.add_subplot(111)
                a.plot(range(len(data)),data)
                a.axis('off')

                f.savefig("/home/px/tmp/f.png",
                          edgecolor='r',
                          facecolor='w',
                          orientation='portrait',
                          papertype=None,
                          format=None,
                          transparent=False,
                          bbox_inches='tight',
                          pad_inches=0.1,
                          frameon=True
                )
                self.pimage.set_from_file("/home/px/tmp/f.png")

                # self.pbox.pack_start(self.drawing_area)
                print "plotted!"
        else:
            self.play_button.set_image(self.PLAY_IMAGE)
            self.is_playing = False
            self.playbin.set_state(gst.STATE_PAUSED)

# Lister funcs

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

# player funcs

    def on_finish(self, bus, message):
        self.playbin.set_state(gst.STATE_PAUSED)
        self.play_button.set_image(self.PLAY_IMAGE)
        self.is_playing = False
        self.playbin.seek_simple(gst.FORMAT_TIME, gst.SEEK_FLAG_FLUSH, 0)
        self.slider.set_value(0)

    def on_destroy(self, window):
        # NULL state allows the pipeline to release resources
        self.playbin.set_state(gst.STATE_NULL)
        self.is_playing = False
        gtk.main_quit()

    def on_slider_change(self, slider):
        seek_time_secs = slider.get_value()
        self.playbin.seek_simple(gst.FORMAT_TIME, gst.SEEK_FLAG_FLUSH | gst.SEEK_FLAG_KEY_UNIT, seek_time_secs * gst.SECOND)

    def update_slider(self):
        if not self.is_playing:
            return False # cancel timeout

        try:
            nanosecs, format = self.playbin.query_position(gst.FORMAT_TIME)
            duration_nanosecs, format = self.playbin.query_duration(gst.FORMAT_TIME)

            # block seek handler so we don't seek when we set_value()
            self.slider.handler_block_by_func(self.on_slider_change)

            self.slider.set_range(0, float(duration_nanosecs) / gst.SECOND)
            self.slider.set_value(float(nanosecs) / gst.SECOND)

            self.slider.handler_unblock_by_func(self.on_slider_change)

        except gst.QueryError:
            # pipeline must not be ready and does not know position
         pass

        return True # continue calling every 30 milliseconds


def main():
    gtk.main()

if __name__ == "__main__":
    GUI()
    main()
