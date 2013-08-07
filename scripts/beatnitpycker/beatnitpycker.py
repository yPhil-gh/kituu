#!/usr/bin/env python

import os, stat, time
import pprint

import pygtk, gtk

import pygame.mixer

from matplotlib.figure import Figure
# import matplotlib.pyplot as pl

from numpy import arange, sin, pi
import numpy as np

import scipy.io.wavfile as wavfile

from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas
pygame.init()

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

class Nitpick:
    column_names = ['Name', 'Size', 'Mode', 'Last Changed']

    def about_box(self, widget):
        about = gtk.AboutDialog()
        about.set_program_name("BeatNitPycker")
        about.set_version("0.1")
        about.set_copyright("(c) Philippe \"xaccrocheur\" Coatmeur")
        about.set_comments("Simple sound sample auditor")
        about.set_website("https://github.com/xaccrocheur")
        about.set_logo(gtk.icon_theme_get_default().load_icon("gstreamer-properties", 128, 0))
        about.run()
        about.destroy()

    def stop_audio(self, plop):
        pygame.mixer.stop()
        print "Audio stopped"

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def mytest(self, audiofile):
        print "yow " + audiofile
        self.__init__.myothertest()

    def __init__(self, dname = None):
        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)

        self.window = gtk.Window()
        self.window.set_size_request(300, 600)
        self.window.connect("delete_event", self.delete_event)
        self.window.set_icon(gtk.icon_theme_get_default().load_icon("gstreamer-properties", 128, 0))

        self.image = gtk.Image()

        vbox = gtk.VBox()
        hbox = gtk.VBox(True)

        def myothertest():
            print "wow ! in !"

        def drawplot (audiofile):
            rate, data = wavfile.read(audiofile)
            # plot
            f = Figure(figsize=(5,4), dpi=100)
            self.drawing_area = FigureCanvas(f)
            self.drawing_area.set_size_request(300, 150)
            a = f.add_subplot(111)
            t = np.arange(len(data[:,0]))*1.0/rate
            a.plot(t, data[:,0])
            f.savefig("/home/px/tmp/f.png")
            # plotimage = pl.savefig("/home/px/tmp/f.png", dpi=None, facecolor='w', edgecolor='w',
                    # orientation='portrait', papertype=None, format=None,
                    # transparent=False, bbox_inches=None, pad_inches=0.1,
                    # frameon=None)

            return self.drawing_area


        # myfilename = "/home/px/scripts/beatnitpycker/gare_du_nord-catchlak.wav"

        # self.myplot = drawplot(myfilename)

        # self.myplot.draw()

        # vbox.pack_start (self.myplot, False, False, 1)
        # vbox.pack_start (self.drawing_area, False, False, 1)
        # self.drawing_area = FigureCanvas(f)
        # self.drawing_area.set_size_request(300, 150)

        vbox.pack_start (hbox, False, False, 1)
        hbox.pack_start (self.image, True, True, 0)

        obutton = gtk.Button ("Open a picture...")
        vbox.pack_start (obutton, False, False, 0)

        # open image
        obutton.connect_after('clicked', self.on_open_clicked)

        uimanager = gtk.UIManager()
        accelgroup = uimanager.get_accel_group()
        self.window.add_accel_group(accelgroup)

        listmodel = self.make_list(dname)
        liststore = gtk.ListStore(str, int, int, str)

        # create the TreeView
        self.treeview = gtk.TreeView(liststore)

        # create the TreeViewColumns to display the data
        self.tvcolumn = [None] * len(self.column_names)

        self.bouton = gtk.ToolButton(gtk.STOCK_MEDIA_STOP)
        self.bouton.connect('clicked', self.stop_audio)
        vbox.pack_end(self.bouton, False)

        cellpb = gtk.CellRendererPixbuf()
        self.tvcolumn[0] = gtk.TreeViewColumn(self.column_names[0], cellpb)
        self.tvcolumn[0].set_cell_data_func(cellpb, self.file_pixbuf)
        cell = gtk.CellRendererText()
        self.tvcolumn[0].pack_start(cell, False)
        self.tvcolumn[0].set_cell_data_func(cell, self.file_name)
        self.tvcolumn[0].set_sort_column_id(0)

        self.treeview.append_column(self.tvcolumn[0])
        for n in range(1, len(self.column_names)):
            cell = gtk.CellRendererText()
            self.tvcolumn[n] = gtk.TreeViewColumn(self.column_names[n], cell)

            if n == 1:
                cell.set_property('xalign', 1.0)
            self.tvcolumn[n].set_cell_data_func(cell, cell_data_funcs[n])

            self.treeview.append_column(self.tvcolumn[n])

        self.treeview.connect('row-activated', self.open_file)
        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.add(self.treeview)
        self.treeview.set_model(listmodel)

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
        vbox.pack_start(self.scrolledwindow)

        self.window.add(vbox)
        self.window.show_all()
        return

    def on_open_clicked (self, button):
        self.image.set_from_file("/usr/lib/lv2/paramEQ-Rafols.lv2/combopix/peak.png")
        self.scrolledwindow.show_all()
        self.myplot.draw()

        # pp = pprint.PrettyPrinter(indent=4)
        # pp.pprint(plop)

    def make_list(self, dname=None):
        if not dname:
            self.dirname = os.path.expanduser('/home/px/scripts/beatnitpycker/')
        else:
            self.dirname = os.path.abspath(dname)
        self.window.set_title("BeatNitpycker : " + self.dirname)
        files = [f for f in os.listdir(self.dirname) if f[0] <> '.']
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
        # print filename
        if stat.S_ISDIR(filestat.st_mode):
            new_model = self.make_list(filename)
            treeview.set_model(new_model)
        else:
            print "playing " + filename
            pygame.mixer.Sound(filename).play()
            rate, data = wavfile.read(open(filename, 'r'))
            # plot
            f = Figure(figsize=(3,2), dpi=100)
            self.drawing_area = FigureCanvas(f)
            self.drawing_area.set_size_request(300, 150)
            a = f.add_subplot(111)
            t = np.arange(len(data[:,0]))*1.0/rate
            a.plot(t, data[:,0])
            f.savefig("/home/px/tmp/f.png")
            self.image.set_from_file("/home/px/tmp/f.png")
        return

    def file_pixbuf(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            pb = gtk.icon_theme_get_default().load_icon("folder", 24, 0)
        elif filename.endswith('.wav'):
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

def main():
    gtk.main()

if __name__ == "__main__":
    flcdexample = Nitpick()
    main()
