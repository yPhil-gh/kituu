#!/usr/bin/env python

import os, stat, time
import pygtk
import gtk
import pygame.mixer

import random

pygame.init()

folderxpm = [
    "17 16 7 1",
    "  c #000000",
    ". c #808000",
    "X c yellow",
    "o c #808080",
    "O c #c0c0c0",
    "+ c white",
    "@ c None",
    "@@@@@@@@@@@@@@@@@",
    "@@@@@@@@@@@@@@@@@",
    "@@+XXXX.@@@@@@@@@",
    "@+OOOOOO.@@@@@@@@",
    "@+OXOXOXOXOXOXO. ",
    "@+XOXOXOXOXOXOX. ",
    "@+OXOXOXOXOXOXO. ",
    "@+XOXOXOXOXOXOX. ",
    "@+OXOXOXOXOXOXO. ",
    "@+XOXOXOXOXOXOX. ",
    "@+OXOXOXOXOXOXO. ",
    "@+XOXOXOXOXOXOX. ",
    "@+OOOOOOOOOOOOO. ",
    "@                ",
    "@@@@@@@@@@@@@@@@@",
    "@@@@@@@@@@@@@@@@@"
    ]
# folderpb = gtk.gdk.pixbuf_new_from_xpm_data(folderxpm)
folderpb = gtk.gdk.pixbuf_new_from_file("../tmp/Beatnitpicker/folder.png")

sndfilepb = gtk.gdk.pixbuf_new_from_file("../tmp/Beatnitpicker/audiofile.png")

filepb = gtk.gdk.pixbuf_new_from_file("../tmp/Beatnitpicker/genericfile.png")

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

def get_resource_path(rel_path):
    dir_of_py_file = os.path.dirname(__file__)
    rel_path_to_resource = os.path.join(dir_of_py_file, rel_path)
    abs_path_to_resource = os.path.abspath(rel_path_to_resource)
    return abs_path_to_resource

class Nitpick:
    column_names = ['Name', 'Size', 'Mode', 'Last Changed']

    def about_box(self, widget):
        about = gtk.AboutDialog()
        about.set_program_name("BeatNitPicker")
        about.set_version("0.1")
        about.set_copyright("(c) Philippe \"xaccrocheur\" Coatmeur")
        about.set_comments("Simple sound sample auditor")
        about.set_website("http://www.zetcode.com")
        about.set_logo(gtk.gdk.pixbuf_new_from_file("../tmp/Beatnitpicker/icon.svg"))
        about.run()
        about.destroy()

    def stop_audio(self, plop):
        pygame.mixer.stop()
        print "Audio stopped"


    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def __init__(self, dname = None):
        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)

        self.window = gtk.Window()
        self.window.set_size_request(400, 600)
        self.window.connect("delete_event", self.delete_event)

        self.window.set_icon_from_file(get_resource_path("../tmp/Beatnitpicker/icon.svg"))

        vbox = gtk.VBox()

        uimanager = gtk.UIManager()
        accelgroup = uimanager.get_accel_group()
        self.window.add_accel_group(accelgroup)

        listmodel = self.make_list(dname)
        liststore = gtk.ListStore(str, int, int, str)

        # create the TreeView
        self.treeview = gtk.TreeView(liststore)

        # create the TreeViewColumns to display the data
        self.tvcolumn = [None] * len(self.column_names)

        # set_sort_column_id(0)

        self.bouton = gtk.Button('Stop sound')
        self.bouton.connect('clicked', self.stop_audio)
        vbox.pack_start(self.bouton, False)

        cellpb = gtk.CellRendererPixbuf()
        self.tvcolumn[0] = gtk.TreeViewColumn(self.column_names[0], cellpb)
        self.tvcolumn[0].set_cell_data_func(cellpb, self.file_pixbuf)
        cell = gtk.CellRendererText()
        self.tvcolumn[0].pack_start(cell, False)
        self.tvcolumn[0].set_cell_data_func(cell, self.file_name)
        self.tvcolumn[0].set_sort_column_id(0)

# Gtk.SortType.ASCENDING or Gtk.SortType.DESCENDING.

        self.treeview.append_column(self.tvcolumn[0])
        for n in range(1, len(self.column_names)):
            cell = gtk.CellRendererText()
            self.tvcolumn[n] = gtk.TreeViewColumn(self.column_names[n], cell)

            # win.tv.cell[i] = gtk.CellRendererText()
            # win.tv.append_column(win.tv.column[i])
            # win.tv.column[i].set_sort_column_id(i)
            # win.tv.column[i].pack_start(win.tv.cell[i], True)
            # win.tv.column[i].set_attributes(win.tv.cell[i], text=i)

            if n == 1:
                cell.set_property('xalign', 1.0)
            self.tvcolumn[n].set_cell_data_func(cell, cell_data_funcs[n])

            self.treeview.append_column(self.tvcolumn[n])

            # treestore = gtk.TreeStore(str, object)
            # treestore.connect("rows-reordered", self.rows_r)

        self.treeview.connect('row-activated', self.open_file)
        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.add(self.treeview)
        self.treeview.set_model(listmodel)

        vbox.pack_start(self.scrolledwindow)


# Menu

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

    def stop_wav(cls, channel):
        """Stops the playback of sound on the specified channel. -1 can be used to
          stop all sounds"""
        if channel==-1:
            pygame.mixer.stop()
        else:
            cls.channels[channel].stop()

    def make_list(self, dname=None):
        if not dname:
            self.dirname = os.path.expanduser('~')
        else:
            self.dirname = os.path.abspath(dname)
        self.window.set_title("Nitpicker : " + self.dirname)
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
            pygame.mixer.Sound(filename).play()
        return

    def file_pixbuf(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            pb = folderpb
        elif filename.endswith('.wav'):
            pb = sndfilepb
        else:
            pb = filepb
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
