#!/usr/bin/env python

import subprocess

# subprocess.call(["play", "~/Studio/Samples/DRUMS/Vintage_Drum_Samples_24bit/DR55/dr55_rim.wav"])

import os, stat, time
import pygtk
pygtk.require('2.0')
import gtk

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
folderpb = gtk.gdk.pixbuf_new_from_xpm_data(folderxpm)

filexpm = [
    "12 12 3 1",
    "  c #000000",
    ". c #ffff04",
    "X c #b2c0dc",
    "X        XXX",
    "X ...... XXX",
    "X ......   X",
    "X .    ... X",
    "X ........ X",
    "X .   .... X",
    "X ........ X",
    "X .     .. X",
    "X ........ X",
    "X .     .. X",
    "X ........ X",
    "X          X"
    ]
filepb = gtk.gdk.pixbuf_new_from_xpm_data(filexpm)

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

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def __init__(self, dname = None):
        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)

        # Create a new window

        # window = self.window

        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_size_request(400, 600)
        self.window.connect("delete_event", self.delete_event)


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
        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.add(self.treeview)
        self.window.add(self.scrolledwindow)
        self.treeview.set_model(listmodel)


# Menu

        # vbox = gtk.VBox()

        # uimanager = gtk.UIManager()
        # accelgroup = uimanager.get_accel_group()
        # self.window.add_accel_group(accelgroup)

        # self.actiongroup = gtk.ActionGroup("uimanager")
        # self.actiongroup.add_actions([
        #     ("New", gtk.STOCK_NEW, "_New", None, "Create a New Document"),
        #     ("Open", gtk.STOCK_OPEN, "_Open", None, "Open an Existing Document"),
        #     ("Save", gtk.STOCK_SAVE, "_Save", None, "Save the Current Document"),
        #     ("Quit", gtk.STOCK_QUIT, "_Quit", None, "Quit the Application", lambda w: gtk.main_quit()),
        #     ("File", None, "_File"),
        #     ("Preferences", gtk.STOCK_PREFERENCES, "_Preferences", None, "Edit the Preferences"),
        #     ("Edit", "None", "_Edit"),
        #     ("About", gtk.STOCK_ABOUT, "_About", None, "Open the About dialog"),
        #     ("Help", "None", "_Help")
        # ])

        # uimanager.insert_action_group(self.actiongroup, 0)
        # uimanager.add_ui_from_string(interface)

        # menubar = uimanager.get_widget("/MenuBar")
        # vbox.pack_start(menubar, False)



        # # window.connect("destroy", lambda w: gtk.main_quit())
        # self.window.add(vbox)


        self.window.show_all()
        return


        # Funcs

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
            subprocess.call(["play", filename])
            # print filename + "is a file!!"
        return

        def on_selection_changed(selection, f):
            model, paths = selection.get_selected_rows()
            if paths:
                # do the thing!
                print selection + f
                self.treeView = gtk.TreeView(mymodel)
                selection = self.treeView.get_selection()
                selection.connect('changed', on_selection_changed)

    def file_pixbuf(self, column, cell, model, iter):
        filename = os.path.join(self.dirname, model.get_value(iter, 0))
        filestat = os.stat(filename)
        if stat.S_ISDIR(filestat.st_mode):
            pb = folderpb
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
