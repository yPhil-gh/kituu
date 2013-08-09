#!/usr/bin/env python

import os, stat, time
import gtk

class Lister(object):
    column_names = ['Name']

    def __init__(self, dname = None):

        listmodel = self.make_list(dname)

        # create the TreeView
        self.treeview = gtk.TreeView()

        # create the TreeViewColumns to display the data
        self.tvcolumn = [None] * len(self.column_names)
        cellpb = gtk.CellRendererPixbuf()
        self.tvcolumn[0] = gtk.TreeViewColumn(self.column_names[0], cellpb)
        cell = gtk.CellRendererText()
        self.tvcolumn[0].pack_start(cell, False)
        self.tvcolumn[0].set_cell_data_func(cell, self.file_name)
        self.treeview.append_column(self.tvcolumn[0])
        self.treeview.connect('row-activated', self.open_file)
        self.treeview.set_model(listmodel)
        return

    def make_list(self, dname=None):
        if not dname:
            self.dirname = os.path.expanduser('~')
        else:
            self.dirname = os.path.abspath(dname)
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
            Buttons().this_one_cannot(filename)
        return

    def file_name(self, column, cell, model, iter):
        cell.set_property('text', model.get_value(iter, 0))
        return


class Buttons(object):

    OPEN_IMAGE = gtk.image_new_from_stock(gtk.STOCK_DND_MULTIPLE, gtk.ICON_SIZE_BUTTON)
    CLOSED_IMAGE = gtk.image_new_from_stock(gtk.STOCK_DND, gtk.ICON_SIZE_BUTTON)


    def __init__(self):

        filename = "plop"

        self.button = gtk.Button()

        self.hbox = gtk.HBox()
        self.hbox.pack_start(self.button, False)

        self.button.set_image(self.OPEN_IMAGE)

        self.button.connect('clicked', self.this_method_can)

    def this_method_can(self, button):
        self.button.set_image(self.CLOSED_IMAGE)

    def this_one_cannot(self, filename):
        self.button.set_image(self.CLOSED_IMAGE)


class GUI(object):

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def __init__(self):
        lister = Lister()
        self.window = gtk.Window()
        self.window.set_size_request(300, 600)
        self.window.connect("delete_event", self.delete_event)

        vbox = gtk.VBox()

        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.add(lister.treeview)
        vbox.pack_start(Buttons().hbox, False, False, 1)
        vbox.pack_start(self.scrolledwindow)

        self.window.add(vbox)
        self.window.show_all()
        return

def main():
    gtk.main()

if __name__ == "__main__":
    GUI()
    main()
