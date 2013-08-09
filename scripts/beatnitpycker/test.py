#!/usr/bin/python

import gtk

class GUI(object):

    OPEN_IMAGE = gtk.image_new_from_stock(gtk.STOCK_ADD, gtk.ICON_SIZE_BUTTON)
    CLOSED_IMAGE = gtk.image_new_from_stock(gtk.STOCK_REFRESH, gtk.ICON_SIZE_BUTTON)
    toggled = True

    def __init__(self):
        self.window = gtk.Window()
        self.window.set_size_request(100, 150)
        self.window.connect("delete_event", gtk.main_quit)

        vbox = gtk.VBox()

        self.button = gtk.Button() # THIS is the button to modify
        self.button.set_image(self.OPEN_IMAGE)

        liststore = gtk.ListStore(str)
        liststore.append(["foo"])
        liststore.append(["bar"])
        self.treeview = gtk.TreeView(liststore)
        cell = gtk.CellRendererText()
        col = gtk.TreeViewColumn("Column 1")
        col.pack_start(cell, True)
        col.set_attributes(cell,text=0)
        self.treeview.append_column(col)

        vbox.pack_start(self.button, False, False, 1)
        vbox.pack_start(self.treeview, False, False, 1)

        self.treeview.connect('row-activated', self.the_method_wrapper, "plop")
        self.button.connect('clicked', self.the_method, "plop")

        self.window.add(vbox)
        self.window.show_all()
        return

    def the_method_wrapper(self, button, *args):
        self.the_method(self, "foo")

    def the_method(self, button, filename):
        print filename
        print vars(self)

        if self.toggled:
            self.button.set_image(self.CLOSED_IMAGE)
            self.toggled = False
        else:
            self.button.set_image(self.OPEN_IMAGE)
            self.toggled = True


def main():
    gtk.main()

if __name__ == "__main__":
    GUI()
    main()
