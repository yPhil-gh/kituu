#!/usr/bin/env python

# example treeviewdnd.py

import pygtk
pygtk.require('2.0')
import gtk

class TreeViewDnDExample:

    TARGETS = [
        ('MY_TREE_MODEL_ROW', gtk.TARGET_SAME_WIDGET, 0),
        ('text/plain', 0, 1),
        ('TEXT', 0, 2),
        ('STRING', 0, 3),
        ]
    # close the window and quit
    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    def clear_selected(self, button):
        selection = self.treeview.get_selection()
        model, iter = selection.get_selected()
        if iter:
            model.remove(iter)
        return

    def __init__(self):
        # Create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)

        self.window.set_title("URL Cache")

        self.window.set_size_request(200, 200)

        self.window.connect("delete_event", self.delete_event)

        self.scrolledwindow = gtk.ScrolledWindow()
        self.vbox = gtk.VBox()
        self.hbox = gtk.HButtonBox()
        self.vbox.pack_start(self.scrolledwindow, True)
        self.vbox.pack_start(self.hbox, False)
        self.b0 = gtk.Button('Clear All')
        self.b1 = gtk.Button('Clear Selected')
        self.hbox.pack_start(self.b0)
        self.hbox.pack_start(self.b1)

        # create a liststore with one string column to use as the model
        self.liststore = gtk.ListStore(str)

        # create the TreeView using liststore
        self.treeview = gtk.TreeView(self.liststore)

       # create a CellRenderer to render the data
        self.cell = gtk.CellRendererText()

        # create the TreeViewColumns to display the data
        self.tvcolumn = gtk.TreeViewColumn('URL', self.cell, text=0)

        # add columns to treeview
        self.treeview.append_column(self.tvcolumn)
        self.b0.connect_object('clicked', gtk.ListStore.clear, self.liststore)
        self.b1.connect('clicked', self.clear_selected)
        # make treeview searchable
        self.treeview.set_search_column(0)

        # Allow sorting on the column
        self.tvcolumn.set_sort_column_id(0)

        # Allow enable drag and drop of rows including row move
        self.treeview.enable_model_drag_source( gtk.gdk.BUTTON1_MASK,
                                                self.TARGETS,
                                                gtk.gdk.ACTION_DEFAULT|
                                                gtk.gdk.ACTION_MOVE)
        self.treeview.enable_model_drag_dest(self.TARGETS,
                                             gtk.gdk.ACTION_DEFAULT)

        self.treeview.connect("drag_data_get", self.drag_data_get_data)
        self.treeview.connect("drag_data_received",
                              self.drag_data_received_data)

        self.scrolledwindow.add(self.treeview)
        self.window.add(self.vbox)
        self.window.show_all()

    def drag_data_get_data(self, treeview, context, selection, target_id,
                           etime):
        treeselection = treeview.get_selection()
        model, iter = treeselection.get_selected()
        data = model.get_value(iter, 0)
        selection.set(selection.target, 8, data)

    def drag_data_received_data(self, treeview, context, x, y, selection,
                                info, etime):
        model = treeview.get_model()
        data = selection.data
        drop_info = treeview.get_dest_row_at_pos(x, y)
        if drop_info:
            path, position = drop_info
            iter = model.get_iter(path)
            if (position == gtk.TREE_VIEW_DROP_BEFORE
                or position == gtk.TREE_VIEW_DROP_INTO_OR_BEFORE):
                model.insert_before(iter, [data])
            else:
                model.insert_after(iter, [data])
        else:
            model.append([data])
        if context.action == gtk.gdk.ACTION_MOVE:
            context.finish(True, True, etime)
        return

def main():
    gtk.main()

if __name__ == "__main__":
    treeviewdndex = TreeViewDnDExample()
    main()
