#!/usr/bin/env python

import os, stat, time
import pygtk
import gtk
import subprocess
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
folderpb = gtk.gdk.pixbuf_new_from_xpm_data(folderxpm)

sndfilexpm = [
"16 16 6 1",
" 	c None",
".	c #242021",
"+	c #262223",
"@	c #231F20",
"#	c #252122",
"$	c #242121",
"                ",
"        .       ",
"       .+       ",
"      .+@  .    ",
"     #+@@  @@+  ",
".+@@#+@@+    $@ ",
"+.@#+@@+#     .+",
"@@#+@@+#@     +@",
"@##@@+#@.     @@",
"##@@+.@.+     @.",
"+@@+.@@+@    @# ",
"     .+.@  @@#  ",
"      @@#  @    ",
"       #+       ",
"        @       ",
"        @       "]

sndfilepb = gtk.gdk.pixbuf_new_from_xpm_data(sndfilexpm)

filexpm = [
"16 16 72 1",
" 	c None",
".	c #787A75",
"+	c #797B76",
"@	c #FFFFFF",
"#	c #7D7F79",
"$	c #F8F8F7",
"%	c #F7F7F6",
"&	c #7D7F7A",
"*	c #DDDDDD",
"=	c #C3C3C2",
"-	c #C2C2C2",
";	c #DBDBDA",
">	c #FBFBFA",
",	c #FEFEFD",
"'	c #FCFCFB",
")	c #FAFAF9",
"!	c #F5F5F5",
"~	c #C1C1C1",
"{	c #C1C1C0",
"]	c #D9D9D8",
"^	c #F0F0F0",
"/	c #FEFEFE",
"(	c #F2F2F2",
"_	c #EEEEEE",
":	c #FDFDFD",
"<	c #C0C0C0",
"[	c #BFBFBF",
"}	c #D7D7D7",
"|	c #EEEEED",
"1	c #ECECEC",
"2	c #FCFCFC",
"3	c #F9F9F8",
"4	c #F9F9F9",
"5	c #F7F7F5",
"6	c #F3F3F3",
"7	c #F0F0EF",
"8	c #EBEBEB",
"9	c #EAEAEA",
"0	c #FBFBFB",
"a	c #BFBFBE",
"b	c #BEBEBE",
"c	c #BEBEBD",
"d	c #BDBDBD",
"e	c #BCBCBC",
"f	c #D2D2D2",
"g	c #FAFAFA",
"h	c #F5F5F4",
"i	c #F6F6F5",
"j	c #F4F4F5",
"k	c #F2F2F1",
"l	c #EFEFEF",
"m	c #E9E9E9",
"n	c #FAF9F9",
"o	c #D8D8D7",
"p	c #E7E7E6",
"q	c #E6E6E5",
"r	c #E5E5E4",
"s	c #F1F1F0",
"t	c #EDEDED",
"u	c #E9E9E8",
"v	c #E6E6E4",
"w	c #E5E5E3",
"x	c #E4E4E2",
"y	c #E7E7E8",
"z	c #E4E4E1",
"A	c #E3E3E0",
"B	c #F8F8F6",
"C	c #72746F",
"D	c #FBFAFB",
"E	c #696A66",
"F	c #666863",
"G	c #686A65",
"  .++++++++++.  ",
" .@@@@@@@@@@@@# ",
" +@$$$$$$$$$%@& ",
" +@*=======-;@& ",
" +@>,,,,,'>)!@& ",
" +@*===--~{]^/& ",
" +@>,,'>)$%(_:& ",
" +@*--~{<[}|12& ",
" +@3>4$5!67890& ",
" +@;{<[abcdefg& ",
" +@hij6k^l_1mn& ",
" +@oabcdefpqr3& ",
" +/sk^lt1uvwx$& ",
" +:19mypqwxzAB& ",
" C@20Dg)433$B@C ",
"  EFFFFFFFFFFG  "]

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



def get_resource_path(rel_path):
    dir_of_py_file = os.path.dirname(__file__)
    rel_path_to_resource = os.path.join(dir_of_py_file, rel_path)
    abs_path_to_resource = os.path.abspath(rel_path_to_resource)
    return abs_path_to_resource


class Nitpick:
    column_names = ['Name', 'Size', 'Mode', 'Last Changed']

    def yowza(plip, plop):
        print "yowza"

    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

    # sortOrder   = gtk.SORT_ASCENDING
    # lastSortCol = None

    # def sortRows(column):
    #     """ Sort the rows based on the given column """
    #     global lastSortCol, sortOrder

    #     if lastSortCol is not None:
    #         lastSortCol.set_sort_indicator(False)

    # # Ascending or descending?
    #     if lastSortCol == column:
    #         if sortOrder == gtk.SORT_ASCENDING:
    #             sortOrder = gtk.SORT_DESCENDING
    #         else:
    #             sortOrder = gtk.SORT_ASCENDING
    #     else:
    #         sortOrder   = gtk.SORT_ASCENDING
    #         lastSortCol = column

    #         # Sort rows
    #         # ...

    #         # Display the sort indicator
    #     column.set_sort_indicator(True)
    #     column.set_sort_order(sortOrder)

    def __init__(self, dname = None):
        cell_data_funcs = (None, self.file_size, self.file_mode,
                           self.file_last_changed)


        self.window = gtk.Window()
        self.window.set_size_request(400, 600)
        self.window.connect("delete_event", self.delete_event)


        # treestore = gtk.TreeStore(str)
        # treestore.connect("rows-reordered", self.rows_reordered)

        self.window.set_icon_from_file(get_resource_path("../tmp/Beatnitpicker/icon.svg"))

        vbox = gtk.VBox()

        uimanager = gtk.UIManager()
        accelgroup = uimanager.get_accel_group()
        self.window.add_accel_group(accelgroup)


        listmodel = self.make_list(dname)
        treestore = gtk.ListStore(str, str, str, str)

        filtered_model = treestore.filter_new()

        self.filtered_model = gtk.TreeModelSort(filtered_model)

        # create the TreeView
        self.treeview = gtk.TreeView(treestore)

        # self.treeview.set_reorderable(True)


        treemodelsort = gtk.TreeModelSort(listmodel)

        treemodelsort.set_sort_column_id(0, gtk.SORT_ASCENDING)

        # create the TreeViewColumns to display the data
        self.tvcolumn = [None] * len(self.column_names)

        # set_sort_column_id(0)

        self.bouton = gtk.Button('Button')
        self.bouton.connect('clicked', self.yowza)
        vbox.pack_start(self.bouton, False)

        cellpb = gtk.CellRendererPixbuf()
        self.tvcolumn[0] = gtk.TreeViewColumn(self.column_names[0], cellpb)
        self.tvcolumn[0].set_cell_data_func(cellpb, self.file_pixbuf)
        cell = gtk.CellRendererText()
        self.tvcolumn[0].pack_start(cell, False)
        self.tvcolumn[0].set_cell_data_func(cell, self.file_name)

        # self.tvcolumn[0].set_sort_column_id(0)


        self.treeview.append_column(self.tvcolumn[0])
        for n in range(1, len(self.column_names)):
            cell = gtk.CellRendererText()
            self.tvcolumn[n] = gtk.TreeViewColumn(self.column_names[n], cell)
            self.tvcolumn[0].set_sort_column_id(0)


            # win.tv.cell[i] = gtk.CellRendererText()
            # win.tv.append_column(win.tv.column[i])
            # win.tv.column[i].set_sort_column_id(i)
            # win.tv.column[i].pack_start(win.tv.cell[i], True)
            # win.tv.column[i].set_attributes(win.tv.cell[i], text=i)


            if n == 1:
                cell.set_property('xalign', 1.0)
                self.tvcolumn[n].set_sort_column_id(0)
            self.tvcolumn[n].set_cell_data_func(cell, cell_data_funcs[n])

            self.tvcolumn[n].set_sort_column_id(n) # make column sortable using column 0 data

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
            ("About", gtk.STOCK_ABOUT, "_About", None, "Open the About dialog"),
            ("Help", None, "_Help")
        ])

        uimanager.insert_action_group(self.actiongroup, 0)
        uimanager.add_ui_from_string(interface)

        menubar = uimanager.get_widget("/MenuBar")
        vbox.pack_start(menubar, False)


        self.window.add(vbox)


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
            pygame.mixer.Sound(filename).play()
            # subprocess.call(["play", filename])
            # print filename + "is a file!!"
        return

        # def on_selection_changed(selection, f):
        #     model, paths = selection.get_selected_rows()
        #     if paths:
        #         # do the thing!
        #         print selection + f
        #         self.treeView = gtk.TreeView(mymodel)
        #         selection = self.treeView.get_selection()
        #         selection.connect('changed', on_selection_changed)

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
