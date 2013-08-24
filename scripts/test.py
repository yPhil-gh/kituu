#!/usr/bin/python


# ensure that PyGTK 2.0 is loaded - not an older version
import pygtk
pygtk.require('2.0')
# import the GTK module
import gtk
import gobject

class MyGUI:

  def __init__( self, title):
    self.window = gtk.Window()
    self.title = title
    self.window.set_title( title)
    self.window.set_size_request( -1, -1)
    self.window.connect( "destroy", self.destroy)
    self.create_interior()
    self.window.show_all()

  def create_interior( self):
    self.mainbox = gtk.ScrolledWindow()
    self.mainbox.set_policy( gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    self.window.add( self.mainbox)
    # model creation
    # text, color, editable
    self.model = gtk.ListStore( str, str, bool)
    self.model.append( ["Editable and red","#FF0000",True])
    self.model.append( ["Noneditable","#FFFFFF",False])
    self.model.append( ["Noneditable and green","#00FF00",False])
    # the treeview
    treeview = gtk.TreeView( self.model) #@+
    # individual columns
    # Text column
    col = gtk.TreeViewColumn( "Text")
    treeview.append_column( col)
    cell = gtk.CellRendererText()
    col.pack_start( cell, expand=False)
    col.set_attributes( cell, text=0, cell_background=1, editable=2) #@+
    cell.connect('edited', self._text_changed, 0)
    col.set_sort_column_id( 0)
    # Editable column
    col = gtk.TreeViewColumn( "Editable")
    treeview.append_column( col)
    cell = gtk.CellRendererToggle()
    cell.set_property( "activatable", True)
    col.pack_start( cell, expand=False)
    col.set_attributes( cell, active=2, cell_background=1)
    col.set_sort_column_id( 2)
    cell.connect('toggled', self._editable_toggled, 2)
    # pack the mainbox
    self.mainbox.add( treeview)
    # show the box
    self.mainbox.set_size_request( 260, 200)
    self.mainbox.show()

  def _text_changed( self, w, row, new_value, column):
    self.model[row][column] = new_value

  def _editable_toggled( self, w, row, column):
    self.model[row][column] = not self.model[row][column]

  def main( self):
    gtk.main()

  def destroy( self, w):
    gtk.main_quit()


if __name__ == "__main__":
  m = MyGUI( "TreeView example III.")
  m.main()
