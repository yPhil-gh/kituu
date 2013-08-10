#!/usr/bin/env python

# ensure that PyGTK 2.0 is loaded - not an older version
import pygtk
pygtk.require('2.0')
# import the GTK module
import gtk

class MyGUI:

  def __init__( self, title, filename): #@+
    self.window = gtk.Window()
    self.title = title
    self.filename = filename
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
    model = gtk.ListStore( str, str, int, str, float) #@+
    f = file( self.filename, "r")
    header = f.readline().strip().split("|") # header in first line
    for line in f:
      data = line.strip().split("|")
      data[2] = int( data[2])
      data[4] = float( data[4])
      model.append( data)
    f.close()
    # the treeview
    treeview = gtk.TreeView( model) #@+
    # cell renderers - for individual data cells rendering
    cells = [gtk.CellRendererText() for i in range( len( header))] #@+
    # columns
    cols = [gtk.TreeViewColumn( title) for title in header] #@+
    for i,col in enumerate( cols):
      treeview.append_column( col) #@+
      col.pack_start( cells[i], expand=False) #@+
      col.set_attributes( cells[i], text=i) #@+
      col.set_sort_column_id( i) #@+
    self.mainbox.add( treeview)
    treeview.show()
    treeview.connect( "row-activated", self.row_activated) #@+
    # show the box
    self.mainbox.set_size_request( 460, 400)
    self.mainbox.show()

  def row_activated( self, tree, path, column):
    model = tree.get_model() #@+
    iter = model.get_iter( path) #@+
    country = model.get_value( iter, 0) #@+
    d = gtk.MessageDialog( parent=self.window,
                           type=gtk.MESSAGE_INFO,
                           flags=gtk.DIALOG_DESTROY_WITH_PARENT,
                           buttons=gtk.BUTTONS_CLOSE,
                           message_format="You selected currency of %s." % country
                           )
    result = d.run()
    d.destroy()

  def main( self):
    gtk.main()

  def destroy( self, w):
    gtk.main_quit()


if __name__ == "__main__":
  m = MyGUI( "TreeView example", "exchange.txt") #@+
  m.main()
