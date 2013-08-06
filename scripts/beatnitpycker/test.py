#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk

class UIMergeExample:
    ui0 = '''<ui>
    <menubar name="MenuBar">
      <menu action="File">
        <menuitem action="Quit"/>
      </menu>
      <menu action="Sound">
        <menuitem action="Mute"/>
      </menu>
      <menu action="RadioBand">
        <menuitem action="AM"/>
        <menuitem action="FM"/>
        <menuitem action="SSB"/>
      </menu>
    </menubar>
    <toolbar name="Toolbar">
      <toolitem action="Quit"/>
      <separator/>
      <toolitem action="Mute"/>
      <separator name="sep1"/>
      <placeholder name="RadioBandItems">
        <toolitem action="AM"/>
        <toolitem action="FM"/>
        <toolitem action="SSB"/>
      </placeholder>
    </toolbar>
    </ui>'''

    def __init__(self):
        # Create the toplevel window
        window = gtk.Window()
        window.connect('destroy', lambda w: gtk.main_quit())
        window.set_size_request(800, -1)
        vbox = gtk.VBox()
        window.add(vbox)

        self.merge_id = 0

        # Create a UIManager instance
        uimanager = gtk.UIManager()
        self.uimanager = uimanager

        # Add the accelerator group to the toplevel window
        accelgroup = uimanager.get_accel_group()
        window.add_accel_group(accelgroup)

        # Create the base ActionGroup
        actiongroup0 = gtk.ActionGroup('UIMergeExampleBase')

        actiongroup0.add_actions([('File', gtk.STOCK_PREFERENCES, '_File'),
                                  ('Sound', gtk.STOCK_PREFERENCES, '_Sound'),
                                  ('RadioBand', gtk.STOCK_PREFERENCES, '_Radio Band')])
        uimanager.insert_action_group(actiongroup0, 0)

        # Create an ActionGroup
        actiongroup = gtk.ActionGroup('UIMergeExampleBase')
        self.actiongroup = actiongroup

        # Create a ToggleAction, etc.
        actiongroup.add_toggle_actions([('Mute', gtk.STOCK_PREFERENCES, '_Mute', '<Control>m',
                                         'Mute the volume', self.mute_cb)])

        # Create actions
        actiongroup.add_actions([('Quit', gtk.STOCK_QUIT, '_Quit me!', None,
                                  'Quit the Program', self.quit_cb)])
        actiongroup.get_action('Quit').set_property('short-label', '_Quit')

        # Create some RadioActions
        actiongroup.add_radio_actions([('AM', gtk.STOCK_PREFERENCES, '_AM', '<Control>a',
                                        'AM Radio', 0),
                                       ('FM', gtk.STOCK_PREFERENCES, '_FM', '<Control>f',
                                        'FM Radio', 1),
                                       ('SSB', gtk.STOCK_PREFERENCES, '_SSB', '<Control>b',
                                        'SSB Radio', 2),
                                       ], 0, self.radioband_cb)

        # Add the actiongroup to the uimanager
        uimanager.insert_action_group(actiongroup, 1)

        # Add a UI description
        merge_id = uimanager.add_ui_from_string(self.ui0)

        # Create a MenuBar
        menubar = uimanager.get_widget('/MenuBar')
        vbox.pack_start(menubar, False)

        # Create a Toolbar
        toolbar = uimanager.get_widget('/Toolbar')
        vbox.pack_start(toolbar, False)

        # Create buttons to control visibility and sensitivity of actions
        buttonbox = gtk.HButtonBox()
        visiblebutton = gtk.CheckButton('Visible')
        visiblebutton.set_active(True)
        visiblebutton.connect('toggled', self.toggle_visibility)
        buttonbox.pack_start(visiblebutton, False)
        vbox.pack_start(buttonbox)
        print uimanager.get_ui()
        window.show_all()
        return

    def mute_cb(self, action):
        # action has not toggled yet
        text = ('muted', 'not muted')[action.get_active()==False]
        self.mutelabel.set_text('Sound is %s' % text)
        return

    def loudness_cb(self, action):
        # action has not toggled yet
        print 'Loudness toggled'
        return

    def radioband_cb(self, action, current):
        text = current.get_name()
        self.bandlabel.set_text('Radio band is %s' % text)
        return

    def new_cb(self, b):
        print 'New settings'
        return

    def save_cb(self, b):
        print 'Save settings'
        return

    def quit_cb(self, b):
        print 'Quitting program'
        gtk.main_quit()

    def toggle_visibility(self, b):
        self.actiongroup.set_visible(b.get_active())
        return
if __name__ == '__main__':
    ba = UIMergeExample()
    gtk.main()
