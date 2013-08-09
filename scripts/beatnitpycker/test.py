#!/usr/bin/env python

import gtk

class C:
    def method(self):
        pass

c = C()
c.method.im_func.whoami = 'my name is c'
