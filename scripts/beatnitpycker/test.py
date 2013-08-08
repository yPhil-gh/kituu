#!/usr/bin/env python

import os
import gst, gtk, gobject
import random

class One(object):
    print "I'm One"
    # plopOne = "plop"
    def __init__(self):
        self.plopOne = "plop"
        self.plop = random.randrange(0, 1001, 2)
        print "One self.plop is", self.plop, "hmm"
    def OneRandom(*args):
        plopOne = "plop"
        return random.randrange(0, 101, 2)
        fooNested = "plop"
    print ""

class Two(object):
    print "I'm Two"
    plopTwo = "plip"
    one = One()
    two = one.plop
    print "Two is", two
    OneRandom = one.OneRandom()
    print ""
    def test(self, *args):
        print vars(self)

print "Now."
one = One()
two = Two()
print "And now it's", two.two, "but OneRandom is", two.OneRandom, "hum"
print one.plopOne, "aaah I self.see !"
print two.test
