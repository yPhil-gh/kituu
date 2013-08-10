#!/usr/bin/python

# ZetCode PyGTK tutorial
#
# This program shows various signals
# of a button widget
# It emits a button-release-event which
# triggers a released singal
#
# author: jan bodnar
# website: zetcode.com
# last edited: February 2009

from scipy.io.wavfile import read,write
from pylab import plot,show,subplot,specgram

# Open the Homer Simpson voice: "Ummm, Crumbled up cookie things."
# from http://www.thesoundarchive.com/simpsons/homer/mcrumble.wav
rate,data = read('/home/px/mcrumble.wav') # reading

subplot(411, axisbg=(0.1843, 0.3098, 0.3098))
# subplot(411)
plot(range(len(data)),data)
subplot(412)
# NFFT is the number of data points used in each block for the FFT
# and noverlap is the number of points of overlap between blocks
specgram(data, NFFT=128, noverlap=0) # small window
subplot(413)
specgram(data, NFFT=512, noverlap=0)
subplot(414)
specgram(data, NFFT=1024, noverlap=0) # big window

show()
