#!/usr/bin/env python

# # import os
from urlparse import urlparse
# # # o = urlsplit('urn:zamaudio:ZamEQ2')

# # for plugin in os.system("lv2ls"):
# #     urlsplit(plugin)

from subprocess import Popen, PIPE, call
pipe = Popen('lv2ls', shell=True, stdout=PIPE)

for plugin in pipe.stdout:
    # parsed_uri = urlparse(plugin)
    # domain = '{uri.scheme}://{uri.netloc}/'.format(uri=parsed_uri)
    # print domain
    # print parsed_uri.netloc
    print plugin
    # print call(["lv2info", plugin])
    # print(line.strip())
    # print urlsplit(plugin).netloc
    # text = "calf.sourceforge.net"
    # print urlsplit(plugin).netloc
    # print plugin
    # print urlsplit(plugin).netloc.partition('.')[0]
    # if urlsplit(plugin).netloc.partition('.')[0] is "www":
    #     print urlsplit(plugin).netloc.partition('.')[1]
    # else:
    #     print urlsplit(plugin).netloc.partition('.')[0]
