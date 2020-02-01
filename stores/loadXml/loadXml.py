#!/usr/bin/env python
# -*- coding: utf-8 -*-

import xml.etree.cElementTree as ET
myTree = ET.parse('Stores0000072906390-202001012330.xml')
myroot = myTree.getroot()
print(myroot[0].tag)
print(myroot[0].attrib)

for x in myroot:
	print(x.tag,x.attrib,x.text.encode('utf-8'))