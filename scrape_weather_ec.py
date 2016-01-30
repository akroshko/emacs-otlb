#! /usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (C) 2015-2016 Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko@gmail.com>
# Created: Sat Mar 28, 2015
# Version: 20151201
# URL: https://github.com/akroshko/python-stdlib-personal
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.

import lxml.html
import re
import sys
import urllib
# uncomment for debugging
# from pprint import pprint
# from lxml.etree import tostring

if len(sys.argv) < 3:
    print "sys.argv[1] is the year, month, day (as YYYYMMDD) and/or --current"
    print "sys.argv[2] is the time (as a string HH:MM)"

# round time up and down
# replace hours with zeros
hoursdown=sys.argv[2].strip()[0:2]
hoursup=("%02d" % ((int(hoursdown)+1)%24))
minutes=int(sys.argv[2].strip()[2:4])
# print hoursdown
# print hoursup
# print minutes

def get_from_url_current(url):
    uh = urllib.urlopen(url)
    html = lxml.html.parse(uh)
    # text = html.xpath('//tr/td//text()')
    table = html.xpath('//table')[0]
    hours=[h.strip() for h in table.xpath('//td[@headers=\'header1\']/text()')]
    raw_temps=table.xpath('//td[@headers=\'header3\']')
    temps = []
    for t in raw_temps:
        txs = t.xpath('text()')[0].strip()
        if txs == '':
            temps.append(str(t.xpath('span/text()')[0].strip()))
        else:
            temps.append(str(txs))
    wind=[w.strip() for w in table.xpath('//td[@headers=\'header6\']/text()')]
    index=None
    for i,h in enumerate(hours):
        if int(h.split(':')[0].strip()) == int(hoursdown):
            index=i
    print temps[index] + u'C with wind ' + wind[index] + ' km/h'

def degToCompass(num):
    if num == -1:
        return "N/A"
    else:
        val=int((num/22.5)+.5)
        arr=["N","NNE","NE","ENE","E","ESE", "SE", "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"]
        return arr[(val % 16)]

def get_from_url_past(url):
    # print url
    uh = urllib.urlopen(url)
    html = lxml.html.parse(uh)
    table = html.xpath('//table')[1]
    # hours = table.xpath('//tr/td[1]/text()')
    hours = get_element_texts(table.xpath('//tr/td[1]'))
    hours = [h.strip() for h in hours[2:26]]
    # print hours
    # temps = table.xpath('//tr/td[2]/text()')
    temps = get_element_texts(table.xpath('//tr/td[2]'))
    temps = [t.strip() for t in temps[2:26]]
    # print temps
    # Note that wind indexing starts at 0
    # winddir = table.xpath('//tr/td[5]/text()')
    winddir = get_element_texts(table.xpath('//tr/td[5]'))
    winddir = [d.strip() for d in winddir[0:24]]
    # print winddir
    winddir = [-1 if d.strip() == '' else d for d in winddir]
    winddir = [degToCompass(int(d)*10) for d in winddir]
    wind = get_element_texts(table.xpath('//tr/td[6]'))
    wind = [d.strip() for d in wind[0:24]]
    # print wind
    index=None
    for i,h in enumerate(hours):
        if int(h.split(':')[0].strip()) == int(hoursdown):
            index=i
    print temps[index] + u'C with wind ' + winddir[index] + ' ' + wind[index] + ' km/h'

def get_element_texts(elements):
    new_elements = []
    for e in elements:
        newstring = re.sub('<[^>]*>', '', lxml.html.tostring(e))
        newstring = re.sub('&#160;', '', newstring)
        newstring = re.sub('LegendMM', 'N/A', newstring)
        new_elements.append(newstring)
    return new_elements

if sys.argv[1] == '--current':
    url = 'http://weather.gc.ca/trends_table/pages/yxe_metric_e.html'
    if '--url' in sys.argv:
        print url
    else:
        get_from_url_current(url)
else:
    # determine URL
    YYYY=sys.argv[1][0:4]
    MM=str(int(sys.argv[1][4:6]))
    DD=str(int(sys.argv[1][6:8]))
    # Saskatoon weather
    url='http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=SASK&StationID=50091&Year=%s&Month=%s&Day=%s' % (YYYY,MM,DD)
    # Sundre weather
    # url='http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=&StationID=10105&Year=%s&Month=%s&Day=%s' % (YYYY,MM,DD)
    # Banff weather
    # url='http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=&StationID=27378&Year=%s&Month=%s&Day=%s' % (YYYY,MM,DD)
    # Calgary weather
    # url='http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=&StationID=50430&Year=%s&Month=%s&Day=%s' % (YYYY,MM,DD)
    if '--url' in sys.argv:
        print url
    else:
        get_from_url_past(url)
