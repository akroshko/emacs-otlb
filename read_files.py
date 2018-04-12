#!/usr/bin/python
#
# Copyright (C) 2015-2018, Andrew Kroshko, all rights reserved.
#
# Author: Andrew Kroshko
# Maintainer: Andrew Kroshko <akroshko.public+devel@gmail.com>
# Created: Sun May 1, 2016
# Version: 20180411
# URL: https://github.com/akroshko/emacs-otlb
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
#
#
#
# Copyright (c) 2012, Gustav Tiger <gustav@tiger.name>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

# TODO: needs documentation
from __future__ import absolute_import, division
import os,sys
import lxml
from lxml import etree
import json
import tempfile
import subprocess
import math
import os
import time
from collections import namedtuple
from pprint import pprint
sys.path.append('/usr/local/lib/python2.7/site-packages')
from fitparse import Activity, FitParseError
from time import mktime
import datetime

LAP_TRIGGER_MAP = {\
    "manual":             "Manual", \
    "time":               "Time", \
    "distance":           "Distance", \
    "position_start":     "Location", \
    "position_lap":       "Location", \
    "position_waypoint":  "Location", \
    "position_marked":    "Location", \
    "session_end":        "Manual", \
    "fitness_equipment":  "Manual"}

INTENSITY_MAP = {\
    "active":             "Active", \
    "warmup":             "Active", \
    "cooldown":           "Active", \
    "rest":               "Resting"}

SPORT_MAP = {\
    "running":            "Running", \
    "cycling":            "Biking"}

def semicircle_to_degrees(semicircles):
    """Convert a number in semicircles to degrees"""
    return semicircles * (180.0 / 2.0 ** 31)

def local_date_to_utc(date):
    """Local date to UTC"""
    return datetime.datetime.utcfromtimestamp(mktime(date.timetuple()))

def main_fit(argv):
    filename=sys.argv[1]
    activity = Activity(filename)
    activity.parse()
    session = next(activity.get_records_by_type('session'))
    session_dict={}
    start_time=local_date_to_utc(session.get_data("start_time"))
    real_start_time=session.get_data("start_time")
    session_dict['elapsed-time']=session.get_data("total_elapsed_time")
    timer_time=session.get_data("total_timer_time")
    session_dict['timer-time']=timer_time
    distance=session.get_data("total_distance")
    session_dict['distance']=distance
    # TODO: possibility do this for very small distances
    if distance == 0.0:
        # TODO: change to None?
        session_dict['pace']=0.0
    else:
        session_dict['pace']=(1000./(distance/timer_time))/60.
    theid=("%04d" % start_time.year)+("%02d" % start_time.month)+("%02d" % start_time.day)+'T'+("%02d" % start_time.hour)+("%02d" % start_time.minute)+("%02d" % start_time.second)
    session_dict['id']=theid
    session_dict['start-time']=theid
    end_time=local_date_to_utc(session.get_data("timestamp"))
    end_time_id=("%04d" % end_time.year)+("%02d" % end_time.month)+("%02d" % end_time.day)+'T'+("%02d" % end_time.hour)+("%02d" % end_time.minute)+("%02d" % end_time.second)
    session_dict['end-time']=end_time_id
    session_dict['laps']={}
    session_dict['laps']['start-time']=[]
    session_dict['laps']['start-timestamp']=[]
    session_dict['laps']['end-time']=[]
    session_dict['laps']['end-timestamp']=[]
    session_dict['laps']['timer-time']=[]
    session_dict['laps']['distance']=[]
    session_dict['laps']['pace']=[]
    session_dict['laps']['maximum-speed']=[]
    session_dict['laps']['average-heart-rate']=[]
    session_dict['laps']['maximum-heart-rate']=[]
    session_dict['laps']['trigger-method']=[]
    session_dict['laps']['trackpoints']=[]
    session_dict['lap start times']=[]
    session_dict['lap end times']=[]
    session_dict['lap start distances']=[]
    session_dict['lap end distances']=[]
    for lap in activity.get_records_by_type('lap'):
        session_dict['laps']['start-time'].append(str(local_date_to_utc(lap.get_data("start_time"))))
        start_time=lap.get_data("start_time")
        session_dict['laps']['end-time'].append(str(local_date_to_utc(lap.get_data("timestamp"))))
        end_time=lap.get_data("timestamp")
        timer_time=lap.get_data("total_timer_time")
        session_dict['laps']['timer-time'].append(timer_time)
        distance=lap.get_data("total_distance")
        session_dict['laps']['distance'].append(distance)
        if distance == 0.0 or distance == None or timer_time == None:
            # TODO: change to None?
            session_dict['laps']['pace'].append(0.)
        else:
            session_dict['laps']['pace'].append((1000./(distance/timer_time))/60.)
        session_dict['laps']['maximum-speed'].append(lap.get_data("max_speed")) # opt
        session_dict['laps']['average-heart-rate'].append(lap.get_data("avg_heart_rate")) #opt
        session_dict['laps']['maximum-heart-rate'].append(lap.get_data("max_heart_rate")) #opt
        session_dict['laps']['trigger-method'].append(LAP_TRIGGER_MAP.get(lap.get_data("lap_trigger"), "Manual"))
        first_track_point=True
        for trackpoint in activity.get_records_by_type('record'):
            tts = trackpoint.get_data("timestamp")
            if tts >= start_time and tts <= end_time:
                if first_track_point==True:
                    session_dict['lap start times'].append((trackpoint.get_data("timestamp")-real_start_time).seconds)
                    session_dict['lap start distances'].append(trackpoint.get_data("distance"))
                    first_track_point=False
                session_dict['laps']['trackpoints'].append({})
                session_dict['laps']['trackpoints'][-1]['timestamp']=str(local_date_to_utc(trackpoint.get_data("timestamp")))
                session_dict['laps']['trackpoints'][-1]['elapsed-time']=(trackpoint.get_data("timestamp")-real_start_time).seconds
                session_dict['laps']['trackpoints'][-1]['latitude-position']=trackpoint.get_data("position_lat")
                session_dict['laps']['trackpoints'][-1]['longitude-position']=trackpoint.get_data("position_long")
                session_dict['laps']['trackpoints'][-1]['distance']=trackpoint.get_data("distance")
                session_dict['laps']['trackpoints'][-1]['altitude']=trackpoint.get_data("altitude")
                session_dict['laps']['trackpoints'][-1]['speed']=trackpoint.get_data("speed")
                session_dict['laps']['trackpoints'][-1]['heart rate']=trackpoint.get_data("heart_rate")
                # TODO: make sure this works out OK, get last valid position
                if session_dict['laps']['trackpoints'][-1]['latitude-position'] == None:
                    session_dict['laps']['trackpoints'][-1]['latitude']=None
                else:
                    session_dict['laps']['trackpoints'][-1]['latitude']=str(semicircle_to_degrees(session_dict['laps']['trackpoints'][-1]['latitude-position']))
                if session_dict['laps']['trackpoints'][-1]['longitude-position'] == None:
                    session_dict['laps']['trackpoints'][-1]['longitude']=None
                else:
                    session_dict['laps']['trackpoints'][-1]['longitude']=str(semicircle_to_degrees(session_dict['laps']['trackpoints'][-1]['longitude-position']))
        # add first and last
        session_dict['lap end times'].append(session_dict['laps']['trackpoints'][-1]['elapsed-time'])
        session_dict['lap end times'].append(session_dict['laps']['trackpoints'][-1]['distance'])
    # update things from laps
    session_dict['start-latitude']=session_dict['laps']['trackpoints'][0]['latitude']
    session_dict['start-longitude']=session_dict['laps']['trackpoints'][0]['longitude']
    session_dict['end-latitude']=session_dict['laps']['trackpoints'][-1]['latitude']
    session_dict['end-longitude']=session_dict['laps']['trackpoints'][-1]['longitude']
    return session_dict

def mps_minpkm(s):
    # XXXX: 16.6666666666 is conversion from m/s to min/km
    if s:
        return 16.6666666666/s
    else:
        return None

def main_graph_fitdistance(argv):
    # graph distance vs speed, distance vs altitude
    session_dict=main_fit(argv)
    filename=os.path.basename(sys.argv[1])
    # create gnuplot file
    # gnuplot> set multiplot layout 2,1 rowsfirst
    # multiplot> plot "test.txt" using 1:2 with lines
    # multiplot> plot "test.txt" using 1:3 with lines
    distance_points = [(p['distance'],mps_minpkm(p['speed']),p['altitude'],p['heart rate']) for p in session_dict['laps']['trackpoints']]
    # create tempfile
    fd,fname = tempfile.mkstemp()
    fh = os.fdopen(fd,'w')
    print session_dict['lap start distances']
    print session_dict['lap end distances']
    # print '# plot "data.txt" using 1:2 with lines '
    for p in distance_points:
        if p[0] is None or p[1] is None or p[2] is None:
            fh.write("%s %s %s %s\n" % ('nan','nan','nan','nan'))
        else:
            if p[3] is None:
                fh.write("%f %f %f 'nan'\n" % p[0:3])
            else:
                fh.write("%f %f %f %f\n" % p)
    fh.close()
    print session_dict['lap start distances']
    print session_dict['lap end distances']
    lapdistances=''
    for z in session_dict['lap start distances']:
        lapdistances+=('set arrow from %f,graph(0,0) to %f,graph(1,1) nohead lw 0.1 lc rgb "blue"\n' % (z,z))
    gnuplotstring=("set multiplot layout 3,1 rowsfirst title '" + filename + "'\n" +
                   "set grid ytics mytics lc rgb \"#bbbbbb\" lt 1, lc rgb \"#bbbbbb\" lt 0\n" +
                   "unset key\n" +
                   "set title 'distance (m) vs speed (min/km)'\n" +
                   # TODO: want option for wider variety of speeds
                   "set xtics 0.,1000.\n" +
                   "set mxtics 10\n" +
                   "set yrange [2.5:8]\n" +
                   "set ytics 1.0\n" +
                   "set mytics 6\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   lapdistances +
                   "plot \"%s\" using 1:2 with lines\n" % fname +
                   "set title 'distance (m) vs altitude (m)'\n" +
                   "set xtics 0.,1000.\n" +
                   "set mxtics 10\n" +
                   "set yrange [*:*]\n" +
                   "set ytics 50\n" +
                   "set mytics 5\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   lapdistances +
                   "plot \"%s\" using 1:3 with lines\n" % fname +
                   "set title 'distance (m) vs HR'\n" +
                   "set xtics 0.,1000.\n" +
                   "set mxtics 10\n" +
                   "set yrange [*:*]\n" +
                   "set ytics 20\n" +
                   "set mytics 5\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   lapdistances +
                   "plot \"%s\" using 1:4 with lines\n" % fname)
    plot = subprocess.Popen(['gnuplot','-persist'], stdin=subprocess.PIPE)
    plot.communicate(gnuplotstring)

def main_graph_fittime(argv):
    # graph time vs speed, time vs altitude
    session_dict=main_fit(argv)
    filename=os.path.basename(sys.argv[1])
    # create gnuplot file
    # gnuplot> set multiplot layout 2,1 rowsfirst
    # multiplot> plot "test.txt" using 1:2 with lines
    # multiplot> plot "test.txt" using 1:3 with lines
    time_points = [(p['elapsed-time'],mps_minpkm(p['speed']),p['altitude'],p['heart rate']) for p in session_dict['laps']['trackpoints']]
    # create tempfile
    fd,fname = tempfile.mkstemp()
    fh = os.fdopen(fd,'w')
    # print '# plot "data.txt" using 1:2 with lines '
    for p in time_points:
        if p[0] is None or p[1] is None or p[2] is None:
            fh.write("%s %s %s %s\n" % ('nan','nan','nan','nan'))
        else:
            if p[3] is None:
                fh.write("%f %f %f 'nan'\n" % p[0:3])
            else:
                fh.write("%f %f %f %f\n" % p)
    fh.close()
    print session_dict['lap start times']
    print session_dict['lap end times']
    laptimes = ''
    for z in session_dict['lap start times']:
        laptimes+=('set arrow from %f,graph(0,0) to %f,graph(1,1) nohead lw 0.1 lc rgb "blue"\n' % (z,z))
    gnuplotstring=("set multiplot layout 3,1 rowsfirst title '" + filename + "'\n" +
                   "set grid ytics mytics lc rgb \"#bbbbbb\" lt 1, lc rgb \"#bbbbbb\" lt 0\n" +
                   "unset key\n" +
                   "set title 'time (s) vs speed (min/km)'\n" +
                   # TODO: want option for wider variety of speeds
                   "set xtics 0.,600.\n" +
                   "set mxtics 10\n" +
                   "set yrange [2.5:8]\n" +
                   "set ytics 1.0\n" +
                   "set mytics 6\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   laptimes +
                   "plot \"%s\" using 1:2 with lines\n" % fname +
                   "set title 'time (s) vs altitude (m)'\n" +
                   "set xtics 0.,600.\n" +
                   "set mxtics 10\n" +
                   "set yrange [*:*]\n" +
                   "set ytics 50\n" +
                   "set mytics 5\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   laptimes +
                   "plot \"%s\" using 1:3 with lines\n" % fname +
                   "set title 'time (s) vs HR '\n" +
                   "set xtics 0.,600.\n" +
                   "set mxtics 10\n" +
                   "set yrange [*:*]\n" +
                   "set ytics 20\n" +
                   "set mytics 5\n" +
                   "set grid xtics\n" +
                   "set grid mxtics\n" +
                   "set grid ytics\n" +
                   "set grid mytics\n" +
                   laptimes +
                   "plot \"%s\" using 1:4 with lines\n" % fname)
    plot = subprocess.Popen(['gnuplot','-persist'], stdin=subprocess.PIPE)
    plot.communicate(gnuplotstring)

def main_tcx(argv):
   # XXXX: requires full path
   tcx_dict={}
   tcx=etree.parse(argv[1])
   # get xml-id
   # print tcx.xpath('/TrainingCenterDatabase/Activities/Activity/TotalTimeSeconds/text()')
   # https://stackoverflow.com/questions/4255277/lxml-etree-xmlparser-namespace-problem
   ids=tcx.xpath("//*[local-name() = 'Id']//text()")
   # tcx_dict['id']=ids
   tcx_dict['id']=tcx.xpath("//*[local-name() = 'Id']//text()")[0].replace('-','').replace(':','')
   tcx_dict['start-time']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'Time']//text()")[0].replace('-','').replace(':','')
   tcx_dict['end-time']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'Time']//text()")[-1].replace('-','').replace(':','')
   tcx_dict['laps']={}
   times=[float(e) for e in tcx.xpath("//*[local-name() = 'Lap']/*[local-name() = 'TotalTimeSeconds']//text()")]
   tcx_dict['laps']['timer-time']=times
   distances=[float(e) for e in tcx.xpath("//*[local-name() = 'Lap']/*[local-name() = 'DistanceMeters']//text()")]
   tcx_dict['laps']['distance']=distances
   paces = zip(distances,times)
   paces = [(1000./(p[0]/p[1]))/60. for p in paces]
   tcx_dict['laps']['pace']=paces
   # XXXX: make sure heart rate data exists
   tcx_dict['laps']['average-heart-rate'] = len(paces)*[None]
   tcx_dict['laps']['maximum-heart-rate'] = len(paces)*[None]
   tcx_dict['start-latitude']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'LatitudeDegrees']//text()")[0]
   tcx_dict['start-longitude']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'LongitudeDegrees']//text()")[0]
   tcx_dict['end-latitude']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'LatitudeDegrees']//text()")[-1]
   tcx_dict['end-longitude']=tcx.xpath("//*[local-name() = 'Lap']//*[local-name() = 'LongitudeDegrees']//text()")[-1]
   # TODO: check this
   tcx_dict['timer-time']=0.
   for t in tcx_dict['laps']['timer-time']:
      tcx_dict['timer-time']+=int(t)
   tcx_dict['timer-time']=tcx_dict['timer-time']
   tcx_dict['distance']=0.
   for d in tcx_dict['laps']['distance']:
      tcx_dict['distance']+=int(d)
   tcx_dict['distance']=tcx_dict['distance']
   tcx_dict['pace']=(1000./(tcx_dict['distance']/tcx_dict['timer-time']))/60.
   return tcx_dict

def main_fit_id(argv):
    theids=[]
    if os.path.isdir(argv[1]):
        fit_files=[os.path.join(argv[1],f) for f in os.listdir(sys.argv[1]) if f.endswith('.fit')]
    else:
        fit_files=[argv[1]]
    for filename in fit_files:
        activity = Activity(filename)
        # TODO: this is the really time consuming one
        activity.parse()
        session = next(activity.get_records_by_type('session'))
        start_time=session.get_data("start_time")
        theid=("%04d" % start_time.year)+("%02d" % start_time.month)+("%02d" % start_time.day)+'T'+("%02d" % start_time.hour)+("%02d" % start_time.minute)+("%02d" % start_time.second)
        thefilenametimestamp=os.path.getmtime(filename)
        theids.append((os.path.normpath(filename),theid))
    for theid in theids:
        print theid[0] + ' ' + theid[1]

def main_gpx(argv):
    gpx_dict={}
    LatLng = namedtuple('LatLng', 'lat, lng')
    gpx=etree.parse(argv[1])
    # TODO: get time and convert
    gpx_dict['id']=gpx.xpath("//*[local-name() = 'metadata']//*[local-name() = 'time']//text()")[0].replace('-','').replace(':','')
    # TODO: fix badly, only first track segment
    gpx_dict['laps']={}
    track_point_times=[t.replace('-','').replace(':','') for t in gpx.xpath("//*[local-name() = 'trk']/*[local-name() = 'trkseg']/*[local-name() = 'trkpt']/*[local-name() = 'time']//text()")]
    # TODO: try and get real timer-time
    gpx_dict['total-time']=id_difference(gpx_dict['id'],track_point_times[-1])
    gpx_dict['start-time']=gpx_dict['id']
    gpx_dict['end-time']=track_point_times[-1]
    # TODO: get only first and last trackpoint with valid location
    gpx_dict['start-latitude']=dict(gpx.xpath("//*[local-name() = 'trkpt']")[0].items())['lat']
    gpx_dict['start-longitude']=dict(gpx.xpath("//*[local-name() = 'trkpt']")[0].items())['lon']
    gpx_dict['end-latitude']=dict(gpx.xpath("//*[local-name() = 'trkpt']")[-1].items())['lat']
    gpx_dict['end-longitude']=dict(gpx.xpath("//*[local-name() = 'trkpt']")[-1].items())['lon']
    # get list of segments
    gpx_dict['laps']={}
    gpx_dict['laps']['distance']=[]
    gpx_dict['laps']['timer-time']=[]
    gpx_dict['laps']['pace']=[]
    for gpx_segment in gpx.xpath("//*[local-name() = 'trkseg']"):
        track_segment_times=[t.replace('-','').replace(':','') for t in gpx_segment.xpath("./*[local-name() = 'trkpt']/*[local-name() = 'time']//text()")]
        timer_time=id_difference(track_segment_times[0],track_segment_times[-1])
        tracksegment_lats=[dict(t.items())['lat'] for t in gpx_segment.xpath("./*[local-name() = 'trkpt']")]
        tracksegment_longs=[dict(t.items())['lon'] for t in gpx_segment.xpath("./*[local-name() = 'trkpt']")]
        start_positions=zip(tracksegment_lats[:-1],tracksegment_longs[:-1])
        end_positions=zip(tracksegment_lats[1:],tracksegment_longs[1:])
        start_end_pairs=zip(start_positions,end_positions)
        tracksegment_distances=[]
        for lines in zip(start_positions,end_positions):
            tracksegment_distances.append(haversine_distance(LatLng(float(lines[0][0]), float(lines[0][1])),
                                                             LatLng(float(lines[1][0]), float(lines[1][1]))))
        total_distance=0
        for d in tracksegment_distances:
            total_distance+=d
        gpx_dict['laps']['distance'].append(total_distance)
        gpx_dict['laps']['timer-time'].append(timer_time)
        gpx_dict['laps']['pace'].append((1000./(total_distance/timer_time))/60.)
    total_distance=0.
    for d in gpx_dict['laps']['distance']:
        total_distance+=d
    timer_time=0.
    for t in gpx_dict['laps']['timer-time']:
        timer_time+=t
    gpx_dict['timer-time']=timer_time
    # XXXX: make sure heart rate data exists
    gpx_dict['laps']['average-heart-rate'] = len(gpx_dict['laps']['pace'])*[None]
    gpx_dict['laps']['maximum-heart-rate'] = len(gpx_dict['laps']['pace'])*[None]
    gpx_dict['distance']=total_distance
    gpx_dict['pace']=(1000./(gpx_dict['distance']/gpx_dict['timer-time']))/60.
    return gpx_dict

def main_utc(argv):
    print id_to_utc(argv[1])

def id_to_utc(theid):
    YYYY=theid[0:4]
    MM=theid[4:6]
    DD=theid[6:8]
    hh=theid[9:11]
    mm=theid[11:13]
    ss=theid[13:15]
    # TODO: just adding 6 hours for now
    dt=datetime.datetime(int(YYYY),int(MM),int(DD),int(hh),int(mm),int(ss))-datetime.timedelta(hours=6)
    return "%04d%02d%02dT%02d%02d%02d" % (dt.year,dt.month,dt.day,dt.hour,dt.minute,dt.second)

def id_difference(theid1,theid2):
    YYYY1=theid1[0:4]
    MM1=theid1[4:6]
    DD1=theid1[6:8]
    hh1=theid1[9:11]
    mm1=theid1[11:13]
    ss1=theid1[13:15]
    YYYY2=theid2[0:4]
    MM2=theid2[4:6]
    DD2=theid2[6:8]
    hh2=theid2[9:11]
    mm2=theid2[11:13]
    ss2=theid2[13:15]
    dt=datetime.datetime(int(YYYY2),int(MM2),int(DD2),int(hh2),int(mm2),int(ss2)) - datetime.datetime(int(YYYY1),int(MM1),int(DD1),int(hh1),int(mm1),int(ss1))
    # convert to seconds
    return dt.seconds

# from https://news.ycombinator.com/item?id=9282102
# TODO: want something slightly better, maybe import geopy
def haversine_distance(origin, destination):
    """ Haversine formula to calculate the distance between two lat/long points on a sphere """
    radius = 6371000 # FAA approved globe radius in km
    dlat = math.radians(destination.lat-origin.lat)
    dlon = math.radians(destination.lng-origin.lng)
    a = math.sin(dlat/2) * math.sin(dlat/2) + math.cos(math.radians(origin.lat)) \
        * math.cos(math.radians(destination.lat)) * math.sin(dlon/2) * math.sin(dlon/2)
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    d = radius * c
    # Return distance in km
    return d

if __name__ == "__main__":
    # TODO: detect filetype automatically
    # TODO: could ensure_ascii=False be required?
    # TODO: use pymath here, but without numpy/scipy dependencies
    if '--fit' in sys.argv:
        print json.dumps(main_fit(sys.argv))
    elif '--graph-fit-distance' in sys.argv:
        main_graph_fitdistance(sys.argv)
    elif '--graph-fit-time' in sys.argv:
        main_graph_fittime(sys.argv)
    elif '--fit-id' in sys.argv:
        main_fit_id(sys.argv)
    elif '--tcx' in sys.argv:
        print json.dumps(main_tcx(sys.argv))
    elif '--gpx' in sys.argv:
        print json.dumps(main_gpx(sys.argv))
    elif '--utc' in sys.argv:
        main_utc(sys.argv)
