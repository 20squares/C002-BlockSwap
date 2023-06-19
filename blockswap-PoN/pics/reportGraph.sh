#!/bin/gnuplot -persist

set output 'reportGraph.gif'
set terminal gif nooptimize size 1500,1200 font ',24'

set title "Report revenue Vs. Submission fees" font "Monospace Regular, 24"

# Legend styling
set key font "Monospace Regular, 24" spacing 3 top left reverse Left

# Axes label styling
set xlabel "Report revenue" offset 0,0 font "Monospace Regular, 24"
set ylabel "Submission fees" offset 0,0 font "Monospace Regular, 24"

# Axes tics styling
set xtics scale 1.0 nomirror offset 0,0 font "Monospace Regular, 24"
set ytics scale 1.0 nomirror offset 0,0 font "Monospace Regular, 24"

# Axes ranges
set xrange [ 0 : 50.0000 ] noreverse nowriteback 
set yrange [ 0 : 50.0000 ] noreverse nowriteback

set grid xtics mxtics ytics mytics ztics mztics

unset parametric

set style line 1 lc rgb "red" lt 1
# set colorbox vertical origin screen 0.9, 0.2 size screen 0.5, 0.6 front  noinvert bdefault

NO_ANIMATION = 1
plot (x>0 ? x : 1/0) with filledcurve y2 title "Reporting is a dominant strategy"

