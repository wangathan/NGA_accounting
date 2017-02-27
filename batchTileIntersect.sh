#!/bin/bash

qsub -N NGA_01_23_2017 runTileIntersect.sh 01_23_2017
qsub -N NGA_11_15_2016 runTileIntersect.sh 11_15_2016
qsub -N NGA_11_29_2016 runTileIntersect.sh 11_29_2016
qsub -N NGA_12_07_2016 runTileIntersect.sh 12_07_2016
