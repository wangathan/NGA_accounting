#!/bin/bash
#$ -pe omp 16
#$ -l mem_total=96G
#$ -l h_rt=24:00:00
#$ -V

echo date

Rscript ABoVE_NGA_tileintersect.R

echo date
