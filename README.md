# Overview
This repository hosts the code and data required to replicate the results in the MS 'Learning predispositions coevolve with song divergence in pied flycatchers'.

## Table of Contents
- [Data and File overview](#data-and-file-overview)
- [Methods to generate data](#methods-to-generate-data)
- [File specific information](#file-specific-information)

## Data and File overview

This repository contains the following:
1. Data: 
- `data_song_mds.xlsx`
- `data_syllable_mds.xlsx`
- `GSIclusteringoutput_syllables.xlsx`
2. Codes:
- `Song_analysis_Translocated_males_LDA.R`
- `Syllable_analysis_Translocatedmales_LDAandClustering.R`
- `Song_analysis_hybridincluded_SupplementaryNote.R`

Relationship between files:
1. The dataset `data_song_mds.xlsx` was used for the analysis of songs of the translocated males and analyses can be reproduced using the R script `Song_analysis_Translocated_males_LDA.R`. The code can also be used to replicate Figure 1B and Figure S1.
2. The dataset `data_syllable_mds.xlsx` was used for the analysis of syllables of the translocated males and analyses can be reproduced using the R script `Song_analysis_Translocated_males_LDA.R`. The code can also be used to replicate Figure 2A,C; Figure S2; Figure S3; Figure S4
3. The dataset `GSIclusteringoutput_syllables.xlsx` is the output of code used in (2) that is too heavy to run and is therefore, attached here. This dataset is the global silhouette index used to find the optimum number of clusters of syllables of individuals used in the translocation experiment. 
4. The code `Song_analysis_hybridincluded_SupplementaryNote.R` can be used to replicate the results in the Supplementary Note. 

## Methods to generate data

## File specific information
