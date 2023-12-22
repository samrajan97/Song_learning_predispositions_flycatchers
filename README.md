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
1) data_song_mds.txt
Number of variables: 25

Number of cases/rows: 1022

Variable List:

- "Population" = Experimental group each individual belongs to, but including information on whether their parentage (local vs non local, dutch father vs dutch mother)
- "Population2" = Experimental group each individual belongs to (used in the final analysis): Swedish (Swedish birds), Dutch (Dutch birds), Dutch egg (Translocated males), Hybrids (Half Ducth-half Swedish birds)
"Individual" = Individual identity
"Song" = Identity of songs that an individual produces
"Dutch_father" = This is relevant for hybrid birds and refers to whether they have a dutch father or mother
"Social_father" = The social father for all the locally born birds in the translocation experiment
"Rec_year" = Year in which the songs of the individual were recorded
"Birth_year" = Birth year
"Age" = Estimate of age for all locally born birds
"First_adult_year" = Year they were observed breeding for the first time
"Hatch_date" = The date when they hatched
"Fledging_mass" = Mass at fledging
"Record_tarsus" = Tarsus length as adults
"Record_p3" = NA
"Record_mass" = Mass recorded as adults
"pc_1_value" = First principal component scores for each song
"pc_2_value" = Second principal component scores for each song
"pc_3_value" = Third principal component scores for each song
"pc_4_value" = Fourth principal component scores for each song
"pc_5_value" = Fifth principal component scores for each song
"pc_6_value" = Sixth principal component scores for each song
"pc_7_value" = Seventh principal component scores for each song
"pc_8_value" = Eighth principal component scores for each song
"pc_9_value" = Ninth principal component scores for each song
"pc_10_value" = Tenth principal component scores for each song

Missing data codes: NA
