# Overview
This repository hosts the code and data required to replicate the results in the MS 'Learning predispositions track population divergence in the songs of wild pied flycatchers (Ficedula hypoleuca)
.

## Table of Contents
- [Data and File overview](#data-and-file-overview)
- [Methods to generate data](#methods-to-generate-data)
- [File specific information](#file-specific-information)

## Data and File overview

This repository contains the following:
1. Data: 
- `data_song_mds.xlsx`
- `rawsong_acoustics.xlsx`
- `data_syllable_mds.xlsx`
- `GSIclusteringoutput_syllables.xlsx`
2. Codes:
- `Song_analysis_Translocated_males_LDA.R`
- `rawacousticmeasures_songs.R`
- `Syllable_analysis_Translocatedmales_LDAandClustering.R`
- `Song_analysis_hybridincluded_SupplementaryNote.R`

Description of files and relationship between files:
1. The dataset `data_song_mds.xlsx` was used for the analysis of songs of the translocated males and analyses can be reproduced using the R script `Song_analysis_Translocated_males_LDA.R`. The code can also be used to replicate Figure 1B and Figure S1 and S2.
2. The dataset `rawsong_acoustics.xlsx` was used for the analysis of spectral and temporal variables contributing to the acoustic differences between groups. It can be reproduced using the R script `rawacousticmeasures_songs.R`
3. The dataset `data_syllable_mds.xlsx` was used for the analysis of syllables of the translocated males and analyses can be reproduced using the R script `Song_analysis_Translocated_males_LDA.R`. The code can also be used to replicate Figure 2A,C; Figure S4-S6. 
4. The dataset `GSIclusteringoutput_syllables.xlsx` is the output of code used in (3) that is too heavy to run and is therefore, attached here. This dataset is the global silhouette index used to find the optimum number of clusters of syllables of individuals used in the translocation experiment. 
5. The code `Song_analysis_hybridincluded_SupplementaryNote.R` can be used to replicate the results for Supplementary Figure 3.  

## Methods to generate data

1. Description of methods used for collection/generation of data: 

Dataset `data_song_mds.xlsx` and dataset `data_song_mds.xlsx`
The song and syllable repertoires of 88 adult male pied flycatchers were recorded and analysed during the two breeding seasons of 2020 and 2021 in Dutch and Swedish populations of pied flycatchers. These contained unmanipulated individuals breeding in the Dutch (N = 26, ref= 'Dutch') and Swedish (N = 38, ref = 'Swedish') populations, as well as translocated males with 100% Dutch ancestry (N = 7, ref = 'Dutch egg') and half Dutch-half Swedish males (N = 17, ref = 'hybrid') that were breeding in the Swedish population. 

Songs of each individual were imported into a database using the Luscinia sound analysis program (http://github.com/rflachlan/luscinia, version 2.17.11.22.01). For each song, the elements (the smallest unit of a song, subunits of a syllable) were measured as a continuous sound sequence and were grouped into syllables if the gap between them was less than 30ms. In order to compare how vocalisations differed between unmanipulated males from the Dutch and Swedish populations and translocated males, the structure of each syllable within a song was compared with every other syllable in the dataset using a dynamic time warping (DTW) algorithm. The analysis was carried out with a compression factor of 0.0001, time SD weighting of 1, maximum warp of 25 % and a minimum element length of 25 samples. Additionally, we used the following weightings for time (5), mean frequency (1), mean frequency change (1), normalised mean frequency (1) with all other acoustic features left at the standard values. The output of the DTW analysis are multiple syllable dissimilarity matrices with the acoustic distance for each acoustic measurement between all possible pairs of syllables in the dataset. Further, using the non-metric multidimensional scaling function in Luscinia, we converted the syllable dissimilarity matrices into Euclidean dimensions at both the syllable and the song level, resulting in the corresponding datasets. 

2. Instrument- or software-specific information needed to interpret the data: 
R v.4.2.0 https://www.r-project.org/
R studio v2022.07.2+576

## File specific information
1) data_song_mds.xlsx
Number of variables: 25

Number of cases/rows: 1022

1. Variable List:
- "Population" = Experimental group each individual belongs to, but including information on whether their parentage (local vs non local, dutch father vs dutch mother)
- "Population2" = Experimental group each individual belongs to (used in the final analysis): Swedish (Swedish birds), Dutch (Dutch birds), Dutch egg (Translocated males), Hybrids (Half Ducth-half Swedish birds)
- "Individual" = Individual identity
- "Song" = Identity of songs that an individual produces
- "Dutch_father" = This is relevant for hybrid birds and refers to whether they have a dutch father or mother
- "Social_father" = The social father for all the locally born birds in the translocation experiment
-"Rec_year" = Year in which the songs of the individual were recorded
- "Birth_year" = Birth year
- "Age" = Estimate of age for all locally born birds
- "First_adult_year" = Year they were observed breeding for the first time
- "Hatch_date" = The date when they hatched
- "Fledging_mass" = Mass at fledging
- "Record_tarsus" = Tarsus length as adults
- "Record_p3" = NA
- "Record_mass" = Mass recorded as adults
- "pc_1_value" = First principal component scores for each song
- "pc_2_value" = Second principal component scores for each song
- "pc_3_value" = Third principal component scores for each song
- "pc_4_value" = Fourth principal component scores for each song
- "pc_5_value" = Fifth principal component scores for each song
- "pc_6_value" = Sixth principal component scores for each song
- "pc_7_value" = Seventh principal component scores for each song
- "pc_8_value" = Eighth principal component scores for each song
- "pc_9_value" = Ninth principal component scores for each song
- "pc_10_value" = Tenth principal component scores for each song

Missing data codes: NA

2) rawsong_acoustics.xlsx
Number of variables: 18

Number of cases/rows: 769

1. Variable List:

- "Population2" = Experimental group each individual belongs to (used in the final analysis): Swedish (Swedish birds), Dutch (Dutch birds), Dutch egg (Translocated males)
- "Individual" = Individual identity
- "Song" = Identity of songs that an individual produces
- "n_syllables" = First temporal measure, number of syllables in a song
- "syllable_length" = Second temporal measure, mean syllable length in a song
- "song_length" = Third temporal measure, song length
- "syllable_gap" = Fourth temporal measure, mean intersyllable gap length in a song
- "tempo" = Fifth temporal measure, number of syllables/song length
- "Overall_Min_Freq" = Mean minimum frequency of syllables in a song
- "Overall_Max_Freq" = Mean maximum frequency of syllables in a song
- "Mean_frequency_Mean" = Mean of mean frequency of syllables in a song
- "Mean frequency Variance" = Mean of mean frequency variance of syllables in a song
- "Mean_frequency_Maximum" = Mean of mean frequency maximum of syllables in a song
- "Mean_frequency_Minimum" = Mean of mean frequency minimum of syllables in a song
- "Mean frequency Start" = Mean of mean frequency at the start of syllables in a song
- "Mean frequency End" = Mean of mean frequency at the end of syllables in a song
- "Wiener_entropy_Mean" = Mean of mean wiener entropy of syllables in a song
- "Wiener_entropy_Variance" = Mean of wiener entropy variance of syllables in a song

Missing data codes: NA


3) data_syllable_mds.xlsx
Number of variables: 25

Number of cases/rows: 11,718

Column information same as for `data_song_mds.xlsx`

4) GSIclusteringoutput_syllables.xlsx
Number of variables: 3

Number of cases/rows: 1001

- "clusters" = n number of clusters from 1-1000
- "y" = Global silhoutte index (GSI) for that cluster
- "diff" = Difference in the GSI index between the cluster and preceding cluster

