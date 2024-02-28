## Clear memory
rm(list = ls())

## Set up packages ##
library(readxl) #import excel files
library(dplyr) #Data formatting
library(tidyverse) #Data formatting
library(MASS) #for LDA analysis
library(ggplot2) #plotting
library(ggpubr) # arrange multiple plots
library(glmmTMB) #Linear mixed models
library(DHARMa) #Model diagnostics
library(sjPlot) #Plotting of random effects
library(hopkins) #For hopkins statistics
library(factoextra) #Plotting hopkins clustering
library(cluster) #heirarchical clustering analysis
library(dendextend) #for analysis of dendograms

## Import dataset 
syl_mds <- read_excel("~/data_syllable_mds.xlsx") 
attach(syl_mds)

## Make Experimental_group a factor
syl_mds$Experimental_group <- factor(syl_mds$Experimental_group, levels = c("Dutch", "Swedish", "Hybrid", "Dutch egg"))
levels(syl_mds$Experimental_group)

## LINEAR DISCRIMINANT ANALYSIS ##

## Subset the swedish and dutch syllable
data_subsyl_SN <-  subset(syl_mds, Experimental_group %in% c("Swedish", "Dutch"))
data_subsyl_SN <- droplevels(data_subsyl_SN)

##Need CV=TRUE to get to the confusion matrix
lda_sylSN <- lda(Experimental_group ~ pc_1_value + pc_2_value + pc_3_value
                 +pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+pc_10_value,
                 data = data_subsyl_SN, CV = TRUE)

#these lines allow you to assess the classification accuracy
ctsylSN <- table(data_subsyl_SN$Experimental_group, lda_sylSN$class)
ctsylSN ## Confusion matrix 
diag(prop.table(ctsylSN, 1)) ##accuracy of LDA for each group

## accuracy of lda model over all categories
sum(diag(ctsylSN))/sum(ctsylSN)

##get a new dataset with the LD1 values for all the Swedish and Dutch syllables
####these lines project a new data set using the previous LD functions. First, you have to re-run the LDA with CV = FALSE
##Just a trial without sub sampling the data
lda_sylSN <- lda(formula = Experimental_group ~ pc_1_value + pc_2_value + pc_3_value +
                   pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+
                   pc_10_value, data = data_subsyl_SN, CV = FALSE)
lda_sylSN

##Get posterior probabilities for Dutch and Swedish syllables
predictsylSN.ld <- predict(object = lda_sylSN, newdata = data_subsyl_SN) ##Prediction to get the LD1 scores for all Swedish and Dutch syllables
combined_sylSN <- cbind(data_subsyl_SN, predictsylSN.ld) ##Combine the original dataset with the posterior probabilities and LD1 scores

##Create  the testing dataset of the translocated males syllables
data_subsyl_D <-  subset(syl_mds, Experimental_group %in% c("Dutch egg"))
data_subsyl_D <- droplevels(data_subsyl_D)

## PART1:  projection of translocated males syllables onto Dutch and Swedish syllables
D_sylSN.ld <- predict(object = lda_sylSN, newdata = data_subsyl_D) ##Predict posterior probabilities for the treatment group
combined_sylD <- cbind(data_subsyl_D, D_sylSN.ld) ##Combine the dataset to get the LD1 and classification scores for all songs

#these lines allow you to assess the classification accuracy
ctsylD <- table(data_subsyl_D$Experimental_group, D_sylSN.ld$class)
ctsylD ## Confusion matrix 
diag(prop.table(ctsylD, 1)) ##accuracy of LDA for Dutch egg birds

##Combine dataset of posterior probabilties of LD syllable scores of translocated males with Dutch and Swedish syllables
combined_sylSND<- rbind(combined_sylSN, combined_sylD)

## Replicate Figure 2A:
LD_syllables <- ggplot(combined_sylSND, aes(LD1, colour = Experimental_group, fill = Experimental_group)) + 
  geom_density(aes(alpha = factor(Experimental_group)), kernel = c("gaussian")) + theme(legend.position = "none") +   
  scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#CC7CFF")) + theme_bw() + 
  scale_colour_manual(values = c('#ac1308',  '#005f62', '#7000b7')) +
  labs(x = "LD syllable scores", y = "Probability Density") +
  xlim(c(-4,4)) + ylim(c(0,0.8)) + theme(legend.position = 'none') +
  theme(axis.title = element_text(size = 45)) +
  theme(axis.text = element_text(size = 40)) + 
    scale_alpha_manual(values = c(0.9,0.9,0.7), guide= "none") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(axis.ticks.length=unit(.45, "cm"))  +
  theme(axis.title.x = element_text(vjust = -0.5), 
        axis.title.y = element_text(hjust = 0.4)) + 
  theme(axis.line = element_line(colour = 'black', size = 1.0), axis.ticks = element_line(colour = "black", size = 1.0)) + geom_vline(xintercept =- 0.237)
LD_syllables

## CLUSTERING ANALYSIS ##

##Step 1: Create a dataset with all those syllables that were classified as Dutch by the LD function from ALL three experimental groups
misdata_SND <- combined_sylSND %>%
  filter(class == "Dutch") 

##sample sizes of how many syllables from each group is classified as Dutch
combined_sylSND%>% group_by(Experimental_group, class)%>% count() 

## Step 2: clustering tendency
## Create dataset with only PC scores of all songs
misclasSND_onlypc <- misdata_SND[c(6:15), drop = FALSE]
rownames(misclasSND_onlypc) <- misdata_SND$Song ##Attach which song rows come from 

#Create a random dataset to check whether the syllable datatset differs from the random dataset
set.seed(123)
random_misclasSND_onlypc <- apply(misclasSND_onlypc, 2, 
                                  function(x){runif(length(x), min(x), (max(x)))}) ##Random dataset
random_misclasSND_onlypc <- as.data.frame(random_misclasSND_onlypc) 

## Convert both datasets to euclidean distances between the different cells
euc_misclasSND <- dist(misclasSND_onlypc, method = "euclidean", diag = TRUE)
euc_randomSND <- dist(random_misclasSND_onlypc, method = "euclidean")

##How does the data cluster between the actual and random dataset? 
hopkins(misclasSND_onlypc) #0.99= high clusterability
hopkins(random_misclasSND_onlypc) #0.50, random clustering

##produce heat maps to see clustering tendency
plotclus_misclassSND <-fviz_dist(euc_misclasSND)
## This code takes a long time to run, run only if necessary
plotclus_misclassSND
plotclus_randommisclassSND <- fviz_dist(euc_randomSND)
plotclus_randommisclassSND

## Step 3: Heirarchical clustering analysis with different methods
#hc1 <- agnes(euc_misclasSND, method = "average") #0.895
hc2 <- agnes(euc_misclasSND, method = "ward") ##ward is the best, 0.994
#hc3 <- agnes(euc_misclasSND, method = "complete") #0.935
hc2$ac ##check coefficient

##convert the clustering output to dendextend object for further analysis 
dendogram <- as.dendrogram(hc2)

# Step 4: What are the optimal number of clusters?

#1) Global silhouette index for heirarchical clustering: 
#run these lines only if you want to do it again, takes a long time
#Otherwise just load in the excel sheet, code below
n_clust <- fviz_nbclust(misclasSND_onlypc, hcut, k.max = 1000, method = "silhouette")+
  labs(subtitle = "Silhouette method")
n_clust <- n_clust$data
n_clust <- n_clust %>%
mutate(diff = y - lag(y))

## Replicate Figure S4:
#Import clustering output from previous step
n_clust <- read_excel("~/GSIclusteringoutput_syllables.xlsx") 
attach(n_clust)
n_clust$clusters <- as.numeric(n_clust$clusters)
optimumclustersSND <- ggplot(n_clust, aes(x = clusters, y = y)) + geom_line(aes(group=1))+
    geom_vline(xintercept = 146) + 
    geom_vline(xintercept = 215) + 
    geom_vline(xintercept = 356) + theme_bw()+
    theme(panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = "none") +
    labs(x = "Number of clusters (Syllable types)", y = "GSI") 
optimumclustersSND
  
## Step 5: identify syllables to syllable clusters 
##Run everything after this again with k= 147 and k=356 to replicate Figure S5.

##Create clusters based on how many clusters we should have, and append them to original dataset
sub_grp <- cutree(dendogram, k = 215)
cluster_SND <- mutate(misdata_SND, cluster = sub_grp)

##Create a column for the total number of misclassified syllables per experimental group
cluster_SND <- cluster_SND %>%
    group_by(Experimental_group)%>%
    mutate(totaln = n()) 
  
## Step 6: create a summary dataset about proportions of syllables from each experimental group in the cluster
##First create the proportion with which each experimental group is present in the cluster, normalised by the total number of misclassified syllables from each group
propSND <- cluster_SND %>%
    group_by(cluster,Experimental_group,totaln) %>%
    summarise(n = n()) %>%
    mutate(prop = n/totaln)
  
##Now create a new column for each cluster depending on whether or not dutch egg birds have a syllable present in the cluster or not
propSND <- propSND %>%
    group_by(cluster) %>%
    mutate(Present = case_when(
      any("Dutch egg" %in% Experimental_group) ~ "1"
    )) %>%
    filter(Experimental_group != "Dutch egg")
propSND[is.na(propSND)] <- "0"
propSND$Present <- as.numeric(propSND$Present) #Dutch egg syllables present or not?
  
##Now calculate the relative proportion of Swedish and Dutch syllables in the syllable cluster
##now you have two rows for each cluster with a mix of syllables from the two populations
propSND <- propSND %>%
    group_by(cluster) %>%
    mutate(totalprop = sum(prop), relative_prop = prop/totalprop) 
  
## Now for each cluster, instead of having the relative proportion of dutch and swedish syllables, calculate how 'Swedish' each cluster is:
## That is, the proportion of syllables originating from the Swedish population
##create dataset with only values of Swedish proportion for each cluster
propSD <- propSND%>%
    filter(Experimental_group == "Swedish")
##Create another dataset for all those syllable clusters where there are no Swedish syllables
propDD <- propSND %>%
    filter(Experimental_group == "Dutch") %>%
    filter(relative_prop == 1) 
  
##Create column of 0 for the dataset with no Swedish syllables
propDD["relative_prop"][propDD["relative_prop"] == 1] <- 0
propDD["Experimental_group"][propDD["Experimental_group"] == "Dutch"] <- "Swedish"#change the population to Swedish
finalpropSND <- rbind(propSD,propDD)#Combine the two datasets
finalpropSND$pop_present<- "Dutch egg"
colnames(finalpropSND)[8] <- "Swedish_prop" ##Change column name to show that it is the proportion of Swedish syllables in the cluster
  
## Step 8:Account for individual differences of translocated males in syllable production
##For each cluster where there are dutch egg syllables, which Dutch egg individual is present and with how many syllables?
indDE <- cluster_SND %>%
    filter(Experimental_group == 'Dutch egg') %>%
    dplyr::select(cluster, Individual) %>%
    group_by(cluster,Individual) %>%
    summarise(n = n()) %>%
    filter(cluster != '215')
  
##dataset with how swedish each cluster is
indSN <- finalpropSND %>%
    dplyr::select(cluster, Experimental_group, Swedish_prop, Present)
  
##For each individual, which cluster are they not a part of?
#List of clusters
dfclusters <- finalpropSND %>%
    dplyr::select(cluster)
##List of Swedish proportion in clusters
dspropcolumn <- finalpropSND %>%
    dplyr::select(cluster,Swedish_prop)
  
##Make a dataset where each cluster has values for the 7 individuals about the number of syllables andthe presence and absence
# List of individuals
individuals <- c("DUTCH_CZ.05412", "DUTCH_DA.64239", "DUTCH_DA.64593", 
                   "DUTCH_DA.64597", "DUTCH_DA.64628", "DUTCH_DA.65911", 
                   "DUTCH_RNR_unknown_blal.onon")
  
# Create an empty list to store the results
result_list <- list()
  
# Loop through each individual
for (individual in individuals) {
    
# Filter data for the current individual
ind_data <- indDE %>%
      filter(Individual == individual) %>%
      dplyr::select(cluster, Individual, n)
    
    # Find clusters not present for the individual
    clusters_not_present <- setdiff(dfclusters$cluster, ind_data$cluster)
    
    # Create a data frame with clusters not present
    df_not_present <- data.frame(cluster = clusters_not_present,
                                 Individual = individual,
                                 n = 0,
                                 Present = '0')
    
    # Add Present column to the data for the individual
    ind_data <- mutate(ind_data, Present = '1')
    
    # Combine data frames
    combined_data <- rbind(ind_data, df_not_present)
    
    # Merge with dspropcolumn
    final_data <- left_join(combined_data, dspropcolumn, by = c('cluster'))
    
    # Add result to the list
    result_list[[individual]] <- final_data
  }
  
# Combine all individual results into one data frame
final_result <- do.call(rbind, result_list)
final_result <- as.data.frame(final_result)
str(final_result)
  
#Prepare dataset
final_result$cluster <- as.factor(final_result$cluster)
final_result$Individual <- as.factor(final_result$Individual)
final_result$Present <- as.numeric(final_result$Present)
  
##Models for the relationship between Presence and absence and proportion ofSwedish syllables in clusters
log_indDE <- glmmTMB(Present ~ Swedish_prop  + (1|Individual) + (1|cluster), family = binomial, data = final_result)
summary(log_indDE) 
car::Anova(log_indDE)
  
#Check residuals
simulationOutputlog <- simulateResiduals(fittedModel = log_indDE, plot = F)
plot(simulationOutputlog) ## everything looks good
  
##Replicate Figure 2C
final_result <- final_result %>% mutate(pop_present = 'Dutch egg')
logisticpredicted <- ggplot(final_result, aes(x = Swedish_prop, y = Present, colour = pop_present)) +
  geom_jitter(data = finalpropSND, aes(x = Swedish_prop, y = Present), 
  height = 0.04,width = 0.02, alpha = 0.5, lwd = 1) +
  geom_smooth(method = "glm", method.args= list(family = "binomial"),aes(fill = pop_present))+
  scale_fill_manual(values = c( "#CC7CFF")) + scale_colour_manual(values = c( "#CC7CFF")) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +
  labs(x = "Proportion of syllables from the Swedish population", 
       y = "Presence of a syllable from a translocated male") + 
  theme(axis.text = element_text(size = 15))  + 
  theme(axis.title = element_text(size = 20)) + 
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 15))  + 
  theme(axis.ticks.length=unit(.25, "cm")) +
  theme(axis.title.x = element_text(vjust = -0.05)) 
logisticpredicted
  
##Replicate Figure S6:
##Reorder individuals to match Figure S1
final_result$Individual <- factor(final_result$Individual, levels = c('DUTCH_DA.64239','DUTCH_RNR_unknown_blal.onon',
                                                                      'DUTCH_DA.64597', 'DUTCH_DA.65911',
                                                                      'DUTCH_DA.64593', 'DUTCH_CZ.05412','DUTCH_DA.64628'))

individualLR <- ggplot(final_result, aes(x = Swedish_prop, y = Present, fill = Individual)) + 
  geom_jitter(height = 0.04,width = 0.02, alpha = 0.5, lwd = 1, aes(colour = Individual))+
  geom_smooth(method = "glm", method.args= list(family = "binomial"),aes(fill = Individual, colour = Individual)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c('#dba3ff', '#d490ff','#c468ff', '#ad2eff', '#a61aff', '#7c00ca','#6400a3')) +  
  scale_colour_manual(values = c('#dba3ff', '#d490ff','#c468ff', '#ad2eff', '#a61aff', '#7c00ca','#6400a3')) +
  theme(legend.position = "none") +
  labs(x = "Proportion of syllables from the Swedish population", y = "Presence of a syllable from a translocated male") + 
  theme(axis.text = element_text(size = 15))  + 
  theme(axis.title = element_text(size = 20)) + 
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 15))  + 
  theme(axis.ticks.length=unit(.25, "cm")) +
  theme(axis.title.x = element_text(vjust = -0.05))  + facet_wrap(.~Individual, nrow = 2)
individualLR

## Response to revier #3 comment:
#Make a dataset where you have only those clusters where atleast 1 syllable from translocated male is present
subset_dataind <- final_result %>%
  group_by(cluster) %>%
  filter(any(Present == 1))

##Check the number of unique clusters, to see how many clusters we lose in the process
length(unique(final_result$cluster))
length(unique(subset_dataind$cluster))
  
##Make a model to check the likelihood of syllables from males present with prop of Swedish sylllables
loghist <- glmmTMB(Present ~ Swedish_prop + (1|cluster) + (1|Individual), family = binomial, data = subset_dataind)
summary(loghist) ##We get the same result as before

#Check residuals
simulationOutputlog <- simulateResiduals(fittedModel = loghist, plot = F)
plot(simulationOutputlog) ## everything looks good


