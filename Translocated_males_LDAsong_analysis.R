## Clear memory
rm(list = ls())

## Set up packages ##
library(readxl) 
library(dplyr)
library(tidyverse) 
library(ggplot2) 
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(effsize)

## Import dataset 
song_mds <- read_excel("~/Desktop/PF_prelimdata/LundFinalData_310822/song_mds_formatted.xlsx") 
attach(song_mds)

## Prepare data 
song_mds$Population2 <- factor(song_mds$Population2, levels = c("Dutch", "Swedish", "Hybrid", "Dutch egg"))
song_mds$Rec_year <- as.factor(song_mds$Rec_year)
song_mds$Age <- as.numeric(song_mds$Age)

##Create dataset without half Dutch-half Swedish 'hybrid' males. 
##This is the dataset that will be used for further analysis of translocated males 'Dutch egg' birds
song_SND <- song_mds %>%
  filter(Population2 != 'Hybrid')

## Subset the swedish and dutch songs 
data_subsong_SN <-  subset(song_SND, Population2 %in% c("Swedish", "Dutch"))
data_subsong_SN <- droplevels(data_subsong_SN)

## Need CV=TRUE to get to the confusion matrix 
lda_SN <- lda(Population2 ~ pc_1_value + pc_2_value + pc_3_value +
                pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+
                pc_10_value,
              data = data_subsong_SN, CV = TRUE)

## Assess the classification accuracy 
ctsongSN <- table(data_subsong_SN$Population2, lda_SN$class)
ctsongSN # Confusion matrix 
diag(prop.table(ctsongSN, 1)) #accuracy of LDA for each experimental group

## accuracy of lda model over all categories 
sum(diag(ctsongSN))/sum(ctsongSN)

## Get a new dataset with the LD values for all the Swedish and Dutch songs ##
#### Project a new data set using the previous LD functions. First, you have to re-run the LDA with CV = FALSE
lda_SN <- lda(formula = Population2 ~ pc_1_value + pc_2_value + pc_3_value +
                pc_4_value + pc_5_value + pc_6_value+pc_7_value+pc_8_value+pc_9_value+
                pc_10_value, data = data_subsong_SN, CV = FALSE)
lda_SN

predictSN.ld <- predict(object = lda_SN, newdata = data_subsong_SN) ##Prediction to get the LD scores for all Swedish and Dutch songs
combinedSN <- cbind(data_subsong_SN, predictSN.ld) ##Combine the original dataset with the posterior probabilities and LD scores

##Create  the testing dataset for translocated males
data_subsong_D <-  subset(song_SND, Population2 %in% c("Dutch egg"))
data_subsong_D <- droplevels(data_subsong_D)

## Projection of translocated males songs onto Dutch and Swedish songs
D_SN.ld <- predict(object = lda_SN, newdata = data_subsong_D) ##Predict posterior probabilities for translocated males' songs
combined_D <- cbind(data_subsong_D, D_SN.ld) ##Combine the dataset to get the LD and classification scores for all songs of translocted males

## Final dataset: Combine dataset of posterior probabilties of translocated males with Dutch and Swedish birds
combined_SND <- rbind(combinedSN, combined_D)

## Summary statistics:
## Get count and percentage for every subgroup:
combined_SND%>%
  group_by(Population2, class)%>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

## Calculate effect size between the different experimental groups:
#Create vectors with variable of interest from each experimental group
DE <- combined_SND %>% filter(Population2 == 'Dutch egg')
DE <- DE$LD1

Se <- combined_SND %>% filter(Population2 == 'Swedish')
Se <- Se$LD1

Du <- combined_SND %>% filter(Population2 == 'Dutch')
Du <- Du$LD1

#Check effect size
cohen.d(Se, DE) #medium: Between Swedish and Dutch egg birds
cohen.d(Se, Du) #large: Between Swedish and Dutch birds
cohen.d(DE, Du) #large: Between Dutch and Dutch egg birds

## Linear regression of experimental group and Age on LD scores of all birds:
model <- glmmTMB(LD1 ~ Population2  + Age +(1|Individual), data =combined_SND, dispformula = ~Population2,family = gaussian())
car::Anova(model) ##Likelihood ratio test
emmeans(model, list(pairwise~Population2)) ##Post-hoc tests

## Check model diagnostics using DHARMA package
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
plot(simulationOutput)
hist(residuals(model)) 

##Check random effect of indvidual
plot_model(model, type = "re")
