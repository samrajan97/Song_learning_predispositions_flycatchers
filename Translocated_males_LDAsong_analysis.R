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
song_mds <- read_excel("~/song_mds_formatted.xlsx") 
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

## REPLICATE FIGURE 1B

## Aggregate LD scores per individual of each experimental group
combined_SNDind <- combined_SND %>%
  group_by(Population2,Individual) %>%
  summarise(LD1 = mean(LD1))

##Plot the density distribution of LD scores of each group
LD1_distribution<- ggplot(combined_SNDind, aes(LD1, colour = Population2, fill = Population2)) + 
  geom_density(aes(alpha = factor(Population2)), kernel = c("gaussian"), n = 1002, lwd = 2) + theme(legend.position = "none") +   scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#CC7CFF")) + theme_bw() + 
  scale_colour_manual(values = c('#ac1308',  '#005f62', '#7000b7')) +
  labs(x = "LD1 song scores", y = "Probability Density") +
  xlim(c(-3,3.5)) + ylim(c(0,1.25)) + theme(legend.position = 'none') +
  theme(axis.title = element_text(size = 45)) +
  theme(axis.text = element_text(size = 40)) + 
  scale_alpha_manual(values = c(0.9,0.9,0.7), guide= "none") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(axis.ticks.length=unit(.45, "cm"))  +
  theme(axis.title.x = element_text(vjust = -0.5), 
        axis.title.y = element_text(hjust = 0.4)) + 
  theme(axis.line = element_line(colour = 'black', size = 1.0), axis.ticks = element_line(colour = "black", size = 1.0))#+
#geom_vline(data = dummy, aes(xintercept = median, color = Population2))
LD1_distribution

#Rearrange the groups for the boxplot
combined_SNDind$Population2 <- factor(combined_SNDind$Population2, levels = c("Dutch", "Dutch egg", "Swedish"))

##Plot the boxplot of LD scores per individual 
LD1_boxplot <- ggplot(combined_SNDind, aes(y= Population2, x = LD1, colour = NULL, fill = Population2), size = 2) + 
  geom_boxplot(aes(colour = Population2, alpha = 0.5), width = 0.5, fatten = 2, lwd=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c('#ac1308', '#7000b7', '#005f62')) +
  geom_jitter(aes(colour = Population2),  width = 0.1, height = 0.2, alpha = 0.7, pch=21,size = 9, lwd = 1, stroke = 2) +
  theme(axis.title = element_text(size = 30)) +
  theme(axis.text = element_text(size = 25)) +  
  theme(axis.ticks.length=unit(.25, "cm")) +
  xlim(c(-3,3.5))
LD1_boxplot

## Combine the two plots
LD1_boxplotdist <- ggarrange(LD1_boxplot,LD1_distribution, ncol = 1, align = c("v"),
                             heights = c(2.0, 2.0))
LD1_boxplotdist
