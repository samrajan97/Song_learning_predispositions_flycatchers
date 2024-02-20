## Clear memory
rm(list = ls())

## Set up packages ##
library(readxl) #import excel files
library(dplyr) #Data formatting
library(tidyverse) #Data formatting
library(MASS) #for LDA analysis
library(ggplot2) #plotting
library(ggridges) #For overlapping density plots
library(lme4) #Model for hybrid parentage
library(DHARMa) #Model diagnostics
library(sjPlot) #Plotting of random effects
library(pwr) #Power analysis


## Import dataset 
song_mds <- read_excel("~/Desktop/PF_prelimdata/LundFinalData_310822/data_song_mds.xlsx")
attach(song_mds)

## Prepare data 
song_mds$Population2 <- factor(song_mds$Population2, levels = c("Dutch", "Swedish", "Hybrid", "Dutch egg"))
song_mds$Rec_year <- as.factor(song_mds$Rec_year)
song_mds$Age <- as.numeric(song_mds$Age)

## 1) LINEAR DISCRIMINANT ANALYSIS 

## Subset the swedish and dutch songs 
data_subsong_SN <-  subset(song_mds, Population2 %in% c("Swedish", "Dutch"))
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

##Create  the testing dataset for translocated males and hybrid males
data_subsong_DH <-  subset(song_mds, Population2 %in% c("Dutch egg", "Hybrid"))
data_subsong_DH <- droplevels(data_subsong_DH)

## Projection of translocated males and hybrid songs onto Dutch and Swedish songs
DH_SN.ld <- predict(object = lda_SN, newdata = data_subsong_DH) ##Predict posterior probabilities for translocated males' songs
combined_DH <- cbind(data_subsong_DH, DH_SN.ld) ##Combine the dataset to get the LD and classification scores for all songs of translocted males

## Assess the classification accuracy of Dutch egg and hybrid songs
ctsongDH <- table(data_subsong_DH$Population2, DH_SN.ld$class)
ctsongDH # Confusion matrix

## Final dataset: Combine dataset of posterior probabilties of translocated males  and hybrids with Dutch and Swedish birds
combined_SNDH <- rbind(combinedSN, combined_DH)

## Linear regression of experimental group and Age on LD scores of all birds:
model <- glmmTMB(LD1 ~ Population2  + Age +(1|Individual), 
                 data =combined_SNDH, dispformula = ~Population2,family = gaussian())
car::Anova(model) ##Likelihood ratio test
emmeans(model, list(pairwise~Population2)) ##Post-hoc tests

## Check model diagnostics using DHARMA package
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
plot(simulationOutput) #All looks good
hist(residuals(model)) 

##power analysis: not the way to do it,but code is here
pwr.t2n.test(n1=38, d = 0.23, sig.level = 0.05, power = 0.2)

## 2) REPLICATE SUPPLEMENTARY FIGURE 3

## calculate LD scores per individual
combined_SNDHind <- combined_SNDH %>%
  group_by(Population2,Individual) %>%
  summarise(LD1 = mean(LD1))
combined_SNDHind$Population2 <- factor(combined_SNHind$Population2, levels = c( 'Dutch', 'Dutch egg', 'Hybrid',  'Swedish'))

densityplotSNDH <- ggplot(combined_SNDHind, aes(x = LD1, y = Population2, fill = Population2, colour = Population2), alpha = 0.8) +
  geom_density_ridges(scale = 0.9, quantile_lines = TRUE, quantiles = 2, lwd = 2, alpha = 0.6)+ #jittered_points = TRUE, point_size = 9, 
  #position = position_points_jitter(width = 0.05, height = 0),
  #point_shape = 'o', point_size = 3, point_alpha = 1, alpha = 0.7) +
  geom_jitter(aes(colour = Population2),  width = 0.05, height = 0, alpha = 0.9, pch=21,size = 9, lwd = 1) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
  scale_fill_manual(values = c("#F8766D", "#CC7CFF", "#7CAE00", "#00BFC4")) + 
  scale_colour_manual(values = c('#ac1308', '#7000b7','#456200', '#005f62')) + 
  theme(legend.position="none") +
  labs(x = "LD song scores", y = "") + 
  theme(axis.text = element_text(size = 25))  + 
  theme(axis.title = element_text(size = 30), axis.title.x = element_text(vjust =-1)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, .2))) +
  theme(axis.ticks.length=unit(0.3,"cm")) 
densityplotSNDH

## 3) ROLE OF HYBRID PARENTAGE

## Create a dataset with only hybrid and Swedish birds
##This is to check whether the song distribution of hybrids with a Dutch mother or a father is different from each other and the local Swedish birds
hybridswedish <- combined_SNDH %>%
  filter(Population2 != 'Dutch egg') %>%
  filter(Population2 != 'Dutch') 

hybridswedish$Population <- droplevels(hybridswedish$Population)
hybridswedish$Population <- recode_factor(hybridswedish$Population,
                                          "Swedish_nonlocal" = "Swedish", "Swedish_local" = "Swedish",  
                                          "Hybrid_dutchmother" = "Hybrid with Dutch mother", 
                                          "Hybrid_dutchfather" = "Hybrid with Dutch father")

#Create model to check effect of parentage
model <- lmer(LD1 ~ Population+ (1|Individual), data = hybridswedish)
car::Anova(model)

## Check model diagnostics using DHARMA package
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
plot(simulationOutput)




