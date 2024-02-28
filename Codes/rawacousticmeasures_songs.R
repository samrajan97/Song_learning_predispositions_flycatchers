rm(list= ls()) #Clear environment

#Load necessary packages
library(readxl) # importing data
library(dplyr) #Cleaning data
library(tidyverse) #Cleaning data
library (lme4) #models
library(emmeans) #post hoc
library(glmmTMB) #non linear models
library(DHARMa) #residuals check
library(MASS) #for pc
library(corrplot) #correlation matrix


song_acoustics <- read_excel("~/rawsong_acoustics.xlsx") 
attach(song_acoustics)

## Convert columns to factors
str(song_acoustics)
song_acoustics$Experimental_group <- factor(song_acoustics$Experimental_group, levels = c("Dutch", "Swedish", "Hybrid", "Dutch egg"))
song_acoustics$Individual<- as.factor(song_acoustics$Individual)
song_acoustics$Song<- as.factor(song_acoustics$Song)

##Change level of factor so Dutch egg is in the middle
song_acoustics$Experimental_group <- factor(song_acoustics$Experimental_group, levels = c('Dutch', 'Dutch egg', 'Swedish'))

##1) Variation in temporal variables

#Multiply tempo by 1000 so differences are more easily visible
song_acoustics<- song_acoustics%>%
  mutate(tempo = tempo*1000)

##Create a dataset for plotting, with the mean values per individual
plotting_song<- song_acoustics %>%
  group_by(Experimental_group, Individual) %>%
  summarise(n_syllables = mean(n_syllables), Syllable_length = mean(syllable_length), 
            syllable_gap = mean(syllable_gap), song_length = mean(song_length),
            tempo = mean(tempo))

## Supplementary Table 4: mean values per population for all variable
TableS4_tempo <- song_acoustics %>%
  group_by(Experimental_group) %>%
  summarise(n_syllables = mean(n_syllables), Syllable_length = mean(syllable_length), 
            syllable_gap = mean(syllable_gap), song_length = mean(song_length),
            tempo = mean(tempo))

# 1. Number of syllables
m_nsyllables <- glmmTMB(n_syllables ~ Experimental_group + (1|Individual), dispformula = ~Experimental_group, family = poisson, data = song_acoustics)
car::Anova(m_nsyllables)  #overall variation
emmeans(m_nsyllables, list(pairwise ~ Experimental_group)) #post hoc

## Plot number of syllables
n_syllables <- ggplot(plotting_song, aes(x= Experimental_group, y = n_syllables, colour = NULL, fill = Experimental_group)) + 
  geom_boxplot(aes(colour = Experimental_group, alpha = 0.5)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D","#CC7CFF", "#00BFC4")) +
  geom_jitter(aes(colour = Experimental_group, size = 0.1),  width = 0.1, height = 0.2, alpha = 0.5) 
n_syllables

# 2. Syllable length
m_syllength <- glmmTMB(syllable_length ~ Experimental_group + (1|Individual), dispformula = ~Experimental_group, data = song_acoustics)
car::Anova(m_syllength)  #overall variation
emmeans(m_syllength, list(pairwise ~ Experimental_group)) #post hoc

## Plot syllable length
Syllable_length <- ggplot(plotting_song, aes(x= Experimental_group, y = Syllable_length, colour = NULL, fill = Experimental_group)) + 
  geom_boxplot(aes(colour = Experimental_group, alpha = 0.5)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D","#CC7CFF", "#00BFC4")) +
  geom_jitter(aes(colour = Experimental_group, size = 0.1),  width = 0.1, height = 0.2, alpha = 0.5) 
Syllable_length

# 3. Between syl gap
m_syllablegap <- glmmTMB(syllable_gap ~ Experimental_group + (1|Individual), dispformula = ~Experimental_group, data = song_acoustics)
car::Anova(m_syllablegap)  #overall variation

## Plot intersyllable gap length
Between_sylgap <- ggplot(plotting_song, aes(x= Experimental_group, y = syllable_gap, colour = NULL, fill = Experimental_group)) + 
  geom_boxplot(aes(colour = Experimental_group, alpha = 0.5)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D","#CC7CFF", "#00BFC4")) +
  geom_jitter(aes(colour = Experimental_group, size = 0.1),  width = 0.1, height = 0.2, alpha = 0.5) 
Between_sylgap

# 4. Song length
m_song <- glmmTMB(song_length ~ Experimental_group + (1|Individual), dispformula = ~Experimental_group, family = nbinom2, data = song_acoustics)
car::Anova(m_song) #overall variation

## Plot song length
song_length <- ggplot(plotting_song, aes(x= Experimental_group, y = song_length, colour = NULL, fill = Experimental_group)) + 
  geom_boxplot(aes(colour = Experimental_group, alpha = 0.5)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D","#CC7CFF", "#00BFC4")) +
  geom_jitter(aes(colour = Experimental_group, size = 0.1),  width = 0.1, height = 0.2, alpha = 0.5) 
song_length

# 5. Tempo
m_tempo <- glmmTMB(tempo ~ Experimental_group + (1|Individual), dispformula = ~Experimental_group, data = song_acoustics)
car::Anova(m_tempo)  #overall variation
emmeans(m_tempo, list(pairwise ~ Experimental_group)) #post hoc

## Plot tempo
tempo <- ggplot(plotting_song, aes(x= Experimental_group, y = tempo, colour = NULL, fill = Experimental_group)) + 
  geom_boxplot(aes(colour = Experimental_group, alpha = 0.5)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") +  
  scale_fill_manual(values = c("#F8766D", "#CC7CFF",  "#00BFC4")) +  
  scale_colour_manual(values = c("#F8766D","#CC7CFF", "#00BFC4")) +
  geom_jitter(aes(colour = Experimental_group, size = 0.1),  width = 0.1, height = 0.2, alpha = 0.5) 
tempo

# 2) Variation in spectral variables

##Make column with bandwidth measures for each song
song_acoustics <- song_acoustics %>%
  mutate(bandwidth = Overall_Max_Freq - Overall_Min_Freq)

##Check correlation of all the spectral variables:
## Take mean frequency change mean, mean freq change variance and mean freq change min
cor_matrix_spectral <- cor(song_acoustics[c(11:19)])
corrplot(cor_matrix_spectral, type="lower")

# Run PCA on the correlation matrix, use scale and center = TRUE
res.pca <- prcomp(song_acoustics[c(11:19)], center = TRUE, scale = TRUE)

summary(res.pca) #proportion of variance explained by each PC and eigenvalues

res.pca #Loadings of the different variables on the PC

# Extract the first nine principal components
first_pc <- res.pca$x[, 1]
second_pc <- res.pca$x[, 2]
third_pc <- res.pca$x[, 3]
fourth_pc <- res.pca$x[, 4]
fifth_pc <- res.pca$x[, 5]
sixth_pc <- res.pca$x[, 6]
seventh_pc <- res.pca$x[, 7]
eighth_pc <- res.pca$x[, 8]
ninth_pc <- res.pca$x[, 9]


# Combine the PCs with categorical variables
combined_pcdata <- data.frame(
  Experimental_group = as.factor(song_acoustics$Experimental_group),
  Individual = as.factor(song_acoustics$Individual),
  PC1 = first_pc, PC2 = second_pc, PC3 = third_pc, PC4 = fourth_pc, PC5 = fifth_pc,
  PC6 = sixth_pc, PC7 = seventh_pc , PC8=  eighth_pc ,PC9= ninth_pc)
  

# Table S4: Spectral variables
TableS4_pc <- combined_pcdata %>%
  group_by(Experimental_group) %>%
  summarise(mean(PC1), mean(PC2), mean(PC3), mean(PC4), mean(PC5), mean(PC6), 
            mean(PC7), mean(PC8), mean(PC9))

# PC1: mixed model
model_pc1 <- glmmTMB(PC1 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc1)
emmeans(model_pc1, list(pairwise ~ Experimental_group))

# PC2: mixed model
model_pc2 <- glmmTMB(PC2 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc2)
emmeans(model_pc2, list(pairwise ~ Experimental_group))

# PC3: mixed model
model_pc3 <- glmmTMB(PC3 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc3)
emmeans(model_pc3, list(pairwise ~ Experimental_group))

# PC4: mixed model
model_pc4 <- glmmTMB(PC4 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc4)
emmeans(model_pc4, list(pairwise ~ Experimental_group))

# PC5: mixed model
model_pc5 <- glmmTMB(PC5 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc5)

# PC6: mixed model
model_pc6 <- glmmTMB(PC6 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc6)
emmeans(model_pc6, list(pairwise ~ Experimental_group))

# PC7: mixed model
model_pc7 <- glmmTMB(PC7 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc7)

# PC8: mixed model
model_pc8 <- glmmTMB(PC8 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc8)
emmeans(model_pc8, list(pairwise ~ Experimental_group))

# PC9: mixed model
model_pc9 <- glmmTMB(PC9 ~ Experimental_group + (1|Individual), data = combined_pcdata)
car::Anova(model_pc9)
