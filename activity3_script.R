####Data####
ch4 <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activiity 3\\a03\\lemming_herbivory.csv")
datI <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activiity 3\\a03\\insect_richness.csv")

####9/11 class####
ch4$herbivory <- as.factor(ch4$herbivory)

#box plot for lemming herbivory
plot(ch4$CH4_Flux ~ ch4$herbivory)

#assumption testing (good for small data sets, under a few thousand)
shapiro.test(ch4$CH4_Flux[ch4$herbivory == 'Ex'])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == 'Ctl'])

bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)

help("t.test")

####ANOVA####
#Step 1: read in data (see Data)
datI$urbanName <- as.factor(datI$urbanName)

#Assumption check
shapiro.test(datI$Richness[datI$urbanName == 'Dense'])
shapiro.test(datI$Richness[datI$urbanName == 'Developed'])
shapiro.test(datI$Richness[datI$urbanName == 'Natural'])
shapiro.test(datI$Richness[datI$urbanName == 'Suburban'])

bartlett.test(datI$Richness ~ datI$urbanName == 'Suburban')
bartlett.test(datI$Richness ~ datI$urbanName == 'Natural')
bartlett.test(datI$Richness ~ datI$urbanName == 'Developed')
bartlett.test(datI$Richness ~ datI$urbanName == 'Dense')


#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)

#run the ANOVA
in.aov <- aov(in.mod)

#print out ANOVA table
summary(in.aov)

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)



tapply(datI$Richness, datI$urbanName, "mean")

####Chi-squared####

#set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#Conduct a chi-squared test
chisq.test(species)

species
