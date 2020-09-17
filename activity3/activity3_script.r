## Activity 3 ##

##in class example
ch4 <- read.csv("/Users/abby/Documents/ENVST206/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

#make a box plot
plot(ch4$CH4_Flux ~ ch4$herbivory)

#test normality
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#test variance
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#t test
t.test(ch4$CH4_Flux ~ ch4$herbivory)


##starting activity 3
#read in data
ch4  <- read.csv("/Users/abby/Documents/ENVST206/lemming_herbivory.csv")
ch4$herbivory <- as.factor(ch4$herbivory)

#make a box plot
# ~ means a function, CH4_Flux depends on herbivory
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", ylab ="CH4 fluxes (mgC m -2 day -1)")

## check normality assumption in each treatment group
#p-value >.05  means it is normal
#shapiro-wilk test on grazing plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
#shapiro-wilk test on grazing exclusion plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

## check equal variance assumption
#p-value >.05 means variances are equal
#bartlett test 
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

## since both assumptions are met, conduct a t-test
t.test(ch4$CH4_Flux ~ ch4$herbivory)

help(t.test)
t.test(ch4$CH4_Flux ~ ch4$herbivory, alternative = "greater")

## ANOVA ##
#read in insect data
datI <- read.csv("/Users/abby/Documents/ENVST206/insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName) 
datI$nameN <- as.numeric(datI$urbanName)

#question 4: test assumptions 
#check normality
shapiro.test(datI$Richness[datI$nameN == 1])
shapiro.test(datI$Richness[datI$nameN == 2])
shapiro.test(datI$Richness[datI$nameN == 3])
shapiro.test(datI$Richness[datI$nameN == 4])
#check equal variance
bartlett.test(datI$Richness ~ datI$urbanName)
bartlett.test(datI$Richness ~ datI$nameN)

#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#display ANOVA table
summary(in.aov)

#posthoc test: Tukey HSD
tukeyT <-TukeyHSD(in.aov)
#view results
tukeyT

#make a plot, make axes labels smaller than usual so they fit
plot(tukeyT, cex.axis=.75)

#means of each group
tapply(datI$Richness, datI$urbanName, "mean")

## chi-squared goodness of fit test ##

#set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow=TRUE)
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increasing")
species

#make a mosaic plot to visualize proportions
mosaicplot(species, xlab="population status", ylab="legal protection", main="Legal protection impacts on populations")

#conduct a chi-squared test 
#to determine if there is an impact of legal protections on population
chisq.test(species)

