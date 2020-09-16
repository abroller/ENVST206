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



