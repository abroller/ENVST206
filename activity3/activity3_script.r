## Activity 3 ##

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
