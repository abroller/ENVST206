#activity 2

## in-class example ##
#vector of tree heights (meters)
heights <- c(30,40,20,22)
#convert to centimeters
heights_cm <- heights*100
heights_cm

## subsetting ##
#first tree height
heights[1]
#second and third tree heights
heights[2:3]

## build a matrix ##
#for info on the matrix function:
help(matrix)
#set up a matrix with 2 columns and fill in by rows
#first () is the vector of numbers to fill in the matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up a matrix that fills in by columns
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

##matrix notation [row,column]
#subset matrix to look at row 1, column 2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1, ]
#look at all values in column 2
Mat.bycol[,2]

##read in noaa weather data
datW <- read.csv("/Users/abby/Documents/ENVST206/noaa2011124.csv")
#get more info about the dataframe
str(datW)
datW$NAME <- as.factor(datW$NAME)
datW$NAME

## question 2 ##
#example character vector
vec_char <- c("a","b","c","d","e")
vec_char
#example numeric vector
vec_num <- c(1.1,1.2,1.3,1.4,1.5)
vec_num
#example integer vector
vec_int <- c(1,2,3,4,5)
vec_int
#example factor vector
vector <- c(1,4,3,5,4)
vec_fac <- as.factor(vector)
vec_fac

help(factor)

## descriptive statistics ##
#find out all unique site names
levels(datW$NAME)
#look at the mean max temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
#mean max temp for Aberdeen with na.rm to ignore NAs
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#look at standard deviation for Aberdeen
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate average daily temp, halfway btwn max and min temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
datW$TAVE
#get the mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp

#change the output column names
#MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor NAME data
datW$siteN <- as.numeric(datW$NAME)
#now you can reference NAME by number which is easier

#make a histogram for the first site
hist(datW$TAVE[datW$siteN == 1], freq=FALSE, main=paste(levels(datW$NAME)[1]), xlab="Average daily temperature (degrees C)", ylab="Relative frequency", col="grey75", border="white")

#look up list of arguments in the hist function above
help(hist)

## question 4 ##
#make a histogram for another weather station
hist(datW$TAVE[datW$siteN == 5], freq=FALSE, main=paste(levels(datW$NAME)[5]), xlab="Average daily temperature (degrees C)", ylab="Relative frequency", col="grey50", border="white")



