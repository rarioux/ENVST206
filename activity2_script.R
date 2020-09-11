#activity 2

####VECTORS####

#make a vector of tree heights in meters
heights <- c(30,41,20,22)

#convert to cm
heights_cm <- heights*100
heights_cm

#look at the first tree height
heights[1]
#look at the 2nd and 3rd tree heights
heights[2:3]

####MATRICES####
#get more info on the matrix function
help(matrix)
#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

####Dataframes####
#read in weather station file from the data folder
datW <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 2\\a02\\noaa2011124.csv")

#get more information about the dataframe
str(datW)

datW$NAME <- as.factor(datW$NAME)

####Question 2####
Character <- c('a', 'b', 'c', 'd', 'e')
Numeric <- c(1.2,4.2, 5, 9.6)
Integer <- c(1, 2, 3, 4, 5)
Factor <- factor(c('Red','Blue', 'Green', 'Yellow', 'Orange'))

####Descriptive Statistics and Histograms####

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"]) #should be NA, missing values

#look at the mean maximum temperature for Aberdeen - with na.rm argument set to true to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#next look at the standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
help(hist)


####Question 4####
hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

####probability density####
help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnrom of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#qnorm gives me the value at which all values and below equal the probability in my argument
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

####Question 5####
IncreasedAvg <- datW$TAVE + 4

hist(IncreasedAvg[datW$siteN == 1],
    freq = FALSE,
    main = paste (levels(datW$NAME)[1]),
    xlab = "Average daily temperature (C)",
    ylab = "Relative frequency",
    col = 'grey75',
    border = 'white')

#Find the probability of climate change temps to be above threshold for extreme temps
1 - pnorm(18.51026,
          mean(IncreasedAvg[datW$siteN == 1],na.rm = TRUE),
          sd(IncreasedAvg[datW$siteN == 1],na.rm = TRUE))

####Question 6####
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average Daily Precip", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

####Question 7####
AnnualPrecip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
colnames(AnnualPrecip) <- c("NAME","YEAR", "Annual Precipitation")
AnnualPrecip

AnnualPrecip$NAME <- as.numeric(AnnualPrecip$NAME)
AnnualPrecip$NAME

####Question 8####

#Aberdeen
hist(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average Annual Precip", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#Mandan
hist(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average Annual Precip", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

####Question 9####

#Aberdeen
pnorm(700,
      mean(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 1],na.rm=TRUE),
      sd(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 1],na.rm=TRUE))
#Mandan
pnorm(700,
      mean(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 3],na.rm=TRUE),
      sd(AnnualPrecip$`Annual Precipitation`[AnnualPrecip$NAME == 3],na.rm=TRUE))
