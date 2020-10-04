####Data####
datW <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 2\\a02\\noaa2011124.csv")

####Fundamentals of plotting data#####
#read in weather station file from the data folder (see Data)
#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS

#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME","year","totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#make a new dataframe
pr <- precip[precip$ncount >=364, ]
#look at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

####plots####

#make a plot of california precip (scatter)
plot(ca$year, ca$totalP)

#make a plot of california precip (add lines)
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")

#make a plot of california precip (lines, but better)
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
#add y axis
#arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(200,800, by=200), las=2 )

#make a Morrisville plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
#add y axis
axis(2, seq(200,800, by=200), las=2 )
#add ny
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#Fixing axes on Morrisville plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add ny
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#NY/CA line graph with proper axes and legend
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

####question 3####

datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))

tmax <- aggregate(datW$TMAX, by=list(datW$NAME,datW$year), FUN="mean", na.rm=TRUE)
tmax <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)
colnames(tmax) <- c("NAME","year","Tmax")
tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
tm <- tmax[tmax$ncount >=364, ]

ny_tm <- tm[tm$NAME == nameS[5], ]
nd_tm <- tm[tm$NAME == nameS[3], ]

#NY/ND line graph with proper axes and legend
#add north dakota
plot(nd_tm$year, nd_tm$Tmax,
     type = "b",
     pch = 19,
     ylab = "Annual max temp (C)",
     xlab = "Year", 
     yaxt = "n",
     xaxt = "n",
     ylim =c(8, 16),
     xlim = c(1930, 2020))
#add y axis
axis(2, seq(0,20, by=2), las=2 )
axis(1, seq(1930, 2020, by=5), las=1)

#add new york
points(ny_tm$year, ny_tm$Tmax,
       type = "b",
       pch = 19,
       col="tomato3")

#add legend
legend("topleft", #position
       c("North Dakota", "New York"),
       col= c("black", "tomato3"),
       pch=19, 
       lwd=1, 
       bty="n")

####Using packages in R####
#ggplot
install.packages("ggplot2")
library(ggplot2)

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation") #make axis labels

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

#question 5
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("steelblue4","lightgoldenrod", "tan3", "sienna1","skyblue"))

####Exploring different visualizations in ggplot2####
ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

#specify date format
#%Y means a four number year 
#- indicates that the date uses dashes to seperate
#%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#Aberdeen
subAb <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
subAb$DATE <- as.Date(subAb$DATE,"%Y-%m-%d")

ggplot(data=subAb, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=subAb, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#Washington

ggplot(data = datW, aes(x=NAME, y=TMIN))+ 
  geom_violin(fill=rgb(0.933,0.953,0.98))+ 
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+
  theme_classic()
dev.off()
sub <- datW[datW$NAME == nameS[1] & datW$ year > 1999,]
sub$year <- as.factor(sub$year)
#make violin plot
ggplot(data = sub, aes(x=year, y=TMIN))+ 
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ 
  theme_classic()+
  labs(x="Year", y="TMIN (C)")

