####Data####
datB <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 4\\a04\\beaver_dam.csv")
head(datB)

pheno <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 4\\a04\\red_maple_pheno.csv")
head(pheno)

####Scatter Plot/ Regression####
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standarized residuals
dam.res <- rstandard(dam.mod)

#set up qq plot
qqnorm(dam.res)
#add qq line
qqline(dam.res)

shapiro.test(dam.res)

#make residual plot
plot(datB$dams.n, dam.res, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

#interpreting results
summary(dam.mod )

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
#add regression line
#make line width thicker
abline(dam.mod, lwd=2)

####Multiple linear regression####

#For data table see Data above#

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

#Question 3
par(mfrow=c(1,4))
plot(pheno$Lat,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude")

plot(pheno$elev,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation")

plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

pheno$siteDesc <- as.factor(pheno$siteDesc)
plot(pheno$doy ~ pheno$siteDesc)


dev.off()

#Continuing on

plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
pheno$urID

mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)

#qqnorm
pheno.res <- rstandard(mlr)
qqnorm(pheno.res)
qqline(pheno.res)

plot(mlFitted, pheno.res)

summary(mlr)



