####Data####
Dorwin <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\FinalProject\\OC_DischargeD.csv")
Spencer <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\FinalProject\\OC_DischargeS.csv")
Combined <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\FinalProject\\OC_DischargeC.csv")

####Just looking at the data####
#Dorwin mean and sd
apply(Dorwin[Dorwin$Location == "Dorwin", 2:13],2,"mean")
apply(Dorwin[Dorwin$Location == "Dorwin", 2:13],2,"sd")

apply(Spencer[Spencer$Location == "Spencer", 2:13],2,"mean")
apply(Spencer[Spencer$Location == "Spencer", 2:13],2,"sd")

#example plots#
plot(Dorwin$�..Year, Dorwin$Nov, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average Dischrage in Nov",
     xlab =  "Year")
plot(Spencer$�..Year, Spencer$Nov, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average Dischrage in Nov",
     xlab =  "Year")


#begin the linear regressions!#
####Linear Regressions (for Nov)####

#set up regression
DorN.mod <- lm(Dorwin$Nov ~ Dorwin$�..Year)
SpeN.mod <- lm(Spencer$Nov ~ Spencer$�..Year)
#get standarized residuals
DorN.res <- rstandard(DorN.mod)
SpeN.res <- rstandard(SpeN.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorN.res)
#add qq line
qqline(DorN.res)

qqnorm(SpeN.res)
qqline(SpeN.res)

shapiro.test(DorN.res)
shapiro.test(SpeN.res)

#make residual plot
plot(Dorwin$�..Year, DorN.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeN.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorN.mod)
summary(SpeN.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Nov, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorN.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Nov, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeN.mod, lwd=2)

####Linear Regressions (for Dec)####

#set up regression
DorD.mod <- lm(Dorwin$Dec ~ Dorwin$�..Year)
SpeD.mod <- lm(Spencer$Dec ~ Spencer$�..Year)
#get standarized residuals
DorD.res <- rstandard(DorD.mod)
SpeD.res <- rstandard(SpeD.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorD.res)
#add qq line
qqline(DorD.res)

qqnorm(SpeD.res)
qqline(SpeD.res)

shapiro.test(DorD.res)
shapiro.test(SpeD.res)

#make residual plot
plot(Dorwin$�..Year, DorD.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeD.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorD.mod)
summary(SpeD.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Dec, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorN.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Dec, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeN.mod, lwd=2)

####Linear Regressions (for Jan)####

#set up regression
DorJ.mod <- lm(Dorwin$Jan ~ Dorwin$�..Year)
SpeJ.mod <- lm(Spencer$Jan ~ Spencer$�..Year)
#get standarized residuals
DorJ.res <- rstandard(DorJ.mod)
SpeJ.res <- rstandard(SpeJ.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorJ.res)
#add qq line
qqline(DorJ.res)

qqnorm(SpeJ.res)
qqline(SpeJ.res)

shapiro.test(DorJ.res)
shapiro.test(SpeJ.res)

#make residual plot
plot(Dorwin$�..Year, DorJ.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeJ.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorJ.mod)
summary(SpeJ.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Jan, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorJ.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Jan, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeJ.mod, lwd=2)

####Linear Regressions (for Feb)####

#set up regression
DorF.mod <- lm(Dorwin$Feb ~ Dorwin$�..Year)
SpeF.mod <- lm(Spencer$Feb ~ Spencer$�..Year)
#get standarized residuals
DorF.res <- rstandard(DorF.mod)
SpeF.res <- rstandard(SpeF.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorF.res)
#add qq line
qqline(DorF.res)

qqnorm(SpeF.res)
qqline(SpeF.res)

shapiro.test(DorF.res)
shapiro.test(SpeF.res)

#make residual plot
plot(Dorwin$�..Year, DorF.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeF.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorF.mod)
summary(SpeF.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Feb, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorF.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Feb, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeF.mod, lwd=2)
####Linear Regressions (for Mar)####

#set up regression
DorM.mod <- lm(Dorwin$Mar ~ Dorwin$�..Year)
SpeM.mod <- lm(Spencer$Mar ~ Spencer$�..Year)
#get standarized residuals
DorM.res <- rstandard(DorM.mod)
SpeM.res <- rstandard(SpeM.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorM.res)
#add qq line
qqline(DorM.res)

qqnorm(SpeM.res)
qqline(SpeM.res)

shapiro.test(DorM.res)
shapiro.test(SpeM.res)

#make residual plot
plot(Dorwin$�..Year, DorM.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeM.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorM.mod)
summary(SpeM.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Mar, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorM.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Mar, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeM.mod, lwd=2)
####Linear Regressions (for Apr)####

#set up regression
DorA.mod <- lm(Dorwin$Apr ~ Dorwin$�..Year)
SpeA.mod <- lm(Spencer$Apr ~ Spencer$�..Year)
#get standarized residuals
DorA.res <- rstandard(DorA.mod)
SpeA.res <- rstandard(SpeA.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorA.res)
#add qq line
qqline(DorA.res)

qqnorm(SpeA.res)
qqline(SpeA.res)

shapiro.test(DorA.res)
shapiro.test(SpeA.res)

#make residual plot
plot(Dorwin$�..Year, DorA.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeA.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorA.mod)
summary(SpeA.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Apr, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorA.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Apr, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeA.mod, lwd=2)
####Linear Regressions (for May)####

#set up regression
DorMay.mod <- lm(Dorwin$May ~ Dorwin$�..Year)
SpeMay.mod <- lm(Spencer$May ~ Spencer$�..Year)
#get standarized residuals
DorMay.res <- rstandard(DorMay.mod)
SpeMay.res <- rstandard(SpeMay.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorMay.res)
#add qq line
qqline(DorMay.res)

qqnorm(SpeMay.res)
qqline(SpeMay.res)

shapiro.test(DorMay.res)
shapiro.test(SpeMay.res)

#make residual plot
plot(Dorwin$�..Year, DorMay.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeMay.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorMay.mod)
summary(SpeMay.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$May, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorMay.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$May, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeMay.mod, lwd=2)

####Linear Regressions (for Jun)####

#set up regression
DorJun.mod <- lm(Dorwin$Jun ~ Dorwin$�..Year)
SpeJun.mod <- lm(Spencer$Jun ~ Spencer$�..Year)
#get standarized residuals
DorJun.res <- rstandard(DorJun.mod)
SpeJun.res <- rstandard(SpeJun.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorJun.res)
#add qq line
qqline(DorJun.res)

qqnorm(SpeJun.res)
qqline(SpeJun.res)

shapiro.test(DorJun.res)
shapiro.test(SpeJun.res)

#make residual plot
plot(Dorwin$�..Year, DorJun.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeJun.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorJun.mod)
summary(SpeJun.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Jun, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorJun.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Jun, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeJun.mod, lwd=2)

####Linear Regressions (for Jul)####

#set up regression
DorJul.mod <- lm(Dorwin$Jul ~ Dorwin$�..Year)
SpeJul.mod <- lm(Spencer$Jul ~ Spencer$�..Year)
#get standarized residuals
DorJul.res <- rstandard(DorJul.mod)
SpeJul.res <- rstandard(SpeJul.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorJul.res)
#add qq line
qqline(DorJul.res)

qqnorm(SpeJul.res)
qqline(SpeJul.res)

shapiro.test(DorJul.res)
shapiro.test(SpeJul.res)

#make residual plot
plot(Dorwin$�..Year, DorJul.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeJul.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorJul.mod)
summary(SpeJul.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Jul, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorJul.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Jul, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeJul.mod, lwd=2)

####Linear Regressions (for Aug)####

#set up regression
DorAug.mod <- lm(Dorwin$Aug ~ Dorwin$�..Year)
SpeAug.mod <- lm(Spencer$Aug ~ Spencer$�..Year)
#get standarized residuals
DorAug.res <- rstandard(DorAug.mod)
SpeAug.res <- rstandard(SpeAug.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorAug.res)
#add qq line
qqline(DorAug.res)

qqnorm(SpeAug.res)
qqline(SpeAug.res)

shapiro.test(DorAug.res)
shapiro.test(SpeAug.res)

#make residual plot
plot(Dorwin$�..Year, DorAug.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeAug.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorAug.mod)
summary(SpeAug.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Aug, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorAug.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Aug, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeAug.mod, lwd=2)

####Linear Regressions (for Sep)####

#set up regression
DorS.mod <- lm(Dorwin$Sep ~ Dorwin$�..Year)
SpeS.mod <- lm(Spencer$Sep ~ Spencer$�..Year)
#get standarized residuals
DorS.res <- rstandard(DorS.mod)
SpeS.res <- rstandard(SpeS.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorS.res)
#add qq line
qqline(DorS.res)

qqnorm(SpeS.res)
qqline(SpeS.res)

shapiro.test(DorS.res)
shapiro.test(SpeS.res)

#make residual plot
plot(Dorwin$�..Year, DorS.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeS.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorS.mod)
summary(SpeS.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Sep, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorS.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Sep, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeS.mod, lwd=2)

####Linear Regressions (for Oct)####

#set up regression
DorO.mod <- lm(Dorwin$Oct ~ Dorwin$�..Year)
SpeO.mod <- lm(Spencer$Oct ~ Spencer$�..Year)
#get standarized residuals
DorO.res <- rstandard(DorO.mod)
SpeO.res <- rstandard(SpeO.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorO.res)
#add qq line
qqline(DorO.res)

qqnorm(SpeO.res)
qqline(SpeO.res)

shapiro.test(DorO.res)
shapiro.test(SpeO.res)

#make residual plot
plot(Dorwin$�..Year, DorO.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeO.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorO.mod)
summary(SpeO.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Oct, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorO.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Oct, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeO.mod, lwd=2)

####Linear Regressions (Annual Avg.)####

#set up regression
DorAvg.mod <- lm(Dorwin$Avg ~ Dorwin$�..Year)
SpeAvg.mod <- lm(Spencer$Avg ~ Spencer$�..Year)
#get standarized residuals
DorAvg.res <- rstandard(DorAvg.mod)
SpeAvg.res <- rstandard(SpeAvg.mod)


#Checking assumptions
#set up qq plot
qqnorm(DorAvg.res)
#add qq line
qqline(DorAvg.res)

qqnorm(SpeAvg.res)
qqline(SpeAvg.res)

shapiro.test(DorAvg.res)
shapiro.test(SpeAvg.res)

#make residual plot
plot(Dorwin$�..Year, DorAvg.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

plot(Spencer$�..Year, SpeAvg.res, 
     xlab = "year", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)


#Interpreting results
summary(DorAvg.mod)
summary(SpeAvg.mod)

#Graphs
#make plot of year and average discharge
#Dorwin
plot(Dorwin$�..Year, Dorwin$Avg, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(DorAvg.mod, lwd=2)

#Spencer
plot(Spencer$�..Year, Spencer$Avg, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Average discharge (ft3/s)",
     xlab =  "year")
#add regression line
#make line width thicker
abline(SpeAvg.mod, lwd=2)








