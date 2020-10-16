####Package Install####
install.packages(c("sp","rgdal","dplyr"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

####Reading in vector data####
#read in shapefiles
#readOGR in rgdal does this
#glaciers in 1966
g1966 <- readOGR("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 6\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

#glaciers in 2015
g2015 <- readOGR("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\Activities\\Activity 6\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

str(g2015)

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME


#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

####Vector data analysis: glacier retreat####
#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name

gAll <- full_join(gdf66,gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

#question 7 - make a scatter plot
ggplot(data = gAll, aes(x = area66, y= gdiff) )+ #data for plot
  geom_point()+ #make points at data point
  labs(x="Area (km sqaured", y="percent change")+ #make axis labels
  theme_classic() #change plot theme

#join data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
#use spplot to shade polygons based on the % change of labels
#first argument is the spatial object
#second is the column in of data to display with the different colors
#add a title using main
#col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")

#look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")

#Question 8
mean(gAll$gdiff)
sd(gAll$gdiff)
max(gAll$gdiff)
min(gAll$gdiff)

#Question 9
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Boulder Glacier retreat 1966 - 2015", col="slategray")

boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder15, main = "Boulder Glacier in 2015", col="darkslategray1", add = TRUE)

legend("topleft", #position
       c("1966", "2015"),
       col= c("slategray", "darkslategray1"),
       pch=15, 
       bty="n")



pump66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
plot(pump66, main = "Pumpelly Glacier retreat 1966 - 2015", col="slategray")

pump15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]
plot(pump15, main = "Pumpelly Glacier in 2015", col="darkslategray1", add = TRUE)

legend("topleft", #position
       c("1966", "2015"),
       col= c("slategray", "darkslategray1"),
       pch=15, 
       bty="n")







