####Data####
Dorwin <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\FinalProject\\OC_DischargeD.csv")
Spencer <- read.csv("C:\\Users\\Robbie Rioux\\OneDrive\\Documents\\College\\Senior Year\\FA2020\\Env Data Science\\FinalProject\\OC_DischargeS.csv")

####Analysis####
#Dorwin mean and sd
apply(Dorwin[Dorwin$Location == "Dorwin", 2:13],2,"mean")
apply(Dorwin[Dorwin$Location == "Dorwin", 2:13],2,"sd")

#plot
ggplot(data=Dorwin, aes(x=ï..Year, y=Oct))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Average discharge for October (ft3/s")

