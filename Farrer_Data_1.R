getwd() #get working directory
list.files () #list folders on computer
setwd("/Users/campbell.reid/Documents") #choose folder to work with
farrer<-read.csv("Farrerdata.csv") #choose document to work with
head(farrer) #look at headers of document
farrer$Stipul #look at this particular document
farrer[15] #look at this particular column
farrer[12,15]#return 12th row of 15th column
farrer[which(is.na(farrer$AV)),12]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$MH)),11]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$Placal)), 14]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$Stipul)), 15]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$Triwil)), 13]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$Legumeweed)),18]<-0 #replace N/As in column with "0"
farrer[248,11]<-0 #replace column and row spot with 0
farrer[117,11]<-8.31 #replace column and row spot with 8.31
farrer[95,11]<-0 #replace column and row spot with 0
farrer$Natives<-farrer$Stipul + farrer$Triwil + farrer$Placal #make new folder of sum of these columns
farrer$Invasives<-as.numeric(farrer$AV) + as.numeric(farrer$MH) #make new folder of sum of these columns, as.numeric means "read as number"
farrer$Weeds<-as.numeric(farrer$Grassweed) + as.numeric(farrer$Forbweed) + as.numeric(farrer$Legumeweed) #make new folder of sum of these columns, as.numeric means "read as number"
farrer$TotalPlantBiomass<-farrer$Natives + farrer$Weeds + farrer$Invasives #make new folder of sum of these columns
farrer$PercentNatives<-farrer$Natives/farrer$TotalPlantBiomass
farrer$PercentInvasives<-farrer$Invasives/farrer$TotalPlantBiomass
farrer$PercentWeeds<-farrer$Weeds/farrer$TotalPlantBiomass
ggplot(farrer, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")
NewDataFrame<-melt(farrer)
RainNova.Natives<-aov(PercentNatives~Rain, data=farrer)
summary(RainNova.Natives) #rain is not significant on percent natives
RainNova.Invasives<-aov(PercentInvasives~Rain, data=farrer)
summary((RainNova.Invasives)) #rain is significant on percent invasives
RainNova.Weeds<-aov(PercentWeeds~Rain, data=farrer)
summary(RainNova.Weeds) #rain is significant on percent weeds
LitterNova.Natives<-aov(PercentNatives~litter, data=farrer)
summary(LitterNova.Natives) #litter is significant on percent natives
TukeyHSD(LitterNova.Natives)#all three comparisons are significant
LitterNova.Invasives<-aov(PercentInvasives~litter, data=farrer)
summary(LitterNova.Invasives)#litter is significant on percent invasives
TukeyHSD(LitterNova.Invasives)#all three comparisons are significant
LitterNova.Weeds<-aov(PercentWeeds~litter, data=farrer)
summary(LitterNova.Weeds) #litter is significant on percent weeds
TukeyHSD(LitterNova.Weeds) #there is only a significant difference between none and high litter
SoilNova.Natives<-aov(PercentNatives~P1, data=farrer)
summary(SoilNova.Natives) #soil type is significant on percent natives
TukeyHSD(SoilNova.Natives)#Difference between MH and AV, MH_L and AV, MH and MH_L is different for soil compared to AV and C, AV is similar to control
SoilNova.Invasives<-aov(PercentInvasives~P1, data=farrer)
summary(SoilNova.Invasives)#soil type is significant on percent invasives
TukeyHSD(SoilNova.Invasives) #soil type is not hugley important for Invasives, soil type is significant on percent weeds, MH_L was significantly different from all that aern't MH
SoilNova.Weeds<-aov(PercentWeeds~P1, data=farrer)
summary(SoilNova.Weeds)
TukeyHSD(SoilNova.Weeds)#also significant difference is MH_L vs. AV
NativeNova<-aov(PercentNatives~litter*P1*Rain, data=farrer)
summary(NativeNova)
TukeyHSD(NativeNova)
NativeTukey<-TukeyHSD(NativeNova)
InvasivesNova<-aov(PercentInvasives~litter*P1*Rain, data=farrer)
summary(InvasivesNova)#P1, Litter, and Rain are singificant but not interactions
WeedsNova<-aov(PercentWeeds~litter*P1*Rain, data=farrer)
summary(WeedsNova)#P1, Litter, and Rain are singificant but not interactions
