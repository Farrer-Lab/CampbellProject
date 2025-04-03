#####Data Cleanup#####
getwd() #get working directory
list.files () #list folders on computer
setwd("/Users/campbell.reid/Documents") #choose folder to work with
farrer<-read.csv("Farrerdata.csv") #choose document to work with
head(farrer) #look at headers of document
farrer$Stipul #look at this particular document
farrer[15] #look at this particular column
farrer[12,15]#return 12th row of 15th column
farrer[which(is.na(farrer$AV)),12]<-0 #replace N/As in column with "0"
farrer[which(is.na(farrer$MH)),11]<-0 
farrer[which(is.na(farrer$Placal)), 14]<-0 
farrer[which(is.na(farrer$Stipul)), 15]<-0 
farrer[which(is.na(farrer$Triwil)), 13]<-0 
farrer[which(is.na(farrer$Legumeweed)),18]<-0 
farrer[248,11]<-0 #replace column and row spot with 0
farrer[117,11]<-8.31 #replace column and row spot with 8.31
farrer[95,11]<-0 #replace column and row spot with 0
farrer$Natives<-farrer$Stipul + farrer$Triwil + farrer$Placal #make new column which is the sum of these columns
farrer$Invasives<-as.numeric(farrer$AV) + as.numeric(farrer$MH) #make new column which is the sum of these columns, as.numeric means "read as number"
farrer$Weeds<-as.numeric(farrer$Grassweed) + as.numeric(farrer$Forbweed) + as.numeric(farrer$Legumeweed) 
farrer$TotalPlantBiomass<-farrer$Natives + farrer$Weeds + farrer$Invasives 
farrer$PercentNatives<-farrer$Natives/farrer$TotalPlantBiomass #make new column which value of the natives divided by the total plant biomass
farrer$PercentInvasives<-farrer$Invasives/farrer$TotalPlantBiomass #make new column which value of the invasives divided by the total plant biomass
farrer$PercentWeeds<-farrer$Weeds/farrer$TotalPlantBiomass #make new column which value of the weeds divided by the total plant biomass
farrer$Seed<-0 #make this column and fill it with zero
farrer$Seed[which(farrer$Treatment=="W")]<-"Orange" #replace the zero in this column with "orange" if its treatment is this seed type
farrer$Seed[which(farrer$Treatment=="Z")]<-"Orange"
farrer$Seed[which(farrer$Treatment=="c")]<-"Orange"
farrer$Seed[which(farrer$Treatment=="X")]<-"Pink" #replace the zero in this column with "pink" if its treatment is this seed type
farrer$Seed[which(farrer$Treatment=="a")]<-"Pink"
farrer$Seed[which(farrer$Treatment=="d")]<-"Pink"
farrer$Seed[which(farrer$Treatment=="V")]<-"Yellow" #replace the zero in this column with "yellow" if its treatment is this seed type
farrer$Seed[which(farrer$Treatment=="Y")]<-"Yellow"
farrer$Seed[which(farrer$Treatment=="b")]<-"Yellow"

#####Analysis#####
RainNova.Natives<-aov(PercentNatives~Rain, data=farrer)
summary(RainNova.Natives) #rain is not significant on percent natives
RainNova.Invasives<-aov(PercentInvasives~Rain, data=farrer)
summary((RainNova.Invasives)) #rain is significant on percent invasives
aggregate(farrer$PercentInvasives,list(farrer$Rain),FUN=mean) #find the mean
RainNova.Weeds<-aov(PercentWeeds~Rain, data=farrer)
summary(RainNova.Weeds) #rain is significant on percent weeds
aggregate(farrer$PercentWeeds,list(farrer$Rain),FUN=mean) #find the mean
LitterNova.Natives<-aov(PercentNatives~litter, data=farrer)
summary(LitterNova.Natives) #litter is significant on percent natives
TukeyHSD(LitterNova.Natives)#all three comparisons are significant
plot(TukeyHSD(LitterNova.Natives, conf.level = .95),las=2) #medium litter average natives > high litter, no litter average natives > high litter, no litter average natives > medium litter
aggregate(farrer$PercentNatives,list(farrer$litter),FUN=mean) #find the mean
LitterNova.Invasives<-aov(PercentInvasives~litter, data=farrer)
summary(LitterNova.Invasives)#litter is significant on percent invasives
TukeyHSD(LitterNova.Invasives)#all three comparisons are significant
plot(TukeyHSD(LitterNova.Invasives, conf.level = .95),las=2) #high litter average invasives > medium litter average invasives > no litter average invasives
aggregate(farrer$PercentInvasives,list(farrer$litter),FUN=mean) #find the mean
LitterNova.Weeds<-aov(PercentWeeds~litter, data=farrer)
summary(LitterNova.Weeds) #litter is significant on percent weeds
TukeyHSD(LitterNova.Weeds) #there is only a significant difference between none and high litter
plot(TukeyHSD(LitterNova.Weeds, conf.level = .95),las=2)#medium litter average weeds > hgih litter average weeds, no litter average weeds > high litter, no litter average weeds > medium litter
aggregate(farrer$PercentWeeds,list(farrer$litter),FUN=mean) #find the mean
SoilNova.Natives<-aov(PercentNatives~P1, data=farrer)
summary(SoilNova.Natives) #soil type is significant on percent natives
TukeyHSD(SoilNova.Natives)#Difference between MH and AV, MH_L and AV, MH and MH_L is different for soil compared to AV and C, AV is similar to control
plot(TukeyHSD(SoilNova.Natives, conf.level = .95),las=2) #mostly positive differences in means, see plot for specifics
aggregate(farrer$PercentNatives,list(farrer$P1),FUN=mean) #find the mean
SoilNova.Invasives<-aov(PercentInvasives~P1, data=farrer)
summary(SoilNova.Invasives)#soil type is significant on percent invasives
TukeyHSD(SoilNova.Invasives) #soil type is not hugely important for Invasive, soil type is significant on percent weeds, MH_L was significantly different from all that are not MH
plot(TukeyHSD(SoilNova.Invasives, conf.level = .95),las=2)#mostly negative differences in means, except one, see plot for specifics
aggregate(farrer$PercentInvasives,list(farrer$P1),FUN=mean) #find the mean
SoilNova.Weeds<-aov(PercentWeeds~P1, data=farrer)
summary(SoilNova.Weeds) #soil type is significant on percent weeds
TukeyHSD(SoilNova.Weeds) #only significant difference is MH_L vs. AV
plot(TukeyHSD(SoilNova.Weeds, conf.level = .95),las=2) #mix between negative and positive, but mostly positive, see plot for specifics
aggregate(farrer$PercentWeeds,list(farrer$P1),FUN=mean) #find the mean
NativeNova<-aov(PercentNatives~litter*P1*Rain, data=farrer)
summary(NativeNova) #litter, soil, interaction between litter*soil, and interaction between litter*soil*rain are important
TukeyHSD(NativeNova)
NativeTukey<-TukeyHSD(NativeNova)
InvasivesNova<-aov(PercentInvasives~litter*P1*Rain, data=farrer)
summary(InvasivesNova)#P1, Litter, and Rain are significant but the interactions are not significant
WeedsNova<-aov(PercentWeeds~litter*P1*Rain, data=farrer)
summary(WeedsNova)#P1, Litter, and Rain are significant but not interactions #Orange means MH added 
SeedNova.Invasives<-aov(PercentInvasives~Seed, data=farrer)
summary(SeedNova.Invasives) #seed is not significant on invasives
SeedNova.Natives<-aov(PercentNatives~Seed, data=farrer) 
summary(SeedNova.Natives) #seed is not significant on natives
TukeyHSD(SeedNova.Natives)#the difference between yellow and pink was near significant
SeedNova.Weeds<-aov(PercentWeeds~Seed, data=farrer) 
summary(SeedNova.Weeds) #seed is not significant on weeds

#####Graphs#####
library(ggplot2) #enable ggplot add-on
ggplot(data=farrer, aes(x=P1,y=PercentNatives))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw() #make a bar graph with x being soil treatment and y being percent natives
ggplot(data=farrer, aes(x=litter, y=PercentNatives))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()
ggplot(data=farrer, aes(x=P1, y=PercentInvasives))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()
NativesLitterandSoil.plot<-ggplot(data=farrer, aes(x=P1, y=PercentNatives, fill=litter))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw() #make a bar graph with x being soil treatment and y being percent natives and include differentiation based on litter
InvasivesLitterandSoil.plot<-ggplot(data=farrer, aes(x=P1, y=PercentInvasives, fill=litter))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()+ theme(legend.position="none")
WeedsLitterandSoil.plot<-ggplot(data=farrer, aes(x=P1, y=PercentWeeds, fill=litter))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()+ theme(legend.position="none")
WeedsLitterandSoil.plot+InvasivesLitterandSoil.plot+NativesLitterandSoil.plot
RainWeeds.plot<-ggplot(data=farrer, aes(x=Rain, y=PercentWeeds))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()
RainNatives.plot<-ggplot(data=farrer, aes(x=Rain, y=PercentNatives))+geom_bar(stat="summary", position="dodge", fun="mean")+theme_bw()
RainInvasives.plot<-ggplot(data=farrer, aes(x=Rain, y=PercentInvasives))+geom_bar(stat="summary", position= "dodge", fun="mean")+theme_bw()
RainWeeds.plot+RainInvasives.plot+RainNatives.plot
