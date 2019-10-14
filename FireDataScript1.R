rm(list=ls())

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gdata)

FireData <- read_excel("C:/Users/Colin.000/Desktop/STAT465/RudyProject/CalOES_DINS_TableToExcel.xlsx")

#Removing Non-residences

toRemove = unique(FireData$StructureT)[c((3:8),(10:16))]
toRemove[13]
FireData2 = FireData

for (i in c(1:12))
{
  FireData2 = subset(FireData2, StructureT!=toRemove[i])
}

#Removing less damaged homes (want Destroyed or 55-75%)

FireData3 = FireData2
toRemove = unique(FireData$Damage)[c(2,3,4)]

for (i in c(1:3))
{
  FireData3 = subset(FireData3, Damage!=toRemove[i])
}



#Correct County (remove unincorporated)

FireData4 = FireData3
unique(FireData5$City)

#Removing Just unincorporated-VEN
FireData4 = subset(FireData4, City!="Unincorporated-VEN")

#Removing both
toRemove = unique(FireData$City)[c(2,3)]
for (i in c(1:2))
{
  FireData5 = subset(FireData4, City!=toRemove[i])
}

#Adding blank rows
colNames = names(FireData5)
na.df = FireData5
for (i in 1:length(FireData5))
{
  for (j in 1:length(FireData5$OBJECTID_1))
  {
    na.df[j,i] = NA
  }
}

na.df = na.df[1,]

spaced.df1 <- do.call(rbind, apply(FireData4, 1, function(x) {rbind(x, na.df)}))


#Adding pairings
spaced.df1$Pairing = NA

loop = c(1:505) * 2
for (i in loop)
{
  spaced.df1$Pairing[i - 1] = i/2
  spaced.df1$Pairing[i] = i/2
}

col_idx <- grep("SiteAddres", names(spaced.df1))
spaced.df1 <- spaced.df1[, c(col_idx, (1:ncol(spaced.df1))[-col_idx])]

col_idx <- grep("Pairing", names(spaced.df1))
spaced.df1 <- spaced.df1[, c(col_idx, (1:ncol(spaced.df1))[-col_idx])]





#Removing unneccessary variables
spaced.df2 = sapply(spaced.df1, as.character)
spaced.df2[is.na(spaced.df2)] = " "



write.csv(spaced.df2, file = "ThomasFireDataReducedInclSB.csv")

getwd()




#####PAIRS DATA######

##SB ONLY##
#ThomasFirePairs1 <- read.csv("~/ThomasFirePairs2.csv")


##INCLUDE VENTURA##
ThomasFirePairs1 <- read.csv("~/ThomasFireDataReducedIncludingSBVent.csv")


ThomasFirePairs2 = ThomasFirePairs1[which(ThomasFirePairs1$Paired == 1),]

unique(ThomasFirePairs2$Damage)

ThomasFirePairs2$Damage <- replace_na(as.numeric(dplyr::recode(ThomasFirePairs2$Damage, 
                                                                                 "Undamaged" = 0)), 1)


ThomasFirePairs2 = drop.levels(ThomasFirePairs2)

#Fence Bar Chart
dat <- data.frame(table(ThomasFirePairs2$Fence,ThomasFirePairs2$Damage))
names(dat) <- c("Fence","Damage","Count")

ggplot(data=dat, aes(x=Fence, y=Count, fill=Damage)) + geom_bar(stat="identity")  + labs(title = "Bar Chart",
                                                                                      subtitle = "Proportion of Burned Homes \nGrouped by Fence Type",
                                                                                      x = "Fence Type", y = "Count") + scale_fill_discrete(name = "Fire Damage", labels = c("Undamaged", "Damaged"))
FenceTable = aggregate(ThomasFirePairs2[,21], list(ThomasFirePairs2$Fence), mean)
FenceTable$Group.1 = as.factor(FenceTable$Group.1)
colnames(FenceTable) = c("Fence Type", "Prop Burned")

FenceModel <- glm(Damage ~ Fence, data = ThomasFirePairs2, family = binomial(link = "logit"))

FenceTable
summary(FenceModel)

#Topography Bar Chart
length(unique(ThomasFirePairs3$Topography))
dat <- data.frame(table(ThomasFirePairs3$Topography,ThomasFirePairs3$Damage))
names(dat) <- c("Topography","Damage","Count")

ggplot(data=dat, aes(x=Topography, y=Count, fill=Damage)) + geom_bar(stat="identity")  + labs(title = "Bar Chart",
                                                                                              subtitle = "Proportion of Burned Homes \nGrouped by Topography Type",
                                                                                              x = "Topography Type",
                                                                                              y = "Count") + scale_fill_discrete(name = "Fire Damage", labels = c("Undamaged", "Damaged"))
TopographyTable = aggregate(ThomasFirePairs2[,21], list(ThomasFirePairs2$Topography), mean)
TopographyTable$Group.1 = as.factor(TopographyTable$Group.1)
colnames(TopographyTable) = c("Topography", "Prop Burned")

TopographyModel <- glm(Damage ~ Topography, data = ThomasFirePairs2, family = binomial(link = "logit"))

TopographyTable
summary(TopographyModel)

unique(ThomasFirePairs2$StructureT)
#Structure Type Bar Chart
dat <- data.frame(table(ThomasFirePairs2$StructureT,ThomasFirePairs2$Damage))
names(dat) <- c("StructureType","Damage","Count")

ggplot(data=dat, aes(x=StructureType, y=Count, fill=Damage)) + geom_bar(stat="identity")  + labs(title = "Bar Chart",
                                                                                                 subtitle = "Proportion of Burned Homes \nGrouped by Structure Type",
                                                                                                 x = "Structure Type",
                                                                                                 y = "Count") + scale_fill_discrete(name = "Fire Damage", labels = c("Undamaged", "Damaged"))
StructureTTable = aggregate(ThomasFirePairs2[,21], list(ThomasFirePairs2$StructureT), mean)
StructureTTable$Group.1 = as.factor(StructureTTable$Group.1)
colnames(StructureTTable) = c("Structure Type", "Prop Burned")

StructureTModel <- glm(Damage ~ StructureT, data = ThomasFirePairs2, family = binomial(link = "logit"))

StructureTTable
summary(StructureTModel)


#Deck/Porch Bar Chart
dat <- data.frame(table(ThomasFirePairs2$DeckPorch,ThomasFirePairs2$Damage))
names(dat) <- c("DeckPorch","Damage","Count")

ggplot(data=dat, aes(x=DeckPorch, y=Count, fill=Damage)) + geom_bar(stat="identity")  + labs(title = "Bar Chart",
                                                                                             subtitle = "Proportion of Burned Homes \nGrouped by Deck/Porch",
                                                                                             x = "Deck/Porch",
                                                                                             y = "Count") + scale_fill_discrete(name = "Fire Damage", labels = c("Undamaged", "Damaged"))

DeckPorchTable = aggregate(ThomasFirePairs2[,21], list(ThomasFirePairs2$DeckPorch), mean)
DeckPorchTable$Group.1 = as.factor(DeckPorchTable$Group.1)
colnames(DeckPorchTable) = c("Deck/Porch Type", "Prop Burned")

DeckPorchModel <- glm(Damage ~ DeckPorch, data = ThomasFirePairs2, family = binomial(link = "logit"))

DeckPorchTable
summary(DeckPorchModel)

