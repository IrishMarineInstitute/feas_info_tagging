rm(list=ls())

library(RODBC)
library(dplyr)
library(readr)
channel <- odbcDriverConnect("Driver=SQL Server; Server=xxxxxxxxx; Database=xxxxxxxxx;")
COD<- sqlQuery(channel,"SELECT Release_lat,Release_long,Release_Date,Tag_front,Tag_back,
Release_Length_cm,Recap_Length_cm,Event,Recap_Lat,Recap_Long,Distance_nm,
Recap_Date,Sex,Maturity,Survey_ID,Station,Year,Days_at_Liberty,Growth
               FROM CodTaggingData")
close(channel)

DUPLICATES<-COD[which(duplicated(COD$Tag_front)==TRUE),]
COD_SINGLE_ENTRY<-COD[-which(duplicated(COD$Tag_front)==TRUE),]

tag=COD_SINGLE_ENTRY%>% count(Year, Survey_ID ,Station ,Release_lat ,Release_long)
names(tag)<-c("Year",'Survey_ID','Station','Latitude','Longitude','numbercodtagged')

lf=COD_SINGLE_ENTRY%>% count(Year,Release_Length_cm)
lf<-na.omit(lf)

###Only complete cases for recap date and distance travaled#
rec<-COD[which(is.na(COD$Recap_Date)==FALSE & is.na(COD$Distance_nm)==FALSE & is.na(COD$Recap_Length_cm )==FALSE & is.na(COD$Growth)==FALSE ),]

rec$Recap_Lat_jit <- jitter(rec$Recap_Lat, factor=0.001)
rec$Recap_Long_jit <- jitter(rec$Recap_Long, factor=0.001)

rec <- mutate(rec, distance_cat=as.factor(ifelse(rec$Distance_nm <50, "Under 50nm",
                                                 ifelse(rec$Distance_nm>50 & rec$Distance_nm<80, "50-80nm",
                                                        ifelse(rec$Distance_nm>80 & Distance_nm<140, "80-140nm", "Migrators >140nm")))))
rec$distance_cat <- ordered(rec$distance_cat, levels = c("Under 50nm", "50-80nm","80-140nm", "Migrators >140nm"))


write_rds(tag,"Data/tag.rds")
write_rds(lf,"Data/lf.rds")
write_rds(rec,"Data/rec.rds")