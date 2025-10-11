#Data analysis for in-person parking survey
#Camille Antinori - owner

rm(list=ls())
#datafile location: C:\Users\camil\Documents\Sail\Parking lots\PRA-Intercept survey data-City of Berkeley-2025....
#One drive version: #setwd("~/Sail/Parking lots/PRA-Intercept survey data-City of Berkeley-2025)
#set working directory to This PC
setwd("C:/Users/camil/Documents/Sail/Parking lots/PRA-Intercept survey data-City of Berkeley-2025")


library(readxl)    
library(dplyr)
library(tidyverse)
library(magrittr)
library(zipcodeR)
library(Hmisc)
library(doBy)
library(psych)

#Reading in file -------------
park24<-read_excel("ParkingSurveyN459.xlsx", sheet = "Sheet", 
                 na=c("NA","na"),
                 col_types = c("numeric","numeric","date","date", "text", 
				"text", "text", "text", "text", "text", 
				"text", "text", "text", "text", "text", 
				"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text", "text", 
"text", "text", "text", "text" )
)



#initial look at dataset
#head(park24)
#tail(park24)
names(park24)
str(park24)
dim(park24)

#  Income ---------
table(park24$Income)
park24$inclevel <- factor(park24$Income, levels = c(
  "$0-$24,999", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999", 
  "$100,000-$124,999", "$125,000-$149,999", "$150,000-$174,999", "$175,000-199,999", "$200,000 and up")
)

table(park24$inclevel)
park24$inclow<-ifelse(park24$inclevel=="$0-$24,999"|
                        park24$inclevel=="$25,000-$49,999"|
                        park24$inclevel=="$50,000-$74,999",1,0)


# Lots --------------------
unique(park24$Lot)
table(park24$Lot)
unique(park24$OtherLot)
table(park24$OtherLot)

#park24$location<-(park24$LOT)
#park24$location <- gsub("oakland|Oakland", "Oakland", park24$LOT)


park24$scove<-as.numeric(park24$Lot=='South Cove West')
park24$ecove<-as.numeric(park24$Lot=='South Cove East')
park24$skatesn<-as.numeric(park24$Lot=='Skates/N Lot')
park24$olot<-as.numeric(park24$Lot=='O Lot')
park24$de<-as.numeric(park24$Lot=='D&E')
park24$spincir<-as.numeric(park24$Lot=='Spinnaker Circle')
park24$spinway<-as.numeric(park24$Lot=='Spinnaker Way')
park24$marinb<-as.numeric(park24$Lot=='Marina Blvd')
park24$jk<-as.numeric(park24$Lot=='J&K')
park24$lm<-as.numeric(park24$Lot=='L&M')
park24$sw199<-as.numeric(park24$Lot=='199 Seawall Drive')
park24$swst<-as.numeric(park24$Lot=='Seawall Drive (Street)')
park24$fg<-as.numeric(park24$Lot=='F&G')


#reclassifying "other" 
park24$scove[park24$OtherLot=="Cal Sailing"]<-1
park24$scove[park24$OtherLot=="West cove"]<-1
park24$scove[park24$OtherLot=="West coast"]<-1
park24$scove[park24$OtherLot=="Over by cal sailing, not sure which one of those listed this is"]<-1
park24$olot[park24$OtherLot=="1 Seawall Drive, Gate O Dock"]<-1
park24$sw199[park24$OtherLot=="Upper HSL"]<-1
park24$ecove[park24$OtherLot=="EAST COVE"]<-1
park24$ecove[park24$OtherLot=="next to new toilet bldg/CAL adventures"]<-1
park24$swst[park24$OtherLot=="seawall"]<-1
park24$skatesn[park24$OtherLot=="Skates"]<-1
park24$fg[park24$OtherLot=="F Marina"]<-1
park24$spinway[park24$OtherLot=="B"]<-1


park24$southfee<-ifelse(park24$scove==1|park24$ecove==1|park24$jk==1,1,0)
#cbind(park24$scove,park24$ecove,park24$jk,park24$southfee)
park24$south<-ifelse(park24$southfee==1|park24$sw199==1|park24$swst==1, 1,0)
#park24$north<-ifelse(park24$marinb==1|park24$spinway==1|park24$spincir==1,1,0)  
park24$north<-ifelse(park24$marinb==1|park24$spinway==1|park24$spincir==1,1,0)  

table(park24$south,park24$inclow)
table(park24$southfee,park24$inclow)
table(park24$north,park24$inclow)

#chisq.test(incdif)


describeBy(park24$scove, park24$inclevel)
describeBy(park24$spinway, park24$inclevel)
describeBy(park24$spincir, park24$inclevel)
describeBy(park24$sw199, park24$inclevel)
describeBy(park24$swst, park24$inclevel)
describeBy(park24$skatesn, park24$inclevel)
describeBy(park24$olot, park24$inclevel)
describeBy(park24$fg, park24$inclevel)
describeBy(park24$inclevel, park24$fg)

# Breakdown by inclevel for each lot, take out NAs in inclevel ----------------

table(park24$scove)
nasum<-sum(is.na(park24$inclevel[park24$scove==1]))
allsum<-sum(park24$scove==1)
summary <- park24 %>%
  filter(scove == 1) %>%
  group_by(inclevel) %>%
  dplyr::summarise(
    count = n(),
    length = length(scove==1),
    percent = n()/(allsum-nasum)
  )
print(summary, n=21)

table(park24$spinway)
summary <- park24 %>%
  filter(spinway == 1) %>%
  group_by(inclevel) %>%
  dplyr::summarise(
    count = n(),
    length = length(spinway==1),
    percent = n()/39
  )
print(summary, n=21)



summary <- park24 %>%
  filter(fg == 1) %>%
  group_by(inclevel) %>%
  dplyr::summarise(
    count = n(),
    length = length(fg==1),
    percent = n()/9
      )
print(summary, n=21)






# Handy R code from other data -------------------------------

#  #obs per year for full  dataset
fish$obsyear=factor(format(fish$DATE,'%Y'))
sumbyyr<-fish %>%
  group_by(obsyear) %>%
  summarise(N=n())
sumbyyr                                                                

##alt way re chatgpi  !!!!!
fish$date <- as.Date(fish$'DATE')
year_counts <- table(format(fish$date, "%Y"))
year_counts

#refusals: 0 in 2021, 17 in 2022, 14 in 2023: rate is 31/250 or 12%

#make shorter name
fish$visits<-fish$Vperyear
table(fish$visits)

fish$future<-fish$futureplan
fish$vpier<-fish$Vperyearpier
fish$origin <- fish$`WHERE FROM` 

fish$WTPdaypkgXvisits
fish$wtp<-as.numeric(fish$WTPdaypkgXvisits)


###########Zipping
### creating new columns from  https://cmdlinetips.com/2020/07/dplyr-mutate-create-new-variables-with-mutate/

#94701 is a PO box zipcode and not location.  Replace with 94703, most common in Bkly
fish$zipcode[fish$zipcode=="94701"]<-"94703"

#create column with berkeley marina zipcode 94710
fish$marzip <- '94710'

#generate distances with zipcodeR
fish %<>%
    rowwise() %>%
    mutate(distberk=zip_distance(zipcode,marzip)$distance)

fish$distberk

#alternative fishing piers

#using Veteran's pier (Bay Farm Island Bridge) for Alameda near airport off of Doolittle dr. 
#(should we change to rock wall at 94501?) 
fish$alamzip <- 94502
fish %<>%
  rowwise() %>%
  mutate(distalam=zip_distance(zipcode,alamzip)$distance)

#Richmond Ferry Pt Pier which is in Miller-Knox park (not at Rich ferry terminal)
fish$richzip <- 94801
fish %<>%
  rowwise() %>%
  mutate(districh=zip_distance(zipcode,richzip)$distance)

#using Pinole zipcode  
fish$pinolezip <- 94806
fish %<>% 
  rowwise() %>%
  mutate(distpin=zip_distance(zipcode,pinolezip)$distance)


#Anticoh
fish$antzip <- 94509
fish %<>%
  rowwise() %>%
  mutate(distantio=zip_distance(zipcode,antzip)$distance)


#Eckley pier in Crockett
#fish$eckleyzip<- 94525

## create closest pier other than berkeley
#need to add distant

fish$altdist1<-pmin(fish$distalam, fish$districh, fish$distemery, fish$distoak, 
                    fish$distpin, fish$distsf, fish$distpit, fish$distslean, fish$distantio)
fish$altdist1


## create second closest pier other than berkeley
##using: https://www.w3schools.com/r/r_for_loop.asp

x<-cbind(fish$distalam, fish$districh, fish$distemery, fish$distoak, fish$distpin, 
         fish$distsf, fish$distpit, fish$distslean, fish$distantio)

#x[125,(t(apply(x,1,rank))==2)[125,]]
numobs<-dim(fish)[1]
numobs

rx2 <- t(apply(x,1,rank)==2)
rx2

x[1,rx2[1,]]
x[1,]

x2 <- 0
for(i in 1:numobs) { x2[i] <- x[i,rx2[i,]] }
x2

fish$altdist2<-x2
fish$altdist2

dim(fish)

#Cost variables

fish$cost<-2*.6188*fish$distberk
fish$costalt<-2*.6188*fish$altdist1
fish$costalt2<-2*.6188*fish$altdist2

fish$costoak<-2*.6188*fish$distoak
fish$costrich<-2*.6188*fish$districh
fish$costalam<-2*.6188*fish$distalam


#check for various ways cities recorded
unique(fish$origin)
table(fish$origin)
fish$town<-(fish$origin)
fish$town <- gsub("oakland|Oakland", "Oakland", fish$origin)
cbind(fish$origin, fish$town)

table(fish$SUBCAT)

##########subdataset with primary reason = fish and distance less than or = 200 miles
#https://www.geeksforgeeks.org/how-to-filter-r-dataframe-by-multiple-conditions/

fishdat <- subset(fish, SUBCAT==1&distberk<=200)
###################################################################

#need visits as integer
fishdat$vint<-as.integer(fishdat$visits)
fishdat$vpierint<-as.integer(fishdat$vpier)
fishdat$futint<-as.integer(fishdat$future)


#drop  obs where vint = NA
fishdat <- subset(fishdat, fishdat$vint!="NA")
dim(fishdat)


##shorter names

fishdat$pieralam<-fishdat$`OTHPUBPIERS-Alameda`
fishdat$pierantioch<-fishdat$`OTHPUBPIERS-Antioch`
fishdat$pieremer<-fishdat$`OTHPUBPIERS-Emeryville`
fishdat$pieroak<-fishdat$`OTHPUBPIERS-Oak`
fishdat$pierother<-fishdat$`OTHPUBPIERS-Other`
fishdat$pierpin<-fishdat$`OTHPUBPIERS-Pinole`
fishdat$pierpitt<-fishdat$`OTHPUBPIERS-Pittsburg`
fishdat$pierrich<-fishdat$`OTHPUBPIERS-Rich`
fishdat$piersl<-fishdat$`OTHPUBPIERS-SanLeandro`
fishdat$piersf<-fishdat$`OTHPUBPIERS-SF`

fishdat$subs<-fishdat$pieralam +
  fishdat$pierantioch +
  fishdat$pieremer +
  fishdat$pieroak +
  fishdat$pierother+
  fishdat$pierpin+
  fishdat$pierpitt+
  fishdat$pierrich+
  fishdat$piersl+
  fishdat$piersf



#create background in one var

#fishdat$back<-with(fishdat, ifelse(fishdat$white==1&fishdat$mix==0, "White", 0))
#fishdat$back
#freq(fishdat$back)
fishdat$back = 0
fishdat$back[fishdat$`RACE-a`==1 & fishdat$mixed==0]="Asian"
fishdat$back[fishdat$`RACE-b`==1 & fishdat$mixed==0]="AfAmer"
fishdat$back[fishdat$`RACE-h`==1 & fishdat$mixed==0]="Hisp"
fishdat$back[fishdat$`RACE-w`==1 & fishdat$mixed==0]="White"
fishdat$back[fishdat$`RACE-nathaw`==1 & fishdat$mixed==0]="NatHaw"
fishdat$back[fishdat$`RACE-natamer`==1 & fishdat$mixed==0]="NatAmer"
fishdat$back[fishdat$`RACE-other`==1 & fishdat$mixed==0]="Other"

#fishdat$back[fishdat$back==0 & fishdat$mixed==1]="Mixed"
fishdat$back[fishdat$mixed==1]="Mixed"
fishdat$back[fishdat$back==0] = NA

fishdat$back
table(fishdat$back)

#dummies
fishdat$backaa <- ifelse(fishdat$back == "AfAmer", 1, 0)
fishdat$backasian <- ifelse(fishdat$back == "Asian", 1, 0)
fishdat$backwhite <- ifelse(fishdat$back == "White", 1, 0)
fishdat$backnonw <- ifelse(fishdat$back == "White", 0, 1)
fishdat$backhisp <- ifelse(fishdat$back == "Hisp", 1, 0)
fishdat$backmix <- ifelse(fishdat$back == "Mixed", 1, 0)


#income levels
fishdat$INClevel
fishdat$inc <- factor(fishdat$INClevel, levels = 1:3, labels = c("Low", "Med", "High"))
table(fishdat$inc)

fishdat$level1<-fishdat$inc=="Low"
fishdat$level2<-fishdat$inc=="Med"
fishdat$level3<-fishdat$inc=="High"

#create dummy for prefer berkeley
unique(fishdat$`PREFER BKLY`)
fishdat$prefer<-fishdat$`PREFER BKLY`

fishdat$prefer <- gsub("yes|YES|Yes", "1", fishdat$`PREFER BKLY`)
fishdat$prefer <- gsub("no|No", "0", fishdat$prefer)
cbind(fishdat$`PREFER BKLY`, fishdat$prefer)
fishdat$prefer<-as.numeric(fishdat$prefer)

#create dummy for precovid
unique(fishdat$`PRE-COVID`)
fishdat$precov<-fishdat$`PRE-COVID`

fishdat$precov <- gsub("yes|yes, when pier open|yes; even more ~3x week", "1", fishdat$`PRE-COVID`)
fishdat$precov <- gsub("no", "0", fishdat$precov)
cbind(fishdat$`PRE-COVID`, fishdat$precov)
fishdat$precov<-as.numeric(fishdat$precov)

## create a year variable
fishdat$obsyear=factor(format(fishdat$DATE,'%Y'))
fishdat$y22 <- fishdat$obsyear==2022
fishdat$y23 <- fishdat$obsyear==2023


#Interaction terms
fishdat$inc1<-fishdat$inc =='Low'
fishdat$inc2<-fishdat$inc =='Med'
fishdat$inc3<-fishdat$inc =='High'

fishdat$inc2Xcost <- fishdat$inc2*fishdat$cost
fishdat$inc3Xcost <- fishdat$inc3*fishdat$cost

fishdat$inc1Xhisp <- fishdat$inc1*fishdat$backhisp
fishdat$inc2Xhisp <- fishdat$inc2*fishdat$backhisp
fishdat$inc3Xhisp <- fishdat$inc3*fishdat$backhisp

fishdat$inc2Xaa <- fishdat$inc2*fishdat$backaa
fishdat$inc3Xaa <- fishdat$inc3*fishdat$backaa

fishdat$inc2Xasian <- fishdat$inc2*fishdat$backasian
fishdat$inc3Xasian <- fishdat$inc3*fishdat$backasian

fishdat$inc2Xwhite <- fishdat$inc2*fishdat$backwhite
fishdat$inc3Xwhite <- fishdat$inc3*fishdat$backwhite

fishdat$inc2Xmix <- fishdat$inc2*fishdat$backmix
fishdat$inc3Xmix <- fishdat$inc3*fishdat$backmix

fishdat$hispXcost <- fishdat$backhisp*fishdat$cost
fishdat$aaXcost <- fishdat$backaa*fishdat$cost
fishdat$asianXcost <- fishdat$backasian*fishdat$cost
fishdat$whiteXcost <- fishdat$backwhite*fishdat$cost

fishdat$food<-as.numeric(fishdat$WHYFOOD)
fishdat$foodcost<-fishdat$food*fishdat$cost

dim(fishdat)

#export
write_csv(fishdat,
          file="fishdat.csv"
)



#create metadata
k <- contents(fishdat)
print(k, sort='names')

# gsub command example
#df$gender <- gsub("(?i)F|(?i)Female", "1", df$gender)
#df$gender <- gsub("(?i)M|(?i)Male", "0", df$gender)
