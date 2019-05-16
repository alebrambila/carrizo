######################################
## CARRIZO DATA IMPORT AND CLEANING ##
######################################

## Bring in all the Carrizo data and merge it into a master dataframe, vegtog.
## Clean the data, taking out unnecessary columns, arranging them and filtering observations.

## Data will be used to investigate the effect of cattle grazing on vegetation dynamics.

## Run this script before running any other script in the project.  
## They all require this script, but not each other.   


library(tidyverse)


### LOAD THE DATA ###
vegdat <- read.csv("veg plot data all.csv") # Collected vegetation data from pin drops: 81 drops/quadrat
names(vegdat) = c("date", "quadrat", "observer", "site", "block", "sitetype", 
                  "newquadrat", "code", "precinct", "count", "precinctcurrent",
                  "year", "cover", "comments", "t1", "t2", "t3", "t4", "t5", "t6")
vegdat<-dplyr::select(vegdat, -starts_with("t"))%>%
  filter(sitetype=="CR"|sitetype=="EP")

sitekey <- read_csv("site type key.csv")    # treatment for each site type in vegdat
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")

precip <- read_csv("veg plot treatment key.csv")%>%
  dplyr::select(c(1,2, 5))
names(precip) =c("site", "sitetype", "preciptrt")  
precip<-mutate(precip, preciptrt=tolower(preciptrt))%>%
  filter(sitetype=="CR"|sitetype=="EP")%>%
  dplyr::select(-sitetype)

vegprecip<-read_csv("vegplot_withprecip.csv")%>%
  dplyr::select(c(4, 6, 9, 16))
names(vegprecip)=c("site", "sitetype", "preciptrt", "year")
vegprecip<-vegprecip%>%
  filter(sitetype=="CR"|sitetype=="EP")%>%
  group_by(site, sitetype, year, preciptrt) %>%
  summarize() %>%
  mutate(preciptrt=tolower(preciptrt))%>%
  mutate(preciptrt=ifelse(preciptrt=="irrigation", "irrigation", ifelse(preciptrt=="shelter", "shelter", "none")))
vegprecip$preciptrt[is.na(vegprecip$preciptrt)] <- "none"

plantkey <- read_csv("plant list.csv")#%>%      # species codes interpreted
funckey <- read_csv("plant forms.csv")      # associated plant functional characteristics
biomass <- read_csv("veg plot biomass all years stacked.csv") # biomass per quadrat
cowpies <- read_csv("cowpie counts all years.csv")            # cowpies per plot


### JOIN VEGDAT SITEKEY and PRECIP TO CREATE VEGTOG ###
sitekey<-(left_join(vegprecip, sitekey))
sitekey<-sitekey%>%
  filter(sitetype=="CR"|sitetype=="EP")%>%
  dplyr::select(year, sitetype, site, preciptrt, grazetrt)

vegtog <- left_join(vegdat, sitekey)%>%
  dplyr::select(-date, -observer, -comments) %>%
  mutate(code = tolower(code)) %>%
  mutate(code=ifelse(code %in% c("bare", "litter", "fresh dirt", "bphole", 
                                 "hole","GOPHER", "MOSS", "ANT", "cowpie", "", "bare ", "litter ", "moss"), 
                     "nohit", code))%>%
  filter (count !=0, !is.na(count)) %>% # remove GKR exclosure plots and 0/NA 
  mutate(precinct=as.character(precinct)) %>%
  mutate(precinct=ifelse(precinct=="P ","P", precinct)) %>%
  mutate(precinct=as.factor(precinct)) %>%
  mutate(year=as.integer(year))

### JOIN VEGTOG AND PLANTKEY ###
#Standardize column names
plantkey<-left_join(plantkey, funckey)%>%
  dplyr::select(c(1, 4, 5, 27, 33, 34))
names(plantkey) = c("family", "code", "binomial", "native", "lifecycle", "growthhabit")
vegtog <- left_join(vegtog, plantkey)



### JOIN VEGTOG AND BIOMASS ###
#clean up and rename biomass columns
biomass <- biomass %>%
  dplyr::select("year", "Site type", "Block", "New Plot ID", "net weight", "season")
names(biomass) = c("year", "sitetype", "block", "quadrat", "netwt", "wtmonth")
biomass <- biomass %>%
  mutate(quadrat=as.factor(quadrat))%>%
  mutate(wtmonth = tolower(wtmonth)) %>%
  filter(sitetype=="CR"|sitetype=="EP", !is.na(year), !is.na(block), !is.na(netwt), !is.na(quadrat), !is.na(wtmonth))%>%
  mutate(wtmonth=ifelse(wtmonth=="spring", "april", wtmonth))  %>%
  filter(wtmonth!="june") %>% ## We lose All the June measurements because their quad #s are 1-8 not "NO215" etc.
  spread(wtmonth, netwt)%>%
  dplyr::select("year", "quadrat", "april", "october")

#join april and october biomass columns to vegtog
vegtog <- left_join(vegtog, biomass)
names(vegtog)


### JOIN VEGTOG AND COWPIES ###
#clean up and rename cowpies (plot/block level)
cowpies <-cowpies %>%
  dplyr::select(year, "alt site type", block, cowpies)
names(cowpies)<-c("year", "type","block", "cowpies")
cowpies <-cowpies %>%
  mutate(site = paste(type, "R", block, sep=""))%>%
  dplyr::select(-type, -block)
##sum cowpies by site and year
cowpies <- aggregate(cowpies$cowpies, by=list(cowpies$site, cowpies$year),  FUN=sum)
names(cowpies) <- c("site", "year", "cowpies")
#join by site and year
vegtog <- left_join(vegtog, cowpies)

#remove columns
vegtog<- vegtog %>%
  dplyr::select(-sitetype,  -newquadrat)%>%
  filter(!is.na(grazetrt))



##############
###VERSIONS###
#############


#####PRECIPITATION TREATMENTS REMOVED ACROSS ALL YEARS
precip<-precip%>% #precip, plots that ever had irrigation treatment on them
  filter(preciptrt=="shelter"|preciptrt=="irrigation")
vegtog<-vegtog%>%                          ## VEGTOG_NOPRECIPTRT
  filter(!site %in% precip$site)%>%                       #take out any plots ever with irrigation in it
  filter(block==2|block==3|block==4|block==6)                        # only keep paired plots

test<-vegtog%>% #count quadrats in each plot with raw vegtog (consistent with tests run earlier in script)
  group_by(year, site, quadrat, precinct)%>%
  summarize() %>%
  group_by(year, site, precinct)%>%
  summarize(count=n())
ggplot(test, aes(x=year, y=count)) +geom_line(aes(color=precinct)) +facet_wrap(~site) 
#weird results include spike in EP 2, 3, 4 up in 2015 and down in EP4 in 2009


##### SHIFTED PRECINCT CLASSIFICATION REMOVED
#precinct column is an interpretation they already did of precinctcurrent, use this
#current<-vegtog%>%
#  group_by(quadrat, site, block, precinct,year)%>%
#  summarize()%>%
#  group_by(quadrat, site, block, precinct)%>%
#  summarize(num=n())

#count(vegtog, precinct) #%>%
#  mutate(precinctcurrent=tolower(precinctcurrent))%>%
#  filter((row_number() %in% c(3, 4, 5, 14, 15, 16, 17, 18, 19, 20, 27, 33, 34, 35, 36, 41)))
#current<-current$precinctcurrent
#shifted <- vegtog %>%
#  mutate(precinctcurrent=tolower(precinctcurrent)) %>%
#  filter((precinctcurrent %in% current)) %>%
#  select(quadrat)%>%
#  group_by(quadrat)%>%
#  summarize() #total of 70 more quadrats to purge

#vm<-vegtog%>%                           ### VEGTOG_MOUNDSHIFT
#  filter(quadrat %in% shifted$quadrat)%>%
#  group_by(quadrat, site, block, year, precinct, precinctcurrent)%>%
#  summarize()%>%
#  mutate(precinctcurrent=tolower(precinctcurrent))%>%
#  filter(block==2|block==3|block==4|block==6)

#vegtog_moundshift<-vegtog%>%                           ### VEGTOG_MOUNDSHIFT
#  filter(!quadrat %in% shifted$quadrat)                #take out any quadrats that ever moved

#test<-vegtog%>% #count quadrats in each plot with raw vegtog 
#  group_by(year, site, quadrat, precinct)%>%
#  summarize() %>%
#  group_by(year, site, precinct)%>%
#  summarize(count=n())
#ggplot(test, aes(x=year, y=count)) +geom_line(aes(color=test$precinct)) +facet_wrap(~site) 


### ONLY HITS ON PLANTS
vegtog<-vegtog%>%
  mutate(code=ifelse(code=="vulmic ", "vulmic", ifelse(code=="lepnit ", "lepnit", ifelse(code=="phlgra ", "phlgra", ifelse(code=="trigra ", "trigra", code)))))


## have to add up same species same place

  

##### 2015 AND AFTER REMOVED (YEARS WITH PRECIPTRT)
#vegtog_pre2015<-vegtog_moundshift%>%
#  filter(year<2015) #double count year, keep old ones in 2015


#####################################################
###  PRECIP                                       ###
#####################################################
climate<-read_csv("CAZC1climate.csv", skip=11)%>%
  dplyr::select(-1)%>%
  separate(X2, into=c("year", "month", "day"), sep="-")%>%
  separate(day, into=c("day", "time"), sep=" ")%>%
  group_by(year, month, day)%>%
  filter(!is.na(Millimeters), !is.na(Celsius))%>%
  summarize(mm=median(Millimeters), celsius=mean(Celsius))%>%
  group_by(year, month)%>%
  summarize(startmm=min(mm), endmm=max(mm), mmt=mean(celsius))%>%
  mutate(preciptest=endmm-startmm)%>%
  mutate(precip=ifelse(preciptest<70, as.character(preciptest), "CHECK"))

climate[11, 7]<-as.numeric(climate[11, 6])-as.numeric(climate[10, 4])
climate[13, 7]<-climate[13, 6]
climate[24, 7]<-climate[25, 3]
climate[36, 7]<-as.numeric(climate[36, 4])-as.numeric(climate[35, 4])
climate[37, 7]<-climate[37, 6]
climate[48, 7]<-climate[49, 3]
climate[51, 7]<-climate[51, 6]
climate[60, 7]<-climate[61, 3]
climate[73, 7]<-as.numeric(climate[73, 4])-as.numeric(climate[72, 4])+as.numeric(climate[74, 3])
climate[84, 7]<-climate[85, 3]
climate[96, 7]<-as.numeric(climate[96, 4])-as.numeric(climate[95, 4])+as.numeric(climate[97, 3])
climate[108, 7]<-0
climate[121, 7]<-climate[121, 4]
climate[122, 7]<-climate[122, 6]

climate<-climate%>%
  ungroup()%>%
  mutate(month=as.numeric(month))%>%
  mutate(year=as.numeric(year))%>%
  mutate(rainyear=ifelse(month>7, year+1, year))%>%
  mutate(preciplegacy1=rainyear+1)%>%
  mutate(preciplegacy2=rainyear+2)%>%
  mutate(precip=as.numeric(precip))%>%
  group_by(rainyear, preciplegacy1, preciplegacy2)%>%
  summarize(precip=sum(precip), MAT=mean(mmt))%>%
  filter(rainyear!=2007, rainyear!=2018)%>%
  mutate(sprecip = (precip-191.68) / 117.91)%>%
  mutate(extremeyear=ifelse(sprecip>=1, "wet", ifelse(sprecip<=-1, "dry", "normal")))%>%
  mutate(wetordry=ifelse(sprecip>=.5, "wet", ifelse(sprecip<=-.5, "dry", "normal")))
climate$extremeyear<-as.factor(climate$extremeyear)
climate$wetordry<-as.factor(climate$wetordry)


precip<-ggplot(climate)+geom_bar(aes(x=as.factor(rainyear), y=precip), stat="identity")
MAT<-ggplot(climate)+geom_line(aes(x=as.integer(rainyear), y=MAT))

ggarrange(precip, MAT)

climsum<-climate%>%
  group_by(rainyear, extremeyear)%>%
  summarize(meanprecip=mean(precip))
climsum2<-climate%>%
  group_by(rainyear, extremeyear)%>%
  summarize(meanprecip=mean(precip))

rainyear<-climate%>%
  mutate(year=rainyear)%>%
  ungroup()%>%
  dplyr::select(1, 8, 7, 6, 5, 4)





##################################################
# EPILOGUE: WHY ARENT QUADRATS BALANCED STILL??? #
##################################################

#General quadrat attrition, see that in 2015 and 16 in 2, 3, and 4 ungrazed # of quads is too high
quadCount<-vegtog%>%
  group_by(year, grazetrt, block, precinct) %>%
  summarize(totquadcount=length(unique(quadrat))) 

ggplot(quadCount, aes(x=year, y=totquadcount, color=interaction(grazetrt, precinct))) +
  geom_line() +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.25, ymax=10, alpha = .2)+
  annotate("text", x=2013.5, y=11, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="# of Quadrats")+facet_wrap(~block)

# what is going on in 2015?
#in 2015, EP2 (12), EP3 (11) and EP4 (12) have more quadrats than normal (8)
#in 2016, EP2 and EP3 still have 9 quadrats. 
# rule: as new plots are added remove all old plots that dont go forwards.  keep them in previous years

vegsum<-vegtog%>%
  group_by(quadrat, site, year, grazetrt)%>%
  summarize()%>%
  filter((year==2014|year==2015|year==2016|year==2017)&(site=="EP2"|site=="EP3"|site=="EP4"))%>%
  ungroup()%>%
  dplyr::select(year, site, quadrat)%>%
  group_by(year, site)%>%
  mutate(n=n())%>%
  mutate(quadrat=substr(quadrat, 5, 9))%>%
  mutate(yrsite=paste(year, site, sep="_"))%>%
  dplyr::select(-n, year, site)%>%
  spread(quadrat, year)
# EP2: new in 15=0210, 0402, 0507.  
  #end in 2015: 1213, 1402... remove from 2015       (11 to 8)
  #end in 2016: 0805... remove from 2015 and 2016.   (9 to 8)
# EP3: new in 15=N1409 P1503, N1507.  
  #end in 2015: P1015... remove from 2015  (12 to 8)
  #end in 2016: P1208, N1506... remove from 2015 and 2016.  
  #P1618, N0307 missed in 2016.  leave it
  #N0209 only 2016 leave it
# EP4: new in 15=1012, 1412, 1607, 1712.  
  #end in 2015: 1719, 0816, 0311, 1516 remove from 2015  (12 to 8)

vegsum<-vegtog%>%
  group_by(quadrat, site, year, grazetrt)%>%
  summarize()%>%
  filter((year==2014|year==2015|year==2016|year==2017)&(site=="EP2"|site=="EP3"|site=="EP4"))%>%
  ungroup()%>%
  dplyr::select(year, site, quadrat)%>%
  dplyr::select(1, 3)%>%
  mutate(new=ifelse((year==2015)&(quadrat=="EP2-N1213"|quadrat=="EP2-P1402"|quadrat=="EP2-P0805"|
                                    quadrat=="EP3-P1015"|quadrat=="EP3-P1208"|quadrat=="EP3-N1506"|
                                    quadrat=="EP4-N1719"|quadrat=="EP4-N0816"|quadrat=="EP4-P0311"|
                                    quadrat=="EP4-P1516"),"bad", "good"))%>%
  mutate(new=ifelse((year==2016)&(quadrat=="EP2-P0805"|quadrat=="EP3-P1208"|quadrat=="EP3-N1506"), "bad", new))

quadCount2<-left_join(vegtog, vegsum)%>%
  mutate(new=ifelse(is.na(new), "good", new))%>%
  filter(new!="bad")%>%
  group_by(year, grazetrt, block, precinct) %>%
  summarize(totquadcount=length(unique(quadrat)))
ggplot(quadCount2, aes(x=year, y=totquadcount, color=interaction(grazetrt, precinct))) +
  geom_line() +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.25, ymax=10, alpha = .2)+
  annotate("text", x=2013.5, y=11, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="# of Quadrats")+facet_wrap(~block)

# extra plots cleaned out.  apply same procedure to vegtog
vegtog<-left_join(vegtog, vegsum)%>%
  mutate(new=ifelse(is.na(new), "good", new))%>%
  filter(new!="bad")%>%
  dplyr::select(-new) #vegtog lost about 150 observations.  sitting at EXACTLY 5000 now, cool! 


vegtog<-left_join(vegtog, rainyear, by=c("year"="rainyear"))
vegtog_vegonly<-left_join(vegtog_vegonly, rainyear, by=c("year"="rainyear"))

# Clean up environment.
rm(PL1, PL2, rainyear, precip, MAT, climsum, climsum2)
rm(cowpies, funckey, plantkey, sitekey, vegdat, biomass, current, precip, shifted, vegprecip, test)

names(vegtog)
str(vegtog)

vegtog<-unique(vegtog)
vegtog_vegonly<-unique(vegtog_vegonly)

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

# set ggplot2 theme
theme_set(theme_bw(base_size = 12) + theme(text = element_text(size = 14)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black"),
                  legend.title=element_blank()))

library(vegan)
library(agricolae)
library(ggpubr)
library(codyn)
library(nlme)
library(multcomp)