######################################
## CARRIZO DATA IMPORT AND CLEANING ##
######################################

## Bring in all the Carrizo data and merge it into a master dataframe, vegtog.
## Clean the data, taking out unnecessary columns, arranging them and filtering observations.

## Data will be used to investigate the effect of cattle grazing on vegetation dynamics.

## Run this script before running any other script in the project.  
## They all require this script, but not each other.   

library(tidyverse)
library(vegan)
library(agricolae)
library(ggpubr)

### LOAD THE DATA ###
vegdat <- read.csv("veg plot data all.csv") # Collected vegetation data from pin drops: 81 drops/quadrat
names(vegdat) = c("date", "quadrat", "observer", "site", "block", "sitetype", 
                  "newquadrat", "code", "precinct", "count", "precinctcurrent",
                  "year", "cover", "comments", "t1", "t2", "t3", "t4", "t5", "t6")
vegdat<-select(vegdat, -starts_with("t"))%>%
  filter(sitetype=="CR"|sitetype=="EP")

sitekey <- read_csv("site type key.csv")    # treatment for each site type in vegdat
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")

precip <- read_csv("veg plot treatment key.csv")%>%
  select(c(1,2, 5))
names(precip) =c("site", "sitetype", "preciptrt")  
precip<-mutate(precip, preciptrt=tolower(preciptrt))%>%
  filter(sitetype=="CR"|sitetype=="EP")%>%
  select(-sitetype)

vegprecip<-read_csv("vegplot_withprecip.csv")%>%
  select(c(4, 6, 9, 16))
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
  select(year, sitetype, site, preciptrt, grazetrt)

vegtog <- left_join(vegdat, sitekey)%>%
  select(-date, -observer, -comments) %>%
  mutate(code = tolower(code)) %>%
  mutate(code=ifelse(code %in% c("bare", "litter", "fresh dirt", "bphole", 
                                 "hole","GOPHER", "MOSS", "ANT", "cowpie", ""), 
                     "nohit", code))%>%
  filter (count !=0, !is.na(count)) %>% # remove GKR exclosure plots and 0/NA 
  mutate(precinct=as.character(precinct)) %>%
  mutate(precinct=ifelse(precinct=="P ","P", precinct)) %>%
  mutate(precinct=as.factor(precinct)) %>%
  mutate(year=as.integer(year))

### JOIN VEGTOG AND PLANTKEY ###
#Standardize column names
plantkey<-left_join(plantkey, funckey)%>%
  select(c(1, 4, 5, 27, 33, 34))

names(plantkey) = c("family", "code", "binomial", "native", "lifecycle", "growthhabit")

vegtog <- left_join(vegtog, plantkey)

### JOIN VEGTOG AND BIOMASS ###

#clean up and rename biomass columns
biomass <- biomass %>%
  select("year", "Site type", "Block", "New Plot ID", "net weight", "season")
names(biomass) = c("year", "sitetype", "block", "quadrat", "netwt", "wtmonth")
biomass <- biomass %>%
  mutate(quadrat=as.factor(quadrat))%>%
  mutate(wtmonth = tolower(wtmonth)) %>%
  filter(sitetype=="CR"|sitetype=="EP", !is.na(year), !is.na(block), !is.na(netwt), !is.na(quadrat), !is.na(wtmonth))%>%
  mutate(wtmonth=ifelse(wtmonth=="spring", "april", wtmonth))  %>%
  filter(wtmonth!="june") %>% ## We lose All the June measurements because their quad #s are 1-8 not "NO215" etc.
  spread(wtmonth, netwt)%>%
  select("year", "quadrat", "april", "october")

#join april and october biomass columns to vegtog
vegtog <- left_join(vegtog, biomass)
names(vegtog)

### JOIN VEGTOG AND COWPIES ###

#clean up and rename cowpies (plot/block level)
cowpies <-cowpies %>%
  select(year, "alt site type", block, cowpies)
names(cowpies)<-c("year", "type","block", "cowpies")
cowpies <-cowpies %>%
  mutate(site = paste(type, "R", block, sep=""))%>%
  select(-type, -block)

##sum cowpies by site and year
cowpies <- aggregate(cowpies$cowpies, by=list(cowpies$site, cowpies$year),  FUN=sum)
names(cowpies) <- c("site", "year", "cowpies")

#join by site and year
vegtog <- left_join(vegtog, cowpies)


###CLEANUP###

#remove columns
vegtog<- vegtog %>%
  select(-sitetype,  -newquadrat)%>%
  filter(!is.na(grazetrt))

#shifted, a list of quadrats that ever shifted precinct classification
current<-count(vegtog, precinctcurrent) %>%
  mutate(precinctcurrent=tolower(precinctcurrent))%>%
  filter((row_number() %in% c(3, 4, 5, 14, 15, 16, 17, 18, 19, 20, 27, 33, 34, 35, 36, 41)))
current<-current$precinctcurrent

shifted <- vegtog_vegonly %>%
  mutate(precinctcurrent=tolower(precinctcurrent)) %>%
  filter((precinctcurrent %in% current)) %>%
  select(quadrat)%>%
  group_by(quadrat)%>%
  summarize() #total of 70 more quadrats to purge

#precip, plots that ever had irrigation treatment on them
precip<-precip%>%
  filter(preciptrt=="shelter"|preciptrt=="irrigation")


### MODIFIED VEGTOGs###


#vegtog with only hits on plants
vegtog_vegonly<-vegtog_hitsonly%>%
  filter(code!="nohit")

# vegtog with plots that had precipitation treatments removed across all years
vegtog_nopreciptrt%>%
  filter(!site %in% precip$site) %>% #take out any plots ever with irrigation in it

# vegtog with plots that had any quadrats shifted removed actoss all years
vegtog_moundshift<-vegtog%>%
  filter(!quadrat %in% shifted$quadrat) #take out any quadrats that ever moved

# vegtog with 2015 and after removed (years with precip treatmetn)
vegtog_pre2015<-vegtog%>%
  filter(year<2015) #double count year, keep old ones in 2015

# Clean up environment.
rm(cowpies, funckey, plantkey, sitekey, vegdat, biomass, current)

names(vegtog)
str(vegtog)



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
