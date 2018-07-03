## This script exists to bring in all the Carrizo data and merge it into a master dataframe, vegtog.
## It also aims to clean the data taking out unnecessary columns, arranging them and filtering irrelevant
## or bad observations. 

# load libraries to use
library(tidyverse)
library(vegan)


### LOAD THE DATA ###
#Collected vegetation data from pin drops
vegdat <- read.csv("veg plot data all.csv")
#treatment for each site type in vegdat
sitekey <- read_csv("site type key.csv")
#what species the species codes in vegdat refer to
plantkey <- read_csv("plant list.csv")
#functional characteristics of species in plant list
funckey <- read_csv("plant forms.csv")
#biomass per quadrat
biomass <- read_csv("veg plot biomass all years stacked.csv")
#cowpies per plot
cowpies <- read_csv("cowpie counts all years.csv")



### JOIN VEGDAT AND SITE KEY TO CREATE VEGTOG ###

#standardize column names:
names(vegdat) = c("date", "quadrat", "observer", "site", "plot", "sitetype", "precipblock", "newquadrat", "preciptrt", "code", "precinct", "comments",
                  "count", "originalorder", "precinctcurrent", "year", "ID", "cover")
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")
vegtog <- left_join(vegdat, sitekey) %>%
  select(-date, -observer, -comments, -originalorder, ID, -altsitetype) %>%
  filter(code != "bare", code != "litter", code != "fresh dirt", code != "BARE", code != "HOLE", code != "bphole", code != "hole", code != "GOPHER", code != "MOSS",
         code != "ANT", code != "LITTER", code != "FRESH DIRT", code != "cowpie", code != "") %>%
  mutate(code = tolower(code)) %>%
  mutate(precipblock = tolower(precipblock)) %>%
  # remove the swain pasture that was never grazed
  filter(pasturetrt != "swain")%>%
  # remove rat exclosures in the remaining area, as well as species with no counts or NA counts.
  filter (count !=0, !is.na(count), exclosure != "CW rat excl")


### JOIN VEGTOG AND PLANTKEY ###
#Standardize column names
names(plantkey) = c("plantID", "family", "commonfamily", "shortcode", "code", "binomial", "genus", "species", "variety", "synonym", "common", "sink","salt",
                    "grass","desert","juniper","oak","seep","oldform","soda","central","western","mountain","elkhorn","teblor","flowermonth","flowercolor",
                    "native","gkrprefer","refcode","comments", "form")
plantkey <- dplyr::select(plantkey, -shortcode, -sink, -salt, -grass, -desert, -juniper, -oak, 
                          -seep, -soda, -central, -western, -mountain, -elkhorn, -teblor, binomial, 
                          -commonfamily, -oldform, -common, -genus, -species, -flowercolor, -comments, -variety, -synonym, -family)
vegtog <- left_join(vegtog, plantkey, by="code")


### JOIN VEGTOG AND FUNCKEY ###
names(funckey) = c("form", "fullform", "lifecycle", "growthhabit")
vegtog <- left_join(vegtog, funckey) %>%
  select(-native, -gkrprefer, -refcode)

### JOIN VEGTOG AND BIOMASS ###

#clean up and rename biomass columns
biomass <- biomass %>%
  select("year", "Site type", "Block", "New Plot ID", "net weight", "season")
names(biomass) = c("year", "sitetype", "plot", "quadrat", "netwt", "wtmonth")
biomass <- biomass %>%
  filter(sitetype=="CR"|sitetype=="EP", !is.na(year), !is.na(plot), !is.na(netwt), !is.na(quadrat), !is.na(wtmonth))%>%
  mutate(wtmonth=ifelse(wtmonth=="Spring", "April", wtmonth)) %>%
  spread(wtmonth, netwt) %>%
  select(-APRIL, -october, -april) ## We lose All the June measurements because their quad #s are 1-8 not "NO215" etc.


#join (this doubles all entries in vegtog, giving them a spring and fall weight row)
vegtog <- left_join(vegtog, biomass)
names(vegtog)

### JOIN VEGTOG AND COWPIES ###

#clean up and rename cowpies
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
  select(-sitetype, -precipblock, -newquadrat, -ID, -pasturetrt, -rodenttrt, -plantID, -exclosure)

vegtog <- vegtog[c("year", "site", "plot", "quadrat", "precinct", "precinctcurrent", "preciptrt", 
                   "grazetrt", "cowpies", "netwt", "wtmonth", "code", "binomial", "count", "cover", "flowermonth", 
                   "form", "fullform", "lifecycle", "growthhabit")]


#remove precip treatments that have irrigation or shelter.  get rid of other unnecessary columns
vegtog <- vegtog %>%
  mutate(preciptrt=tolower(preciptrt)) %>%
  filter(preciptrt!="irrigation", preciptrt!="shelter", !is.na(binomial)) %>%
  #get rid of anything in precinct current that is "NOT OK"
  mutate(precinctcurrent=tolower(precinctcurrent)) %>%
  filter(precinctcurrent!="n not ok", precinctcurrent!="n not ok. active pruning.", 
         precinctcurrent!="n not okay", precinctcurrent!="not ok", precinctcurrent!="p not ok", 
         precinctcurrent!="p not okay")%>%
  select(-June)

# Clean up environment.
rm(cowpies, funckey, plantkey, sitekey, vegdat, biomass)

names(vegtog)

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

str(vegtog)
