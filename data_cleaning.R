## This script exists to bring in all the Carrizo data and merge it into a master dataframe, vegtog.
## It also aims to clean the data taking out unnecessary columns, arranging them and filtering irrelevant
## or bad observations. 

# load libraries to use
library(tidyverse)
library(vegan)


### LOAD THE DATA ###

# make sure you first set your working directory to where the files are stored!
list.files()

#create vegdat, a data frame with collected vegetation data
vegdat <- read.csv("veg plot data all.csv")
#create sitekey, a dataframe that tells you what rodent and grazing treatment each site type from vegdat received
sitekey <- read_csv("site type key.csv")
#create plantkey, a dataframe that tells you what species the species codes in vegdat refer to
plantkey <- read_csv("plant list.csv")
# create funckey, a dataframe that tells you functional characteristics of species in plant list
funckey <- read_csv("plant forms.csv")
#create biomass, a dataframe that tells you biomass measurements
biomass <- read_csv("veg plot biomass all years stacked.csv")
#create cowpies, a dataframe that tells you how many cowpies per plot
cowpies <- read_csv("cowpie counts all years.csv")



### JOIN VEGDAT AND SITE KEY TO CREATE VEGTOG ###

#standardize column names first:
names(vegdat) = c("date", "quadrat", "observer", "site", "plot", "sitetype", "precipblock", "newquadrat", "preciptrt", "code", "precinct", "comments",
                  "count", "originalorder", "precinctcurrent", "year", "ID", "cover")
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")

#join:
vegtog <- left_join(vegdat, sitekey) %>%
  
  # remove columns we don't really need
  select(-date, -observer, -comments, -originalorder, ID, -altsitetype) %>%
  
  # filter non-species codes
  filter(code != "bare", code != "litter", code != "fresh dirt", code != "BARE", code != "HOLE", code != "bphole", code != "hole", code != "GOPHER", code != "MOSS",
         code != "ANT", code != "LITTER", code != "FRESH DIRT", code != "cowpie", code != "") %>%
  
  # make the case consistent for code
  mutate(code = tolower(code)) %>%
  mutate(precipblock = tolower(precipblock)) %>%
  
  # remove the swain pasture that was never grazed
  filter(pasturetrt != "swain")%>%
  # remove rat exclosures in the remaining area, as well as species with no counts or NA counts.
  filter (count !=0, !is.na(count), exclosure != "CW rat excl")


### JOIN VEGTOG AND PLANTKEY ###

#Standardize column names first
names(plantkey) = c("plantID", "family", "commonfamily", "shortcode", "code", "binomial", "genus", "species", "variety", "synonym", "common", "sink","salt",
                    "grass","desert","juniper","oak","seep","oldform","soda","central","western","mountain","elkhorn","teblor","flowermonth","flowercolor",
                    "native","gkrprefer","refcode","comments", "form")

#remove unnecessary column from plantkey
plantkey <- dplyr::select(plantkey, -shortcode, -sink, -salt, -grass, -desert, -juniper, -oak, 
                          -seep, -soda, -central, -western, -mountain, -elkhorn, -teblor, binomial, 
                          -commonfamily, -oldform, -common, -genus, -species, -flowercolor, -comments, -variety, -synonym, -family)

# join vegtog and plantkey
vegtog <- left_join(vegtog, plantkey, by="code")


### JOIN VEGTOG AND FUNCKEY ###

#rename funckey columns
names(funckey) = c("form", "fullform", "lifecycle", "growthhabit")

#join
vegtog <- left_join(vegtog, funckey) %>%
  select(-native, -gkrprefer, -refcode)
vegtog.sim <-vegtog

### JOIN VEGTOG AND BIOMASS ###

#clean up and rename biomass columns
biomass <- biomass %>%
  select("year", "Block", "New Plot ID", "net weight", "season")
names(biomass) = c("year", "plot", "quadrat", "netwt", "wtmonth")
biomass <- biomass %>%
  filter(!is.na(year), !is.na(plot), !is.na(netwt), !is.na(quadrat), !is.na(wtmonth))%>%
  mutate(wtmonth=ifelse(wtmonth=="Spring", "April", wtmonth))


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
         precinctcurrent!="p not okay")

# Clean up environment.
rm(cowpies, funckey, plantkey, sitekey, vegdat)

names(vegtog)

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

str(vegtog)
