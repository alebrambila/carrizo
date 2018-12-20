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
sitekey <- read_csv("site type key.csv")    # treatment for each site type in vegdat
plantkey <- read_csv("plant list.csv")      # species codes interpreted
funckey <- read_csv("plant forms.csv")      # associated plant functional characteristics
biomass <- read_csv("veg plot biomass all years stacked.csv") # biomass per quadrat
cowpies <- read_csv("cowpie counts all years.csv")            # cowpies per plot


### JOIN VEGDAT AND SITE KEY TO CREATE VEGTOG ###

names(vegdat) = c("date", "quadrat", "observer", "site", "plot", "sitetype", "precipblock", 
                  "newquadrat", "preciptrt", "code", "precinct", "comments",
                  "count", "originalorder", "precinctcurrent", "year", "ID", "cover")
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")

vegtog <- left_join(vegdat, sitekey) %>%
  select(-date, -observer, -comments, -originalorder, ID, -altsitetype) %>%
  filter(code != "bare", code != "litter", code != "fresh dirt", code != "BARE", code != "HOLE", 
         code != "bphole", code != "hole", code != "GOPHER", code != "MOSS", code != "ANT", 
         code != "LITTER", code != "FRESH DIRT", code != "cowpie", code != "") %>%
  mutate(code = tolower(code)) %>%
  mutate(precipblock = tolower(precipblock)) %>%
  filter(pasturetrt != "swain")%>%                               # remove the ungrazed "swain" pasture
  filter (count !=0, !is.na(count), exclosure != "CW rat excl") %>% # remove GKR exclosure plots and 0/NA 
  mutate(precinct=as.character(precinct)) %>%
  mutate(precinct=ifelse(precinct=="P ","P", precinct)) %>%
  mutate(precinct=as.factor(precinct)) %>%
  mutate(year=as.integer(year))


### JOIN VEGTOG AND PLANTKEY ###
#Standardize column names
names(plantkey) = c("plantID", "family", "commonfamily", "shortcode", "code", "binomial", 
                    "genus", "species", "variety", "synonym", "common", "sink","salt",
                    "grass","desert","juniper","oak","seep","oldform","soda","central",
                    "western", "mountain","elkhorn","teblor","flowermonth","flowercolor",
                    "native","gkrprefer","refcode","comments", "form")

plantkey <- dplyr::select(plantkey, -shortcode, -sink, -salt, -grass, -desert, -juniper, -oak, 
                          -seep, -soda, -central, -western, -mountain, -elkhorn, -teblor, binomial, 
                          -commonfamily, -oldform, -common, -genus, -species, -flowercolor, -comments, 
                          -variety, -synonym, -family)

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
  mutate(wtmonth = tolower(wtmonth)) %>%
  filter(sitetype=="CR"|sitetype=="EP", !is.na(year), !is.na(plot), !is.na(netwt), !is.na(quadrat), !is.na(wtmonth))%>%
  mutate(wtmonth=ifelse(wtmonth=="spring", "april", wtmonth))  %>%
  filter(wtmonth!="june") %>% ## We lose All the June measurements because their quad #s are 1-8 not "NO215" etc.
  spread(wtmonth, netwt)


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
  select(-sitetype, -precipblock, -newquadrat, -ID, -pasturetrt, -rodenttrt, -plantID, -exclosure)

vegtog <- vegtog[c("year", "site", "plot", "quadrat", "precinct", "precinctcurrent", "preciptrt", 
                   "grazetrt", "cowpies", "april", "october", "code", "binomial", "count", "cover", "flowermonth", 
                   "form", "fullform", "lifecycle", "growthhabit")]


# remove precip treatments that have irrigation or shelter
vegtog <- vegtog %>%
  mutate(preciptrt=tolower(preciptrt)) %>%
  filter(preciptrt!="irrigation", preciptrt!="shelter", !is.na(binomial))

# remove quadrats that have shifted between on off mound.
current<-count(vegtog, precinctcurrent) %>%
  mutate(precinctcurrent=tolower(precinctcurrent))%>%
  filter(n<150) %>%
  filter(!(row_number() %in% c(20, 21, 30, 31, 33, 34, 35)))
current<-current$precinctcurrent

vegtog <- vegtog %>%
  mutate(precinctcurrent=tolower(precinctcurrent)) %>%
  filter(!(precinctcurrent %in% current)) %>%
  select(-precinctcurrent, -preciptrt, -cover)%>%
  mutate(native=substr(form, start=1, stop=1))

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
