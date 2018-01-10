# load libraries to use
library(tidyverse)

# load the data
# make sure you first set your working directory to where the files are stored!
list.files()

#create vegdat, a data frame with collected vegetation data
vegdat <- read.csv("veg plot data all.csv") %>%
  tbl_df()

#check out vegdat
head(vegdat)
str(vegdat)

#create sitekey, a dataframe that tells you what rodent and grazing treatment each site type from vegdat received
sitekey <- read.csv("site type key.csv") %>%
  tbl_df()

#create plantkey, a dataframe that tells you what species the species codes in vegdat refer to
plantkey <- read.csv("plant list.csv") %>%
  tbl_df()

# create funckey, a dataframe that tells you functional characteristics of species in plant list
funckey <- read.csv("plant forms.csv") %>%
   tbl_df()

## Rename the columns of vegdat and sitekey to a) remove spaces and b) standardizing the name of the column to join vegdat and sitekey by sitetype
## vegdat previously had it as "Site.type" and sitekey as "site.type" - R is case sensitive and so these would not have joined
names(vegdat)
names(vegdat) = c("date", "newplotID", "observer", "site", "block", "sitetype", "precipblock", "newquadrat", "preciptrt", "code", "precinct", "comments",
                  "count", "originalorder", "precinctcurrent", "year", "ID", "cover")
names(vegdat)

names(sitekey)
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")
names(sitekey)


# join the two dataframes. merge() does something similar in base R
vegtog <- left_join(vegdat, sitekey) %>%
  # remove columns we don't really need
  # select() is a common function, we specify that we are using the one in dplyr
  dplyr::select(-date, -observer) %>%
  # remove bare and litter for now
  # in base R subset() and which() do something similar
  filter(code != "bare", code != "litter", code != "fresh dirt", code != "BARE", code != "HOLE", code != "bphole", code != "hole", code != "GOPHER", code != "MOSS",
         code != "ANT", code != "LITTER", code != "FRESH DIRT") %>%
  # make the case consistent for code
  mutate(code = tolower(code)) %>%
  mutate(precipblock = tolower(precipblock)) %>%
  # remove the swain 
  filter(pasturetrt != "swain")%>%
  filter(code != "cowpie")

#Rename the columns in plantkey to a) remove spaces and b) standardize name of column to vegdat and sitekey
names(plantkey)
names(plantkey) = c("plantID", "family", "commonfamily", "shortcode", "code", "binomial", "genus", "species", "variety", "synonym", "common", "sink","salt",
                    "grass","desert","juniper","oak","seep","oldform","soda","central","western","mountain","elkhorn","teblor","flowermonth","flowercolor",
                    "native","gkrprefer","refcode","comments", "form")
#remove unnecessary column from plantkey
plantkey <- dplyr::select(plantkey, -shortcode)
plantkey <- dplyr::select(plantkey, -sink, -salt, -grass, -desert, -juniper, -oak, -seep, -soda, -central, -western, -mountain, -elkhorn, -teblor)
plantkey <- dplyr::select(plantkey, -binomial, -commonfamily, -oldform)
#make sure they went away
names(plantkey)

# check out plant codes in vegtog and plantkey for joining
unique(vegtog$code)
unique(plantkey$code)
# what's up with the blank codes in vegtog
ourcheck <- vegtog %>%
  filter(code == "")
unique(vegtog$code) ##they just forgot to read the plot. It's only three rows.

# join the two dataframes vegtog and plantkey to make vegtog1
vegtog1 <- left_join(vegtog, plantkey, by="code") %>%
  # remove those three blank observances for now
  # in base R subset() and which() do something similar
  filter(code != "")
#check out vegtog1 (combined vegdat, sitekey, and plantkey)
names(vegtog1)
##new columns just got added onto the end. there is now two comment columns because I just merged it by code
##maybe i should merge it by both, or name them vegcomment and plantcomment

##join the two dataframes vegtog1 and funckey to make vegtog2
#first rename funckey columns
names(funckey)
names(funckey) = c("form", "fullform", "lifecycle", "growthhabit")
#join
vegtog2 <- left_join(vegtog1, funckey)
names(vegtog2)

## joining cowpie data to vegtog 2 ## issues ##
## cowpie data is at the plot level, not at the quadrat level
## all quadrats in a plot have the same grazing pressure (precinct and non precinct)
## cowpie counts in cowpie data frame are made up of 7 rows of transect data within each plot
## I need to figure out how to get these seven rows to fit into one row in vegdat (add them up?)

#import and cleanup cowpies
cowpies <- read.csv("cowpie counts all years.csv") %>%
  tbl_df()
cowpies <- dplyr::select(cowpies, -date, -obs) 
names(cowpies) <- c("altsitetype", "altsite", "block", "cowpiecount", "transect", "cowpiecomments", "year", "cowpieID")

##pull out sums of cowpies by site and year into cowpies2
cowpies2 <- aggregate(cowpies$cowpiecount, by=list(cowpies$altsitetype, cowpies$block, cowpies$year),  FUN=sum)

#rename cowpies 2 so I can merge it with vegtog 2
names(cowpies2) = c("altsitetype", "block", "year", "cowpietotal")

#join vegtog2 and cowpies 2 by site and year
#rearrange columns
#remove redundant columns no longer interesting since merged already
#sitetype and block info are both contained within site
vegtog3 <- left_join(vegtog2, cowpies2, by=c("altsitetype", "block", "year"))%>%
  dplyr::select(-sitetype, -altsitetype, -comments.y)
vegtog3 <-vegtog3[c("newplotID", "ID", "year", "site", "block", "precinct","precinctcurrent", 
                    "newquadrat","precipblock","preciptrt", "grazetrt", "pasturetrt", "rodenttrt", 
                    "exclosure", "cowpietotal", "plantID", "code", "family", "genus", "species",
                    "variety", "synonym",
                    "common", "count", "cover", "originalorder", "form", "fullform", "native", 
                    "lifecycle", "growthhabit", "flowermonth", "flowercolor", "gkrprefer", "refcode",
                    "comments.x")]
#rename comments.x to comments
names(vegtog3)[36] <- "comments"

#remove all the old component df that were merged into vegtog3 and rename vegtog3 as vegtog
#i'm doing this now because i'm about to do transformations on vegtog for quesitons i'm interested in other scripts
#this is basically the end of this script - i have a master (maybe tidy) data frame by observation for all observations
##MAYBE STILL HAVE TO REMOVE SOME BAD OBSERVATIONS OR NAS 
rm(cowpies)
rm(funckey)
rm(ourcheck)
rm(plantkey)
rm(sitekey)
rm(vegdat)
rm(vegtog1)
rm(vegtog2)
rm(cowpies2)
vegtog <- vegtog3
rm(vegtog3)
