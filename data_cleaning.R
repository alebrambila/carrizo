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
