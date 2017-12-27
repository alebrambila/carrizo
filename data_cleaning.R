# load libraries to use
library(tidyverse)

# load the data
# make sure you first set your working directory to where the files are stored!
list.files()

#create vegdat, a data frame with collected vegetation data
vegdat <- read.csv("veg plot data all.csv") %>%
  tbl_df()


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

## Rename the columns to a) remove spaces and b) standardizing the name of the column to join vegdat and sitekey by
## vegdat previously had it as "Site.type" and sitekey as "site.type" - R is case sensitive and so these would not have joined
names(vegdat)
names(vegdat) = c("date", "newplotID", "observer", "site", "block", "sitetype", "precipblock", "newquadrat", "preciptrt", "code", "precinct", "comments",
                  "count", "originalorder", "precinctcurrent", "year", "ID", "cover")
names(vegdat)
names(sitekey)
names(sitekey) = c("sitetype", "grazetrt", "pasturetrt", "rodenttrt", "exclosure", "altsitetype")
names(sitekey)


unique(vegdat$code)

#Rename the columns in plantkey to a) remove spaces and b) standardize name of column to vegdat and sitekey
names(plantkey)
names(plantkey) = c("id", "family", )

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

unique(vegtog$code)

ourcheck <- vegtog %>%
  filter(code == "")
unique(vegtog$code)
