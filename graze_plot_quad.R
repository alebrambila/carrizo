## Create dataframes that summarize vegtog by plot and quadrat that will allow for 
# a comparison at these levels grazing vs. non grazing as well as grazing pressure as measured.
# by cowpie count in a plot.  
## This is not so straightforward because cowpies were only
#  counted in some years so they will be dealt with differently than simply grazing/nongrazing.

rich_graze_quad <- vegtog %>%
  filter(count !=0, !is.na(count)) %>% #filter count NAs and zeroes (not really an observation)
  # filter(!is.na(cover)) %>% #only 2008 has cover data
  filter(exclosure != "CW rat excl") %>% #to ignore where rats change.  just look at difference cows have
  group_by(newplotID, site, year, grazetrt) %>%
  summarize(richness = n())

#this doesnt work because it recounts species that reappear across quadrats
#maybe try something like filtering out non-unique species codes from vegtog and then regrouping this down to the plot level..
rich_graze_block <- vegtog %>%
  filter(count !=0, !is.na(count)) %>% #filter count NAs and zeroes (not really an observation)
  # filter(!is.na(cover)) %>% #only 2008 has cover data
  filter(exclosure != "CW rat excl") %>% #to ignore where rats change.  just look at difference cows have
  group_by(site, year, grazetrt) %>%
  summarize(richness = length(unique(plantID))) #this should count up the unique species within each group

rich_cowpie_quad <- vegtog %>%
  filter(count !=0, !is.na(count)) %>% #filter count NAs and zeroes (not really an observation)
  # filter(!is.na(cover)) %>% #only 2008 has cover data
  filter(exclosure != "CW rat excl") %>% 
  filter((year == 2008| year==2009| year==2010| year==2011| year==2016))%>%  #filter years cowpies werent counted
  group_by(newplotID, site, year, grazetrt, cowpietotal) %>%
  summarize(richness = n())

rich_cowpie_block <- vegtog %>%
  filter(count !=0, !is.na(count)) %>% #filter count NAs and zeroes (not really an observation)
  # filter(!is.na(cover)) %>% #only 2008 has cover data
  filter(exclosure != "CW rat excl") %>% #to ignore where rats change.  just look at difference cows have
  group_by(site, year, grazetrt, cowpietotal) %>%
  summarize(richness = length(unique(plantID))) #same as above