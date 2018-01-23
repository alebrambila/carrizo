#create some dataframes that summarize vegtog by plot and quadrat that will allow for 
#a direct comparison of grazing vs. non grazing as well as grazing pressure as measured
#by cowpie count in a plot.  This is not so straightforward because cowpies were only
#counted in some years so they will be dealt with differently than simply grazing/nongrazing.

richness_grazing_quad <- vegtog %>%
  #filter(count != 0, !is.na(count)) %>% #what should I do when thre is count and no cover/ vice versa.  this is pretty common
  #filter(cover != 0, !is.na(cover)) %>% #there are also many observations where there is an observation but no count and/or no cover (only 2008 has any cover data)
  filter(exclosure != "CW rat excl") %>% #to ignore where rats change.  just look at difference cows have
  group_by(newplotID, site, year, grazetrt) %>%
  summarize(richness = n())

#this doesnt work because it recounts species that reappear across quadrats
#maybe try something like filtering out non-unique species codes from vegtog and then regrouping this down to the plot level..
richness_grazing_block <- richness_grazing_quad%>%
  group_by(site, year, grazetrt) %>%
  summarize(richness = sum(richness)) #need to replace sum(richness) with something that counts unique values of column, code from vegtog for that specific site
#select from vegtog, choose just columns that give you unique species per plot
#unique function summarize by this

richness_cowpies_byquad <- vegtog %>%
  #filter(count != 0, !is.na(count)) %>% #same count and cover issues as above
  #filter(cover != 0, !is.na(cover)) %>% 
  filter(exclosure != "CW rat excl") %>% 
  filter((year == 2008| year==2009| year==2010| year==2011| year==2016))%>%  #filter years cowpies werent counted
  group_by(newplotID, site, year, grazetrt, cowpietotal) %>%
  summarize(richness = n())

#this doesnt work because it recounts species that reappear across quadrats
richness_cowpies_byblock <- richness_cowpies_byquadrat%>%
  group_by(site, year, grazetrt, cowpietotal) %>%
  summarize(richness = sum(richness)) #same as above