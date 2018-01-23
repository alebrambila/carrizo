## Create dataframes that summarize vegtog by plot and quadrat that will allow for 
# a comparison at these levels grazing vs. non grazing as well as grazing pressure as measured.
# by cowpie count in a plot.  
## This is not so straightforward because cowpies were only
#  counted in some years so they will be dealt with differently than simply grazing/nongrazing.

##SUMMARIZING NEW DATA FRAMES

rich_graze_quad <- vegtog %>%
  group_by(newplotID, site, year, grazetrt, precinct, native, lifecycle) %>%
  summarize(richness = n())

#this doesnt work because it recounts species that reappear across quadrats
#maybe try something like filtering out non-unique species codes from vegtog and then regrouping this down to the plot level..
rich_graze_block <- vegtog %>%
  group_by(site, year, grazetrt, precinct) %>%
  summarize(richness = length(unique(plantID))) #this should count up the unique species within each group

rich_cowpie_quad <- vegtog %>%
  filter((!is.na(cowpietotal)))%>%  #filter years cowpies werent counted
  group_by(newplotID, site, year, grazetrt, cowpietotal, precinct, native, lifecycle) %>%
  summarize(richness = n())

rich_cowpie_block <- vegtog %>%
  filter((!is.na(cowpietotal)))%>%  #filter years cowpies werent counted
  group_by(site, year, grazetrt, cowpietotal, precinct, native, lifecycle) %>%
  summarize(richness = length(unique(plantID))) #same as above

##RICHNESS VISUALIZATIONS

# Species Richness vs grazed/ungrazed at the quad level
ggplot(rich_graze_quad, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()+ facet_grid(native~lifecycle)
## this doesnt look good...

# Species Richness vs grazed ungrazed at the plot level
ggplot(rich_graze_block, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()

# Species Richness by cowpie total at the quad level
ggplot(rich_cowpie_quad, aes(cowpietotal, richness)) + geom_jitter(aes(color=native)) +geom_smooth(method=lm, aes(group=native, color=native)) +facet_wrap(~precinct)
ggplot(rich_cowpie_quad, aes(cowpietotal, richness)) + geom_jitter(aes(color=lifecycle)) +geom_smooth(method=lm, aes(group=lifecycle, color=lifecycle)) +facet_wrap(~precinct)

#Species Richness by cowpie total at the plot level
ggplot(rich_cowpie_block, aes(cowpietotal, richness)) + geom_jitter(aes(color=native)) +geom_smooth(method=lm, aes(group=native, color=native)) +facet_wrap(~precinct)
ggplot(rich_cowpie_block, aes(cowpietotal, richness)) + geom_jitter(aes(color=lifecycle)) +geom_smooth(method=lm, aes(group=lifecycle, color=lifecycle)) +facet_wrap(~precinct)

## I'm not sure why these are all giving me such different results... esp between the different cowpie ones
## All the interesting stuff is happening in grazed/ungrazed at very little values, looks like they're identical
