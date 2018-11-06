## Look at effects of grazing vs species richness. 

#Grazing treatment by quadrat (alpha) interaction with precinct
alpha <- vegtog %>%
  filter(year!=2007, year!=2008, year!=2013, year!=2014, year!=2015, year!=2016) %>%
  group_by(year, grazetrt, quadrat, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(alpha, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()
ggplot(alpha, (aes(x=richness))) +geom_histogram()

#Cowpie density by quadrat interaction with precinct
cowpierich <- vegtog %>%
  #filter the years cowpies weren't counted
  filter((!is.na(cowpietotal)))%>%
  group_by(year, cowpietotal, quadrat, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(cowpierich, aes(x=cowpietotal, y=richness)) +geom_point() +geom_smooth() +facet_wrap(~precinct)
