## Look at effects of grazing vs species richness. 

#Grazing treatment by quadrat interaction with precinct
grazerich <- vegtog %>%
  group_by(year, grazetrt, quadrat, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(grazerich, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()


#Cowpie density by quadrat interaction with precinct
cowpierich <- vegtog %>%
  #filter the years cowpies weren't counted
  filter((!is.na(cowpietotal)))%>%
  group_by(year, cowpietotal, quadrat, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(cowpierich, aes(x=cowpietotal, y=richness)) +geom_point() + geom_boxplot() #+geom_historgam()
