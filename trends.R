##Look at trends of richness over time.

#Total number of species by graze treatment over the years (ignoring plots)
trends <- vegtog%>%
  group_by(year, grazetrt) %>%
  summarize(richness=length(unique(plantID)))
ggplot(trends, aes(x=year, y=richness, color=grazetrt))+ geom_line() #+ geom_smooth(aes(color=grazetrt))


