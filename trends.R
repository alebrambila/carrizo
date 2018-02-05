##Look at trends of richness over time.

#Total number of species by graze treatment over the years (ignoring plots)
trends <- vegtog%>%
  group_by(year, grazetrt) %>%
  summarize(richness=length(unique(plantID)))
ggplot(trends, aes(x=year, y=richness, color=grazetrt))+ geom_line() #+ geom_smooth(aes(color=grazetrt))

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
  }

#calculate trend using average of plots over time
trends_plot <- vegtog%>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(richness=length(unique(plantID))) %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness))
ggplot(trends_plot, aes(color=grazetrt))+ geom_line(aes(x=year, y=meanrich)) +
  facet_wrap(~precinct) +geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE))

