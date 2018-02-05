## I will look at year x various measures

trends <- vegtog%>%
  group_by(year, grazetrt) %>%
  summarize(richness=length(unique(plantID)))
ggplot(trends, aes(x=year, y=richness, color=grazetrt))+ geom_line() #+ geom_smooth(aes(color=grazetrt))

trends_precinct <- vegtog%>%
  group_by(year, grazetrt, precinct) %>%
  summarize(richness=length(unique(plantID)))
ggplot(trends_precinct, aes(x=year, y=richness, color=grazetrt, linetype=precinct))+ geom_line() #+ geom_smooth(aes(color=lifecycle))+ facet_wrap(~grazetrt)

#do this again by plot
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

trends_plot <- vegtog%>%
  group_by(year, grazetrt, precinct, site) %>%
  summarize(richness=length(unique(plantID))) %>% #summarize for mean and sd rather than total richness. calculate standard error (sd/sqrt(richness))
  group_by(year, grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness))
ggplot(trends_plot, aes(color=grazetrt))+ geom_line(aes(x=year, y=meanrich)) +
  facet_wrap(~precinct) +geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE))


trends <- vegtog%>%
  group_by(year, grazetrt, lifecycle, native) %>%
  summarize(richness=length(unique(plantID)))
ggplot(trends, aes(x=year, y=richness, color=native))+ geom_point() #+ geom_smooth(aes(color=native))+ facet_wrap(~grazetrt)

