################################
#GRAZING EFFECTS ON COMPOSITION#
################################


### HORDEUM DEMOGRAPHICS ###

#HORMUR COUNTS BY QUADRAT, includes a LOT of zeroes and ones...
hordeum <- subset(vegtog, vegtog$code == "hormur")%>%
  select(year, quadrat, site, grazetrt, precinct, code, count)
h2<-vegtog %>%
  group_by(year, quadrat, site, grazetrt, precinct) %>%
  summarize()

hordeum<-left_join(h2, hordeum)%>%
  mutate(hormur=count)%>%
  select(-code, -count)
hordeum[is.na(hordeum)] <- 0 

#plot average counts per quadrat, lots of zeroes, then lots of ones...
ggplot(hordeum, aes(x=interaction(precinct, grazetrt), y=hormur)) + geom_boxplot(aes())
#plot all counts over years
ggplot(hordeum, aes(x=year, y=hormur)) + geom_jitter(aes(color=interaction(precinct, grazetrt)))


#AVERAGE TOTAL HORMUR counted in each plot +- SE
hordeumtot <-hordeum %>%
  group_by(year, grazetrt, precinct, site) %>%
  summarize(totalcount=sum(hormur))%>%
  group_by(year, grazetrt, precinct)%>%
  summarize(mean=mean(totalcount), SE=calcSE(totalcount)) 
#plot total count/plot for species over the years.  replication and error bars n=10 plots
ggplot(hordeumtot, aes(x=year, y=mean, color=grazetrt)) +geom_line() +facet_wrap(~precinct) +
  geom_errorbar(aes(x=year, ymin=mean-SE, ymax=mean+SE))


#count proportion of quadrats that have hordeum present and not... does treatment affect the total number?
# when horedum is present, what are the characteristics - or when it is present and >1
#for those that are over 10%, what are their treatment?



### RED BROME DEMOGRAPHICS ###

#REDBROME COUNTS BY QUADRAT 
redbrome <- subset(vegtog, vegtog$code == "bromad") 
#plot average counts per quadrat
ggplot(redbrome, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
#plot all counts over years
ggplot(redbrome, aes(x=year, y=count)) + geom_jitter(aes(color=grazetrt)) + facet_wrap(~precinct)

#AVERAGE TOTAL REDBROME counted in each plot +- SE
redbrometot <-redbrome %>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(totalcount=sum(count))%>%
  group_by(year, grazetrt, precinct)%>%
  summarize(mean=mean(totalcount), SE=calcSE(totalcount)) 
#plot total count/plot for species over the years.  replication and error bars n=10 plots
ggplot(redbrometot, aes(x=year, y=mean, color=grazetrt)) +geom_line() +facet_wrap(~precinct) +
  geom_errorbar(aes(x=year, ymin=mean-SE, ymax=mean+SE))


### SCHISMUS DEMOGRAPHICS ###

#SCHISMUS COUNTS BY QUADRAT 
schismus <- subset(vegtog, vegtog$code == "schara") 
#plot average counts per quadrat
ggplot(schismus, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
#plot all counts over years
ggplot(schismus, aes(x=year, y=count)) + geom_jitter(aes(color=grazetrt)) + facet_wrap(~precinct)


#AVERAGE TOTAL SCHISMUS counted in each plot +- SE
schismustot <-schismus %>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(totalcount=sum(count))%>%
  group_by(year, grazetrt, precinct)%>%
  summarize(mean=mean(totalcount), SE=calcSE(totalcount)) 
#plot total count/plot for species over the years.  replication and error bars n=10 plots
ggplot(schismustot, aes(x=year, y=mean, color=grazetrt)) +geom_line() +facet_wrap(~precinct) +
  geom_errorbar(aes(x=year, ymin=mean-SE, ymax=mean+SE))




#read about error structure nlme - nonlinear mixed effect model and lme4 - linear mixed effect model
#book by doug bates on mixed effects and blocks
#block effects means you cant do ANOVA

# check out broom for pulling out model info

l <-lm(count~grazetrt*precinct, data=redbrome)
anova(l)

l2 <-aov(count~grazetrt*precinct, data=redbrome)
summary(l2)

