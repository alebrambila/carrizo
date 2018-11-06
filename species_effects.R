################################
#GRAZING EFFECTS ON COMPOSITION#
################################


### % NATIVE COVER ###
# what is the percent of native cover at the quadrat level across treatment. Only have this data for 2008, useless. 
pctnative <- vegtog %>%
  filter(!is.na(cover))
unique(pctnative$year)

### RED BROME DEMOGRAPHICS ###

#REDBROME COUNTS BY QUADRAT 
redbrome <- subset(vegtog, vegtog$code == "bromad") 
#plot average counts per quadrat
ggplot(redbrome, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
#plot all counts over years
ggplot(redbrome, aes(x=year, y=count)) + geom_jitter(aes(color=grazetrt)) + facet_wrap(~precinct)

#NUMBER OF QUADRATS WITH REDBROME present each year by treatment
redbrometrend <- subset(vegtog, vegtog$code == "bromad") %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(quadcount=length(unique(quadrat))) 
#plot number of quadrats species found in over the years
ggplot(redbrometrend, aes(x=year, y=quadcount, color=grazetrt)) +geom_line() +facet_wrap(~precinct)

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

#NUMBER OF QUADRATS WITH SCHISMUS present each year by treatment
schismustrend <- subset(vegtog, vegtog$code == "schara") %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(quadcount=length(unique(quadrat))) 
#plot number of quadrats species found in over the years
ggplot(schismustrend, aes(x=year, y=quadcount, color=grazetrt)) +geom_line() +facet_wrap(~precinct)

#AVERAGE TOTAL SCHISMUS counted in each plot +- SE
schismustot <-schismus %>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(totalcount=sum(count))%>%
  group_by(year, grazetrt, precinct)%>%
  summarize(mean=mean(totalcount), SE=calcSE(totalcount)) 
#plot total count/plot for species over the years.  replication and error bars n=10 plots
ggplot(schismustot, aes(x=year, y=mean, color=grazetrt)) +geom_line() +facet_wrap(~precinct) +
  geom_errorbar(aes(x=year, ymin=mean-SE, ymax=mean+SE))



### HORDEUM DEMOGRAPHICS ###

#HORMUR COUNTS BY QUADRAT 
hordeum <- subset(vegtog, vegtog$code == "hormur") 
#plot average counts per quadrat
ggplot(hordeum, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
#plot all counts over years
ggplot(hordeum, aes(x=year, y=count)) + geom_jitter(aes(color=grazetrt)) + facet_wrap(~precinct)

#NUMBER OF QUADRATS WITH HORMUR present each year by treatment
hordeumtrend <- subset(vegtog, vegtog$code == "hormur") %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(quadcount=length(unique(quadrat))) 
#plot number of quadrats species found in over the years
ggplot(hordeumtrend, aes(x=year, y=quadcount, color=grazetrt)) +geom_line() +facet_wrap(~precinct)

#AVERAGE TOTAL HORMUR counted in each plot +- SE
hordeumtot <-hordeum %>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(totalcount=sum(count))%>%
  group_by(year, grazetrt, precinct)%>%
  summarize(mean=mean(totalcount), SE=calcSE(totalcount)) 
#plot total count/plot for species over the years.  replication and error bars n=10 plots
ggplot(hordeumtot, aes(x=year, y=mean, color=grazetrt)) +geom_line() +facet_wrap(~precinct) +
  geom_errorbar(aes(x=year, ymin=mean-SE, ymax=mean+SE))




#read about error structure nlme - nonlinear mixed effect model and lme4 - linear mixed effect model
#book by doug bates on mixed effects and blocks
#block effects means you cant do ANOVA

# check out broom for pulling out model info

l <-lm(count~grazetrt*precinct, data=redbrome)
anova(l)

l2 <-aov(count~grazetrt*precinct, data=redbrome)
summary(l2)

