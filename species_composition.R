redbrome <- subset(vegtog, vegtog$code == "bromad"& vegtog$rodenttrt == "gkr")

ggplot(redbrome, aes(x=interaction(grazetrt, precinct), y=count)) + geom_boxplot() 
ggplot(redbrome, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
ggplot(redbrome, aes(x=year, y=count)) + geom_point(aes(color=grazetrt)) + facet_wrap(~grazetrt)

#read about error structure nlme - nonlinear mixed effect model and lme4 - linear mixed effect model
#book by doug bates on mixed effects and blocks
#block effects means you cant do ANOVA

# check out broom for pulling out model info

l <-lm(count~grazetrt*precinct, data=redbrome)
anova(l)

l2 <-aov(count~grazetrt*precinct, data=redbrome)
summary(l2)

#plot cowpies per block over the years in grazed sites
ggplot(subset(vegtog3, vegtog3$grazetrt =="grazed"), aes(x=site, y=cowpietotal)) +geom_boxplot()
