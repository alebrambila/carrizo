#Look at the effect of grazing on specific species

#Red Brome
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


#Hordeum
hordeum <- subset(vegtog, vegtog$code =="hormur"& vegtog$rodenttrt =="gkr")

ggplot(hordeum, aes(x=interaction(grazetrt, precinct), y=count)) + geom_boxplot() 
ggplot(hordeum, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
ggplot(hordeum, aes(x=year, y=count)) + geom_point(aes(color=grazetrt)) + facet_wrap(~grazetrt)
## looks like much more hordeum in grazed

#Schismus
schismus <- subset(vegtog, vegtog$code =="schara"& vegtog$rodenttrt =="gkr")

ggplot(schismus, aes(x=interaction(grazetrt, precinct), y=count)) + geom_boxplot() 
ggplot(schismus, aes(x=grazetrt, y=count)) + geom_boxplot() + facet_wrap(~precinct)
ggplot(schismus, aes(x=year, y=count)) + geom_point(aes(color=grazetrt)) + facet_wrap(~grazetrt)

