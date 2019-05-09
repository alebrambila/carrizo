######################################
## TRENDS / INTERANNUAL VARIABILITY ##
######################################

## Look at trends of richness, cover, biomass over time. 
## Demonstrate that interannual variability is a major structuring force in this system.
## We will need to deal with this to go forwards
## In these visualizations ungrazed drought years are included to show 
## consistency of pattern across treatments. 

###PRE: Quadrat classification changes, climate trend
## Part 1: Richness trends at three levels (old-skip)
## Part 2: Cover trend
## Part 3: Biomass trend
## Part 4: Functional group trends



library(nlme)  
library(multcomp)

#################### CLIMATE  #####################
clim<-ggplot(climate)+geom_bar(aes(x=as.integer(rainyear), y=precip), stat="identity")+
  geom_line(aes(x=as.integer(rainyear), y=MAT/.05))+
  scale_y_continuous(sec.axis = sec_axis(~.*.05, name = "Mean Temperature (celsius)"))+
  labs(x="Growing Season (July-July)", y="Precipitation(mm)")
clim

################################################
# Part 1: RICHNESS TRENDS AT THREE LEVELS:     #
################################################
#kind of irrelevant now, see richness in subset analysis

# Total 
annUniqueSp <- vegtog%>%  
  group_by(year, grazetrt, precinct) %>%
  summarize(richness=length(unique(code)))
# Plot-Level
gammaUniqueSp<- vegtog%>%
  group_by(year, grazetrt, precinct, site) %>%
  summarize(richness=length(unique(code))) %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness))
# Quadrat-Level 
alphaUniqueSp <- vegtog%>%
  group_by(year, grazetrt, precinct, quadrat) %>%
  summarize(richness=length(unique(code))) %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness))

# Visualize diversity trends in one figure. 
p1<-ggplot(annUniqueSp, aes(x=year, y=richness, color=grazetrt))+ 
  geom_line() + 
  facet_wrap(~precinct) +
  ylab("Total Unique Species") +
  xlab("") + facet_wrap(~precinct) +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=30, alpha = .2)+
  annotate("text", x=2013.5, y=25, label="not grazed", alpha=.6)
p2<-ggplot(gammaUniqueSp, aes(x=year, y=meanrich, color=grazetrt))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE)) +
  facet_wrap(~precinct) +
  ylab("Unique Species/Plot") +
  xlab("") + facet_wrap(~precinct) +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=15, alpha = .2)+
  annotate("text", x=2013.5, y=13, label="not grazed", alpha=.6)
p3<-ggplot(alphaUniqueSp, aes(x=year, y=meanrich, color=grazetrt))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE)) +
  facet_wrap(~precinct) +
  ylab("Unique Species/Quadrat") +
  xlab("Year") + facet_wrap(~precinct) +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=9, alpha = .2)+
  annotate("text", x=2013.5, y=8, label="not grazed", alpha=.6)
ggarrange(p1, p2, p3, nrow=3, ncol=1, common.legend = TRUE)


#######################
# Part 2: COVER TREND #
#######################

alphaCover<- vegtog_vegonly%>% # make a version for each cleaned
  group_by(year, grazetrt, precinct, quadrat) %>% 
  summarize(stems=sum(count))%>% #count by quad
  group_by(year, grazetrt, precinct)%>% #average all quads for the year
  summarize(mean=mean(stems), se=calcSE(stems))
cov<-ggplot(alphaCover, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Hits/81/Quad") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=80, alpha = .2)+
  annotate("text", x=2013.5, y=75, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))
ggarrange(cov, clim)

#########################
# Part 3: BIOMASS TREND #
#########################

gammaBiomass<- vegtog%>% # measured at the plot level, not quadrat
  filter(!is.na(april), !is.na(october))%>%
  group_by(year, grazetrt, precinct) %>% 
    summarize(mean=mean(april), se=calcSE(april))
ggplot()+ 
  geom_bar(data=climate, aes(x=as.integer(rainyear), y=precip/10), stat="identity", fill='lightgrey')+
  geom_line(data=gammaBiomass, aes(x=year, y=mean, color=interaction(grazetrt, precinct))) +
 geom_errorbar(data=gammaBiomass, aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Biomass/Plot") +
  xlab("Year") +
  #annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=30, alpha = .2)+
  #annotate("text", x=2013.5, y=25, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Precipitation in mm"))

## run a two way ANOVA: grazing and mound in 2014, grazing and mound in 2017.  are these differences significant?
vegtog.plot<-vegtog_clim%>%
  group_by(year, site, april, october, grazetrt, precinct, block, precip, quadrat)%>%
  summarize()%>%
  ungroup()%>%
  mutate(grazetrt=as.factor(grazetrt), precinct=as.factor(precinct), alltrt = as.factor(paste(grazetrt, precinct, sep = "_")))

l <- lme(april~factor(grazetrt)*factor(precinct), random =~1|factor(block),  subset(vegtog.plot, year==2017), na.action = na.omit)
anova(l)


l <- lme(april~alltrt, random =~1|factor(block),  subset(vegtog.plot, year==2017), na.action = na.omit)
anova(l)

summary(glht(l, linfct=mcp(alltrt="Tukey")))


biomass.2017.anova <- aov(april~factor(grazetrt)*factor(precinct)+factor(block), subset(vegtog.plot, year==2017))
summary(biomass.2017.anova)
TukeyHSD(biomass.2017.anova)
plot(TukeyHSD(biomass.2017.anova)) #only precinct is significant..., but i'm not taking advantage of paired design, or am i since i have block?

biomass.2014.anova <- aov(april~factor(grazetrt)*factor(precinct)+factor(block), subset(vegtog.plot, year==2014))
summary(biomass.2014.anova)
TukeyHSD(biomass.2014.anova)
plot(TukeyHSD(biomass.2014.anova))

## test for relationship of biomass to precipitation
summary(lm(april~precip, vegtog_clim))    #20% r2
summary(lm(april~legacyOne, vegtog_clim)) #.2% r2

## LME: continuous precip, factor grazed, factor mound
library(nlme)
## NOT WORKING
mm<-lme(april~precip*grazetrt*precinct, random=~1|block, data=vegtog_clim, na.action = na.omit)
summary(mm)

vegtog_clim$alltrt <- as.factor(paste(vegtog_clim$grazetrt, vegtog_clim$precinct, sep = "_"))
mm<-lme(april~precip*alltrt, random=~1|block, data=vegtog_clim, na.action = na.omit)
summary(mm)

ggplot(vegtog_clim, aes(x=precip, y=april, color = interaction(grazetrt, precinct))) + geom_point() + geom_smooth(se=F, method = "lm")
###################################
# Part 4: FUNCTIONAL GROUP TRENDS #
###################################

#General Grass Trend
#totalStems<- vegtog_vegonly%>%
#  group_by(year, grazetrt, precinct, quadrat) %>% 
#  summarize(stems=sum(count))

#vegtog<-left_join(vegtog, totalStems)
#vegtog<-vegtog%>%
#  mutate(prop=count/stems)

#fgTrend<-subset(vegtog, growthhabit=="grass")%>%
#  group_by(year, grazetrt, quadrat, site, precinct) %>%
#  summarize(grasscount=sum(count))
#fgTrend<-left_join(totalStems, fgTrend)
#fgTrend[is.na(fgTrend)] <- 0  
#fgTrend<-fgTrend%>%
#  group_by(year, grazetrt, precinct)%>% #average all quads for the year
#  mutate(prop=grasscount/stems)%>%
#  summarize(mean=mean(prop), se=calcSE(prop))
#Visualize
#grass<-ggplot(fgTrend, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
#  geom_line() +
#  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
#  ylab("Proportion Grass (excluding bare)") +
#  xlab("Year") +
#  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
#  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
#  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))
#ggarrange(grass, clim)

# Invasive Grasses Trend
funcTrend<-vegtog%>%
  group_by(year, quadrat, site, native, growthhabit, precinct, grazetrt, april)%>%
  summarize(count=sum(count)) %>%
  filter(!is.na(native))%>%
  mutate(func=paste(growthhabit, native, sep="_"))
funcTrend<-left_join(funcTrend, totalStems)
funcTrend[is.na(funcTrend)] <- 0  
funcTrendsum<-funcTrend%>%
  group_by(year, grazetrt, precinct, func)%>% #average all quads for the year
  summarize(mean=mean(count), se=calcSE(count))

#Visualize
ggplot()+ 
  geom_bar(data=climate, aes(x=as.integer(rainyear), y=precip/6), stat="identity", fill='lightgrey')+
  geom_line(data=subset(funcTrendsum, func=="grass_i"), aes(x=year, y=mean/.81, color=interaction(grazetrt, precinct))) +
  geom_errorbar(data=subset(funcTrendsum, func=="grass_i"), aes(x=year, ymin=mean/.81-se, ymax=mean/.81+se)) +
  ylab("Percent cover introduced grasses") +
  xlab("Year") +
 # annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
#  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
scale_y_continuous(sec.axis = sec_axis(~.*6, name = "Precipitation in mm"))


## run a two way ANOVA: grazing and mound in 2014, grazing and mound in 2017.  are these differences significant?
anovafunc<-left_join(func.agg, climate, by = c("year" = "rainyear"))

ig.2017.anova <- aov(count~factor(grazetrt)*factor(precinct)+factor(site), subset(anovafunc, year==2017&func=="grass_i"))
summary(ig.2017.anova)
TukeyHSD(ig.2017.anova)
plot(TukeyHSD(ig.2017.anova)) 

ig.2014.anova <- aov(count~factor(grazetrt)*factor(precinct)+factor(site), subset(anovafunc, year==2014&func=="grass_i"))
summary(ig.2014.anova)
TukeyHSD(ig.2014.anova)
plot(TukeyHSD(ig.2014.anova))

## test for relationship of ig to precipitation
summary(lm(count~precip, subset(anovafunc, func=="grass_i")))    #2% r2


func.biomass<-vegtog%>%
  group_by(year, quadrat, site, native, growthhabit, precinct, grazetrt, april)%>%
  summarize(count=as.numeric(sum(count))) %>%
  filter(!is.na(native))%>%
  mutate(func=paste(growthhabit, native, sep="_"))%>%
  filter(func=="grass_i")%>%
  group_by(year, site, func, precinct, grazetrt, april)%>%
  summarize(meancount=as.numeric(mean(count)))
ggplot(data=func.biomass, aes(x=log(meancount), y=april))+
  geom_point(aes(color=interaction(grazetrt, precinct)))+
  geom_smooth(method=lm)

ggplot(data=func.biomass, aes(x=log(meancount), y=april))+
  geom_point(aes(color=interaction(grazetrt, precinct)))+
  geom_smooth(aes(color=interaction(grazetrt, precinct)), method=lm, se = F)

##relationship of ig to biomass
aggFT<-funcTrend%>%
  group_by(year, site, func, precinct, grazetrt, april)%>%
  summarize(count=mean(count))

summary(lm(april~count, subset(aggFT, func=="grass_i")))    #19% r2, basically the same as precip

#mixed model with 
lme(count~as.factor(precip)*as.factor(grazetrt)*as.factor(precinct)+1|as.factor(site)+1|as.factor(year), data=subset(aggFT, func=="grass_i"), na.action = na.omit)

