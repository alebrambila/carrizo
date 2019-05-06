######################################
## TRENDS / INTERANNUAL VARIABILITY ##
######################################

## Look at trends of richness, cover, biomass over time. 
## Demonstrate that interannual variability is a major structuring force in this system.
## We will need to deal with this to go forwards
## In these visualizations ungrazed drought years are included to show 
## consistency of pattern across treatments. 

###PRE: Quadrat classification changes, climate trend
## Part 1: Richness trends at three levels
## Part 2: Cover trend
## Part 3: Biomass trend
## Part 4: Functional group trends



  
#################### CLIMATE  #####################
ggplot(climate)+geom_bar(aes(x=as.integer(rainyear), y=precip), stat="identity")+
  geom_line(aes(x=as.integer(rainyear), y=MAT/.05))+
  scale_y_continuous(sec.axis = sec_axis(~.*.05, name = "Mean Temperature (celsius)"))+
  labs(x="Growing Season (July-July)", y="Precipitation(mm)")


################################################
# Part 1: RICHNESS TRENDS AT THREE LEVELS:     #
# Full site, 20 paired plots, and 173 quadrats #
################################################

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
ggplot(alphaCover, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Hits/81/Quad") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=80, alpha = .2)+
  annotate("text", x=2013.5, y=75, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))


#########################
# Part 3: BIOMASS TREND #
#########################

gammaBiomass<- vegtog%>% # measured at the plot level, not quadrat
  filter(!is.na(april), !is.na(october))%>%
  group_by(year, grazetrt, precinct) %>% 
    summarize(mean=mean(april), se=calcSE(april))
ggplot(gammaBiomass, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
 geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Biomass/Plot") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=30, alpha = .2)+
  annotate("text", x=2013.5, y=25, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))

###################################
# Part 4: FUNCTIONAL GROUP TRENDS #
###################################

#General Grass Trend
totalStems<- vegtog_vegonly%>%
  group_by(year, grazetrt, precinct, quadrat) %>% 
  summarize(stems=sum(count))

vegtog<-left_join(vegtog, totalStems)
vegtog<-vegtog%>%
  mutate(prop=count/stems)

fgTrend<-subset(vegtog, growthhabit=="grass")%>%
  group_by(year, grazetrt, quadrat, site, precinct) %>%
  summarize(grasscount=sum(count))
fgTrend<-left_join(totalStems, fgTrend)
fgTrend[is.na(fgTrend)] <- 0  
fgTrend<-fgTrend%>%
  group_by(year, grazetrt, precinct)%>% #average all quads for the year
  mutate(prop=grasscount/stems)%>%
  summarize(mean=mean(prop), se=calcSE(prop))
#Visualize
ggplot(fgTrend, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Proportion Grass (excluding bare)") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))
  

# Invasive Grasses Trend
funcTrend<-vegtog%>%
  group_by(year, quadrat, site, native, growthhabit, precinct, grazetrt)%>%
  summarize(count=sum(count)) %>%
  filter(!is.na(native))%>%
  mutate(func=paste(growthhabit, native, sep="_"))
funcTrend<-left_join(funcTrend, totalStems)
funcTrend[is.na(funcTrend)] <- 0  
funcTrendsum<-funcTrend%>%
  group_by(year, grazetrt, precinct, func)%>% #average all quads for the year
  summarize(mean=mean(count), se=calcSE(count))
#Visualize
ggplot(subset(funcTrendsum, func=="grass_i"), aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Cover iGrasses") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))


# Invasive Forbs Trend (Compensatory foil for I.G. in drought)
ifTrend<-subset(vegtog, native=="i"&growthhabit=="forb")%>%
  group_by(year, grazetrt, quadrat, site, precinct) %>%
  summarize(count=sum(count))
ifTrend<-left_join(totalStems, ifTrend)
ifTrend[is.na(ifTrend)] <- 0  
ifTrend<-ifTrend%>%
  group_by(year, grazetrt, precinct)%>% #average all quads for the year
  mutate(prop=count/stems)%>%
  summarize(mean=mean(prop), se=calcSE(prop))
#Visualize
ggplot(ifTrend, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Proportion iForbs (excluding bare)") +
  xlab("Year") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))


