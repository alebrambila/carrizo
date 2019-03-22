######################################
## TRENDS / INTERANNUAL VARIABILITY ##
######################################

## Look at trends of richness, cover, biomass over time. 
## Demonstrate that interannual variability is a major structuring force in this system.
## We will need to deal with this to go forwards
## In these visualizations ungrazed drought years are included to show 
## consistency of pattern across treatments. 

## Part 1: Richness trends at three levels
## Part 2: Cover trend
## Part 3: Biomass trend
## Part 4: Functional group trends
## Part 5: Species invasion trends (proportion and accumulation)

#####################################################
# PRELUDE: WHAT IS GOING ON WITH QUADRATS OVER TIME #
#####################################################

#General quadrat attrition  ## rerun with just quadrats we have at the end. salvage any?
quadCount<-vegtog_nopreciptrt%>%
  group_by(year, grazetrt, precinct) %>%
  summarize(totquadcount=length(unique(quadrat))) 
quadCount2<-vegtog_pre2015%>%
  group_by(year, grazetrt, precinct) %>%
  summarize(totquadcount=length(unique(quadrat))) 
quadCount3<-vegtog1%>%
  group_by(year, grazetrt, precinct) %>%
  summarize(totquadcount=length(unique(quadrat))) 

#Visualize, we have to use proportions not total# of quads:
p2<-ggplot(quadCount3, aes(x=year, y=totquadcount, color=interaction(grazetrt, precinct))) +
  geom_line() +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.25, ymax=48, alpha = .2)+
  annotate("text", x=2013.5, y=45, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="# of Quadrats full vegtog")
p3<-ggplot(quadCount, aes(x=year, y=totquadcount, color=interaction(grazetrt, precinct))) +
  geom_line() +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.25, ymax=48, alpha = .2)+
  annotate("text", x=2013.5, y=45, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="# of Quadrats nopreciptrt")
p4<-ggplot(quadCount2, aes(x=year, y=totquadcount, color=interaction(grazetrt, precinct))) +
  geom_line() +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.25, ymax=48, alpha = .2)+
  annotate("text", x=2013.5, y=45, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="# of Quadrats pre2015")
ggarrange(p2, p3, p4)

#which quadrats are we losing?
lostQuads <- vegtog1 %>%
  group_by(quadrat, grazetrt, precinct)%>%
  summarize(lastyear=max((year)))
ggplot(lostQuads, aes(x=lastyear)) +geom_bar(aes(fill=interaction(grazetrt,precinct)), position="dodge")+
  ggtitle("last year")



vegtog<-vegtog_nopreciptrt #Run this line to visualize with removed precip plots instead of full
vegtog<-vegtog_pre2015 #Run this line to visualize with pre-2015 vegtog instead of full (either way lose 1/2 data)
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

alphaCover<- vegtog_vegonly%>%
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
    summarize(mean=mean(october), se=calcSE(october))
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
totalStems<- vegtog%>%
  group_by(year, grazetrt, precinct, quadrat) %>% 
  summarize(stems=sum(count))
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
igTrend<-subset(vegtog, native=="i"&growthhabit=="grass")%>%
  group_by(year, grazetrt, quadrat, site, precinct) %>%
  summarize(count=sum(count))
igTrend<-left_join(totalStems, igTrend)
igTrend[is.na(igTrend)] <- 0  
igTrend<-igTrend%>%
  group_by(year, grazetrt, precinct)%>% #average all quads for the year
  mutate(prop=count/stems)%>%
  summarize(mean=mean(prop), se=calcSE(prop))
#Visualize
ggplot(igTrend, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
  geom_line() +
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  ylab("Proportion iGrasses (excluding bare)") +
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


###############################################################
# Part 5: SPECIES INVASION TRENDS (Hordeum, Schismus, Bromus) #  
###############################################################
#from here on out it doesnt make as much sense to do purged and compare grazed and ungrazed because they have different numbers of quadrats.  Have to do it all proportionally within treatment

## PROPORTION OF QUADRATS WITH KEY WEEDS
HquadCount <- subset(vegtog, vegtog$code == "hormur") %>%
  group_by(year, grazetrt, precinct) %>%
  mutate(hquadcount=length(unique(quadrat))) %>%
  group_by(year, grazetrt, precinct, hquadcount) %>%
  summarize()
SquadCount <- subset(vegtog, vegtog$code == "schara") %>%
  group_by(year, grazetrt, precinct) %>%
  mutate(squadcount=length(unique(quadrat))) %>%
  group_by(year, grazetrt, precinct, squadcount) %>%
  summarize()
BquadCount <- subset(vegtog, vegtog$code == "bromad") %>%
  group_by(year, grazetrt, precinct) %>%
  mutate(bquadcount=length(unique(quadrat))) %>%
  group_by(year, grazetrt, precinct, bquadcount) %>%
  summarize()
quadCount<-left_join(quadCount, HquadCount)
quadCount<-left_join(quadCount, BquadCount)
quadCount<-left_join(quadCount, SquadCount)
quadCount[is.na(quadCount)] <- 0 
quadProp<-quadCount %>%
  mutate(hormur=hquadcount/totquadcount, 
         bromad=bquadcount/totquadcount, 
         schara=squadcount/totquadcount) %>%
  select(-hquadcount, -totquadcount, -squadcount, -bquadcount)
qpLong<-gather(quadProp, key="species", value="propQuads", 
               hormur, bromad, schara)
#Visualize
ggplot(qpLong, aes(x=year, y=propQuads, color=interaction(grazetrt, precinct))) +
  geom_line() +
  facet_wrap(~species, scales="free") +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=.4, ymax=.5, alpha = .2)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(y="Proportion of Quadrats")



## QUADRAT ACCUMULATIONS: i.e. how many quadrats has the species ever been seen in?
weedSpread <- subset(vegtog, code=="hormur"|code=="schara"|code=="bromad") %>%
  select(year, quadrat, site, grazetrt, precinct, code) %>%
  group_by(quadrat, site, code, grazetrt, precinct) %>%
  summarize(firstyear=min(year)) %>% #calculate first year a species was seen in each quadrat
  group_by(firstyear, grazetrt, precinct, code) %>%
  summarize(newquads=n())%>% #how many new quadrats were added that year
  ungroup()%>%
  mutate(firstyear=as.character(firstyear))%>%
  mutate(firstyear=paste("y", firstyear, sep=""))%>%
  spread(key=firstyear, value=newquads, fill=0)%>%
  mutate(y2008=y2008+y2007, y2009=y2009+y2008, y2010=y2010+y2009,
         y2011=y2011+y2010, y2012=y2012+y2011, y2013=y2013+y2012,
         y2014=y2014+y2013, y2015=y2015+y2014, y2016=y2016+y2015,
         y2017=y2017+y2016)%>%  #add all previous years to each years' new count
  gather(key="year", value="numQuads", 
         y2007, y2008, y2009, y2010, y2011, y2012,
         y2013, y2014, y2015, y2016, y2017) %>%
  mutate(year=substr(year, start=2, stop=5), year=as.integer(year))
# Repeat by site
weedSpreadG <- subset(vegtog, code=="hormur"|code=="schara"|code=="bromad") %>%
  select(year,  site, grazetrt, precinct, code) %>%
  group_by( site, code, grazetrt, precinct) %>%
  summarize(firstyear=min(year)) %>%
  group_by(firstyear, grazetrt, precinct, code) %>%
  summarize(newsite=n())%>%
  ungroup()%>%
  mutate(firstyear=as.character(firstyear))%>%
  mutate(firstyear=paste("y", firstyear, sep=""))%>%
  spread(key=firstyear, value=newsite, fill=0)%>%
  mutate(y2008=y2008+y2007, y2009=y2009+y2008,
         y2011=y2011+y2009, y2012=y2012+y2011, y2016=y2016+y2012,
         y2017=y2017+y2016)%>%
  gather(key="year", value="numSite", 
         y2007, y2008, y2009, y2011, y2012,
         y2016, y2017) %>%
  mutate(year=substr(year, start=2, stop=5), year=as.integer(year))


#Visualize quadrat accumulation curves
ggplot(weedSpread) + 
  geom_line(aes(x=year, y=numQuads,
                color=interaction(grazetrt, precinct))) +
  facet_wrap(~code)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue")) +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=5, ymax=52, alpha = .2)+
  labs(y="Number of unique quadrats ever occupied", x="Year")+
  ggtitle("Weed spread though quadrat accumulation")


#Visualize site accumulation curves
ggplot(weedSpreadG) + 
  geom_line(aes(x=year, y=numSite,
                color=interaction(grazetrt, precinct))) +
  facet_wrap(~code)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue")) +
  labs(y="Number of unique sites ever occupied", x="Year")+
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=4, ymax=10, alpha = .2)+
  ggtitle("Weed spread though site accumulation")

