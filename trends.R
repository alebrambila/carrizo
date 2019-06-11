######################################
## TRENDS / INTERANNUAL VARIABILITY ##
######################################

## Look at trends of richness, cover, biomass over time. 
## Demonstrate that interannual variability is a major structuring force in this system.
## We will need to deal with this to go forwards
## In these visualizations ungrazed drought years are included to show 
## consistency of pattern across treatments. 

###PRE: Quadrat classification changes, climate trend
### Part 1: Biomass
### Part 2: Functional groups

library(nlme)  
library(multcomp)
vegtog_vegonly<-mutate(vegtog_vegonly, alltrt=factor(paste(grazetrt, precinct, sep="")))
vegtog<-mutate(vegtog, alltrt=factor(paste(grazetrt, precinct, sep="")))


#prolog - climate viz.
#clim<-ggplot(climate)+geom_bar(aes(x=as.integer(rainyear), y=precip), stat="identity")+
#  geom_line(aes(x=as.integer(rainyear), y=MAT/.05))+
#  scale_y_continuous(sec.axis = sec_axis(~.*.05, name = "Mean Temperature (celsius)"))+
#  labs(x="Growing Season (July-July)", y="Precipitation(mm)")
#clim

# Part a: old-richness
#kind of irrelevant now, see richness in subset analysis

# Quadrat-Level 
#alphaUniqueSp <- vegtog%>%
#  group_by(year, grazetrt, precinct, quadrat) %>%
#  summarize(richness=length(unique(code))) %>%
#  group_by(year, grazetrt, precinct) %>%
#  summarize(meanrich=mean(richness), SE=calcSE(richness))

# Visualize diversity trends in one figure. 
#ggplot(alphaUniqueSp, aes(x=year, y=meanrich, color=grazetrt))+ 
#  geom_line() +
#  geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE)) +
#  facet_wrap(~precinct) +
#  ylab("Unique Species/Quadrat") +
#  xlab("Year") + facet_wrap(~precinct) +
#  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=9, alpha = .2)+
#  annotate("text", x=2013.5, y=8, label="not grazed", alpha=.6)

# Part b: cover

#alphaCover<- vegtog_vegonly%>% # make a version for each cleaned
#  group_by(year, grazetrt, precinct, quadrat) %>% 
#  summarize(stems=sum(count))%>% #count by quad
#  group_by(year, grazetrt, precinct)%>% #average all quads for the year
#  summarize(mean=mean(stems), se=calcSE(stems))
#cov<-ggplot(alphaCover, aes(x=year, y=mean, color=interaction(grazetrt, precinct)))+ 
##  geom_line() +
#  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
#  ylab("Hits/81/Quad") +
#  xlab("Year") +
#  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=80, alpha = .2)+
#  annotate("text", x=2013.5, y=75, label="not grazed", alpha=.6)+
#  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))
#ggarrange(cov, clim)

#########################
# Part 1: BIOMASS TREND #
#########################

gammaBiomass<- vegtog_vegonly%>% # measured at the plot level, not quadrat
  filter(!is.na(april), !is.na(october))%>%
  mutate(april=april*16)%>%
  group_by(year, grazetrt, precinct) %>% 
    summarize(mean=mean(april), se=calcSE(april))

ggplot()+ 
  geom_bar(data=climate, aes(x=as.numeric(rainyear), y=precip/.5), stat="identity", fill='lightgrey')+
  geom_line(data=gammaBiomass, aes(x=as.integer(year), y=mean, color=interaction(grazetrt, precinct))) +
 geom_errorbar(width=.2, data=gammaBiomass, aes(x=year, ymin=mean-se, ymax=mean+se, color=interaction(grazetrt, precinct))) +
  ylab("Biomass (g/m2)") +
  xlab("Year") +
  #annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=30, alpha = .2)+
  #annotate("text", x=2013.5, y=25, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"), 
                     labels=c("Grazed, Off-Mound", 
                              "Not Grazed, Off-Mound", 
                              "Grazed, On-Mound", "Not Grazed, On-Mound"), name="")+
  scale_y_continuous(sec.axis = sec_axis(~.*.5, name = "Precipitation (mm)"))
  
#######################
## BIOMASS STATISTICS
#######################

vegtog.plot<-vegtog%>%
  group_by(year, site, april, october, grazetrt, precinct, block, precip, quadrat, wetordry)%>%
  summarize()%>%
  ungroup()%>%
  mutate(grazetrt=as.factor(grazetrt), precinct=as.factor(precinct), alltrt = as.factor(paste(grazetrt, precinct, sep = "_")))


### biomass across all variables
l <- lme(april~precip + grazetrt*precinct, random =~1|factor(block),  subset(vegtog.plot), na.action = na.omit)
summary(l)

### biomass as a function of treatment (specific years, wet years, all years)
l <- lme(april~precip, random =~1|factor(block),  subset(vegtog.plot), na.action = na.omit)
anova(l)

l <- lme(april~alltrt, random =~1|factor(block),  subset(vegtog.plot, year == 2017), na.action = na.omit)
anova(l)

summary(glht(l, linfct=mcp(alltrt="Tukey")))


l <- lme(april~alltrt, random =~1|factor(block),  subset(vegtog.plot, year==2017), na.action = na.omit)
anova(l)


### biomass as a function of continuous precip var
summary(lm(april~precip, vegtog.plot))    #20% r2

# how biomass-precip relationship interacts with treatment 
library(nlme)
mm<-lme(april~precip*alltrt, random=~1|block, data=vegtog_vegonly, na.action = na.omit)
summary(mm)

F2a<-ggplot(vegtog_vegonly, aes(x=precip, y=april, color = interaction(grazetrt, precinct))) + 
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"), 
                     labels=c("Grazed, Off-Precinct", 
                              "Ungrazed, Off-Precinct", 
                              "Grazed, On-Precinct", "Ungrazed, On-Precinct"), name="")+
  geom_point() + geom_smooth(se=F, method = "lm")+
  ylab("Peak Biomass (units)")+xlab("Precipitation in mm")

############################
# Part 2: FUNCTIONAL GROUP #
############################

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

# Functional group trend
funcTrend<-vegtog_vegonly%>%
  group_by(year, quadrat, block, site, native, growthhabit, precinct, grazetrt, april, wetordry, precip)%>%
  summarize(count=sum(count)) %>%
  filter(!is.na(native))%>%
  mutate(func=factor(paste(growthhabit, native, sep="_")))
funcTrend[is.na(funcTrend)] <- 0  
funcTrendsum<-funcTrend%>%
  group_by(year, grazetrt, precinct, func)%>% #average all quads for the year
  summarize(mean=mean(count), se=calcSE(count))%>%
  ungroup()

#Visualize
F1b<-ggplot()+ 
  geom_bar(data=climate, aes(x=as.integer(rainyear), y=precip/6), stat="identity", fill='lightgrey')+
  geom_line(data=subset(funcTrendsum, func=="grass_i"), aes(x=as.integer(year), y=mean/.81, color=interaction(grazetrt, precinct))) +
  geom_errorbar(width=.2, data=subset(funcTrendsum, func=="grass_i"), aes(x=year, ymin=mean/.81-se, ymax=mean/.81+se, color=interaction(grazetrt, precinct))) +
  ylab("Percent cover introduced grasses") +
  xlab("Year") +
 # annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=1, alpha = .2)+
#  annotate("text", x=2013.5, y=.95, label="not grazed", alpha=.6)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
scale_y_continuous(sec.axis = sec_axis(~.*6, name = "Precipitation in mm"))
F1b

######################### 
## FIGURE 1: treatment effect on biomass (a), and FN group (b) over time.  viz with precip
#########################
ggarrange(F1a, F1b, common.legend=TRUE, legend="bottom")

##########################
### Functional Group stats:
############################
funcTrend<-mutate(funcTrend, alltrt=(paste(grazetrt, precinct, sep="_")))%>%
  ungroup()
funcTrend<-mutate(funcTrend, alltrt=factor(alltrt), grazetrt=factor(grazetrt), precinct=factor(precinct))

### FG as a function of treatment (specific years, wet years, all years)
l <- lme(count~precinct, random =~1|factor(block),  subset(funcTrend, wetordry == "wet" &  func=="grass_i"), na.action = na.omit)
anova(l)
l <- lme(count~alltrt, random =~1|factor(block),  subset(funcTrend, year==2017&func=="grass_i"), na.action = na.omit)
anova(l)

summary(glht(l, linfct=mcp(precinct="Tukey")))

#Gp-Gn sig across all years (4.9, .0048), Gp-Gn dry(4.22, .044), Up-Gp dry(-3.8, .034), precinct in wet (DF168, F-9.6, p=.002) not quite Gp-Gn wet
#2017: Gp-Gn (21.25, P=.004), 
# forbN - Up-Gp2014 (.02, -1.87), nothing otherwise; grassN, grazetrt main in wet (138, 6.56, .01)


## test for relationship of ig to precipitation
summary(lm(count~precip, subset(funcTrend, func=="grass_i")))   
# P=2.52e-6, f-22.7, DF-478, r2=.04

funcTrend$precip2 <- funcTrend$precip*funcTrend$precip
# how ig-precip relationship interacts with treatment 
mm<-lme(count~precip  + alltrt, random=~1|block, data=subset(funcTrend, func=="forb_n"), na.action = na.omit)
summary(mm)
summary(glht(mm, linfct=mcp(alltrt="Tukey")))

mm<-lme(count~  grazetrt + precinct + precip, random=~1|block, data=subset(funcTrend, func=="grass_n"), na.action = na.omit)
summary(mm)


func.biomass<-vegtog_vegonly%>%
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
#################
##FIGURE 3: viz. cover X biomass
################
ggplot(data=func.biomass, aes(x=log(meancount), y=april))+
  geom_point(aes(color=interaction(grazetrt, precinct)))+
  geom_smooth(aes(color=interaction(grazetrt, precinct)), method=lm, se = F)+
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  xlab("log(% Introduced Grasses)")+ylab("Peak Biomass")+theme_classic()

aggFT<-funcTrend%>%
  group_by(year, site, func, precinct, grazetrt, april)%>%
  summarize(count=mean(count))

### Figure 3 stats: biomassXcover
summary(lm(april~count, subset(funcTrend, func=="grass_i"&grazetrt=="ungrazed")))    #19% r2, basically the same as precip
mm<-lme(april~count*grazetrt, random=~1|block, data=subset(funcTrend, func=="grass_i"), na.action = na.omit)
summary(mm)
summary(glht(mm, linfct=mcp(alltrt="Tukey")))


F2a<-ggplot(vegtog_vegonly, aes(x=precip, y=april, color = interaction(grazetrt, precinct))) + 
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"), 
                     labels=c("Grazed, Off-Precinct", 
                              "Ungrazed, Off-Precinct", 
                              "Grazed, On-Precinct", "Ungrazed, On-Precinct"), name="")+
  geom_point() + geom_smooth(se=F, method = "lm")+
  ylab("Peak Biomass (units)")+xlab("Precipitation in mm")

anovafunc<anovafunc%>%
  mutate(precip=precip/10)

F2a<-ggplot(data=subset(anovafunc, func=="grass_i"), aes(x=precip, y=(count))) + 
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"), 
                     labels=c("Grazed, Off-Mound", "Not Grazed, Off-Mound", "Grazed, On-Mound", "Not Grazed, On-Mound"))+
  geom_point(aes(color = interaction(grazetrt, precinct))) + geom_smooth(se=F, method = "lm", color="black")+
  ylab("Percent cover introduced annual grasses")+ggtitle("a")+xlab("")

F2b<-ggplot(data=subset(anovafunc, func=="grass_n"), aes(x=precip, y=(count))) + 
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  geom_point(aes(color = interaction(grazetrt, precinct))) + geom_smooth(method=lm, se=F, color="black")+
  ylab("Percent cover native annual grasses")+xlab("Precipitation (mm)")+ggtitle("b")

F2c<-ggplot(data=subset(anovafunc, func=="forb_n"), aes(x=precip, y=(count))) + 
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  geom_point(aes(color = interaction(grazetrt, precinct))) + geom_smooth(method=lm, formula=y~poly(x, 2), se=F, color="black")+
  ylab("Percent cover native forbs")+ggtitle("c")+xlab("")

anovafunc_uni<-anovafunc%>%
  mutate(precip=precip*precip)
summary(lm(count~precip, subset(anovafunc_uni, func=="forb_n")))

##################
### Figure 2 Viz.: biomass and grass by precip in mm
##################
ggarrange(F2a, F2b, F2c, ncol=3, common.legend=TRUE, legend="right")
