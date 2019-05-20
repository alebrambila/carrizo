### First round of analysis using only four paired plots: blocks 2, 3, 4 & 6.
### How does grazing affect vegetation as major environmental factors shift (on and off krat mound)
###   specifically, over this time period we are coming out of a major drought - do trajectories differ? 

### Outline

### I. 2014 vs 2017 direct comparison
###   1. Species Richness
###     a. Quadrat Level
###     b. Plot level
###   2. Shannon Diversity
###     a. Quadrat Level (Total + Native)
###     b. Plot Level  (Total + Native)
###   3. Biomass (Quadrat Level)
###   4. Functional group % cover
###   5. Specific native and invasive species
###     a. Relative cover
###     b. Number of quadrats occupied
### II. NMDS Ordination


########################################
## I.  2014 vs 2017 direct comparison ##
########################################
library(codyn)
library(tidyverse)

### 1. Alpha Diversity

#each year by quadrat
alpha.rich <- vegtog_vegonly %>%
  group_by(year, grazetrt, block, quadrat, site, precinct) %>%
  summarize(richness=length(unique(code)))#
alpha.shan <- vegtog_vegonly
alpha.shan <- left_join(alpha.shan, community_diversity(alpha.shan, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shan <- alpha.shan %>%
  group_by(year, site, block, quadrat, precinct, grazetrt, Shannon) %>%
  summarize()%>%
  mutate(alltrt=(paste(grazetrt, precinct, sep="_")))%>%
  ungroup()
alpha.shan<-left_join(alpha.shan, alpha.rich)
alpha.shan<-mutate(alpha.shan, alltrt=factor(alltrt))
alpha.shan<-left_join(alpha.shan, climate, by = c("year" = "rainyear"))%>%
  filter(!is.na(precip))%>%
  mutate(wet.trt=paste(alltrt, wetordry, sep=""))
alpha.shan<-mutate(alpha.shan, wet.trt=factor(wet.trt))

ggplot(subset(alpha.shan, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

ggplot(subset(alpha.shan, !is.na(precip)&wetordry!="normal"), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~wetordry) + #wetordry
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

ggplot(subset(alpha.shan), 
       aes(x=interaction(grazetrt, precinct), 
           y=richness, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

#integrated across years by quadrat
alpha.allyear <-vegtog_vegonly %>%
  group_by(grazetrt, block, quadrat, site, precinct, code)%>%
  summarize(count=sum(count))
alpha.allyear<-left_join(alpha.allyear, community_structure(alpha.allyear, abundance.var="count", replicate.var="quadrat", metric="SimpsonEvenness"))
alpha.allyear <- left_join(alpha.allyear, community_diversity(alpha.allyear,  abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))%>%
  group_by(grazetrt, block, quadrat, site, precinct, Shannon, SimpsonEvenness)%>%
  summarize(richness=length(unique(code)))%>%
  ungroup()%>%
  mutate(alltrt=as.factor(paste(grazetrt, precinct, sep="_")))

ggplot(subset(alpha.allyear), 
       aes(x=interaction(grazetrt, precinct), 
           y=SimpsonEvenness, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

## SHANNON DIVERSITY STATS ##


l <- lme(Shannon~alltrt, random=~1|factor(block),  subset(alpha.shan, year>2006), na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(alltrt="Tukey")))

l <- lme(richness~grazetrt, random=~1|factor(block),  alpha.allyear, na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(alltrt="Tukey")))

### 3. Biomass
biomass<-vegtog%>%
  filter(!is.na(april), !is.na(october))%>%
  group_by(year, site, quadrat, site, precinct, grazetrt, april, block, october, wetordry) %>%
  summarize()%>%
  ungroup()%>%
  mutate(alltrt=paste(grazetrt, precinct, sep=""))%>%
  mutate(wet.trt=paste(alltrt, wetordry, sep=""))%>%
  filter(!is.na(alltrt), !is.na(wetordry))

biomass<-mutate(biomass, wet.trt=as.factor(wet.trt))
biomass<-mutate(biomass, alltrt=as.factor(alltrt))

l <- lme(april~wet.trt, random=~1|factor(block),  subset(biomass), na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(wet.trt="Tukey")))

ggplot(subset(biomass, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=april, color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("April Biomass")

ggplot(subset(biomass, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=october, color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("October Biomass")

### 4. Functional group % cover
func.agg<-vegtog%>%
  group_by(year, quadrat, site, native, growthhabit, precinct, grazetrt)%>%
  summarize(count=sum(count)) %>%
  filter(!is.na(native))%>%
  mutate(func=paste(growthhabit, native, sep="_"))


# Vis option 1
ggplot(subset(funcTrend, (year==2014|year==2017)&(func=="grass_i")), 
       aes(x=interaction(grazetrt, precinct), 
           y=count, color=interaction(grazetrt, precinct))) +
  geom_boxplot(aes(middle=mean(count))) +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_grid(func~year, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Functional Group Cover")

ggplot(subset(funcTrendsum, year==2014|year==2017), aes(x=func, 
                      y=mean, fill=interaction(grazetrt, precinct)))+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position="dodge") +
  facet_wrap(~year)+
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(x="Functional Group", y="%cover")


# Vis option 2
#ggplot(subset(func.agg, year==2014|year==2017),   
#       aes(x=func,
#           y=count, color=interaction(grazetrt, precinct))) +
#  geom_boxplot() +
#geom_jitter(width=.1, color="black") +
#  facet_grid(interaction(grazetrt, precinct)~year, scales="free") + 
#  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
#                     values=c("pink", "brown", "lightblue", "darkblue")) +
#  xlab("Treatment") +
#  ggtitle("Functional Group Cover")


### 5. Specific Native and Invasive species % cover

# a. Percent cover of key weeds. Log-transformed.
ggplot(subset(vegtog, (year==2014|year==2017)&(code=="hormur"|code=="schara"|code=="bromad")), 
       aes(x=interaction(grazetrt, precinct), 
           y=log(count), color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_grid(code~year, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                    values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") + ylab ("log(Count)") + 
  ggtitle("Invasive grass cover")

## 2017 weeds STATS ##
hormur.2017.anova <- aov(count~factor(grazetrt)*factor(precinct)+factor(site), subset(vegtog, (year==2017)&(code=="hormur")))
summary(hormur.2017.anova)
TukeyHSD(hormur.2017.anova) #grazed p-n is significantly different, ungrazed p-n is almost. grazing doesnt affect w/in p/n

bromad.2017.anova <- aov(count~factor(grazetrt)*factor(precinct)+factor(site), subset(vegtog, (year==2017)&(code=="bromad")))
summary(bromad.2017.anova)
TukeyHSD(bromad.2017.anova) #nothing

schara.2017.anova <- aov(count~factor(grazetrt)*factor(precinct)+factor(site), subset(vegtog, (year==2017)&(code=="schara")))
summary(schara.2017.anova)
TukeyHSD(schara.2017.anova) #nothing


# Nothin much, look at with key native too. NOT MUCH NATIVE GRASS AT ALL
# TO DO: eventually work this in to a time block (before during after drought repeated measures ANOVA, but not yet)

##############################################
## II. Community NMDS Ordinations 2014-2017 ##
##############################################
# note: NMDS had to be run with dimensions k=4 to find solutions.
# multivariate view of communities - not worried about who is who

### All Species
plotspec <- dplyr::select(vegtog_vegonly, year, quadrat, code, count)%>%
  mutate(quadyr=paste(quadrat, year, sep = "_"))%>%
  group_by(quadyr, code) %>%
  summarize(meancount=mean(count))
plotspec <-spread(plotspec, code, meancount, fill=0)%>%
  as.data.frame()
rownames(plotspec) <-plotspec$quadyr
plotspec <- dplyr::select(plotspec, -quadyr)

t<-as.tibble(rowSums(plotspec))
t<-rownames_to_column(t)
t<-t%>%
  filter(value==0)%>%
  dplyr::select(1)
t<-as.list(t[,1])
t<-t$rowname

plotspec<-subset(plotspec, !(rownames(plotspec)%in%t))

plotspecNMDS <- metaMDS(plotspec, scale = T, k=4)



data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(data.scores)
data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
  dplyr::select(-quadrat2)%>%
  mutate(quadyr=paste(quadrat, year, sep="_"))

plotkey <- vegtog %>%
  dplyr::select(quadrat, precinct, grazetrt, year)%>%
  mutate(quadyr=paste(quadrat, year, sep = "_"))%>%
  dplyr::select(quadyr, precinct, grazetrt)%>%
  unique()

plotkey2<-subset(plotkey, (plotkey$quadyr%in%rownames(plotspec))) %>%
  unique() %>%
  tbl_df() 

plotkeycheck<-subset(plotkey2, (plotkey2$quadyr%in%rownames(plotspec))) %>%
  group_by(quadyr, grazetrt) %>%
  mutate(repcount = n()) %>%
  filter(repcount == 2)

plotkey <- plotkey2 %>%
  filter(quadyr != unique(plotkeycheck$quadyr)) %>%
  mutate(quadyr2 = quadyr) %>%
  separate(quadyr2, c("quadrat", "year"), sep = "_")%>%
  mutate(block=substr(quadyr, 3, 3))

plotspec$quadyr <- row.names(plotspec)
plotspec2<-plotspec %>%
  mutate(quadyr = row.names(plotspec)) %>%
  filter(quadyr != unique(plotkeycheck$quadyr)) 

row.names(plotspec2) <- plotspec2$quadyr
plotspec2$quadyr <- NULL
plotspec <- plotspec2




data.scores <- left_join(data.scores, plotkey)%>%
  mutate(trt=paste(grazetrt, precinct, sep="_"))
data.scores<- left_join(data.scores, climate, by=c("year"="rainyear"))

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)


##### NMDS VISUALIZATOIN #######
ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores, year==2014|year==2017),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
  stat_ellipse(data=subset(data.scores, year==2014|year==2017), aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)), type='t',size =1)+
   # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                     values=c("pink", "lightblue", "brown",  "darkblue")) +
  coord_equal() +
  theme_bw() + facet_wrap(~year, scales="free")


ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
  stat_ellipse(data=subset(data.scores), aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)), type='t',size =1)+
  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                     values=c("pink", "lightblue", "brown",  "darkblue")) +
  coord_equal() +
  theme_bw() 




###  DISPERSION AND CENTROID STATS - multivairate view of communities
      # multivariate difference: centroids and dispersion between treatments (quad level composition - no blocks)
      # multivariate change: change in species comp from 2007/2014 to 2017
      # PERMANOVA - statistical testing of centroid difference
      # betadisper - dispersion using vegan/to verify my results
** run dispersion output by treatment across precip. 
multivariate_difference(subset(vegtog_vegonly, precinct=="N"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="grazetrt",
                        time.var="year")

multivariate_difference(subset(vegtog_vegonly, year==2014&precinct=="P"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="grazetrt")

multivariate_difference(subset(vegtog_vegonly, year==2014&grazetrt=="grazed"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="precinct")

multivariate_difference(subset(vegtog_vegonly, year==2014&grazetrt=="ungrazed"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="precinct")


# PERMANOVA
# plotkey2<-as.tibble(rownames(plotspec))%>%
#   mutate(quadrat=substr(value, 1,9))%>%
#   mutate(quadyr=value)%>%
#   dplyr::select(-1)
plotkey2<-left_join(plotkey2, plotkey)%>% # new plot key with shifting quadrats
  mutate(year=substr(quadyr, 11, 14))%>%
  mutate(block=substr(quadrat, 3,3))
plotkey2017<-plotkey2%>% #plotkey for only 2017
  filter(year==2017)
plotkey2017n<-plotkey2017%>%#plotkey for 2017 only off mound
  filter(precinct=="N")
plotkey2017p<-plotkey2017%>% #plot key for 2017 only on mound
  filter(precinct=="P")

plotspec2017<-plotspec
plotspec2017<-rownames_to_column(plotspec2017)
plotspec2017<-plotspec2017%>%
  mutate(year=substr(plotspec2017$rowname, 11, 14))%>%
  filter(year==2017)%>%
  dplyr::select(-year)

plotspec2017p<-subset(plotspec2017, plotspec2017$rowname%in%plotkey2017p$quadyr)
plotspec2017n<-subset(plotspec2017, plotspec2017$rowname%in%plotkey2017n$quadyr)

plotspec2017<-column_to_rownames(plotspec2017)
rownames(plotspec2017p)<-plotspec2017p$rowname
plotspec2017p<-dplyr::select(plotspec2017p, -rowname)
rownames(plotspec2017n)<-plotspec2017n$rowname
plotspec2017n<-dplyr::select(plotspec2017n, -rowname)

### Incorrect (no strata) - all years
adonis(plotspec ~ precinct*grazetrt, data=plotkey, perm=1e3)  ### some issue with not equal length datasets

### Correct hypothesis test with strata
adonis(plotspec ~ precinct*grazetrt, data=plotkey, strata=plotkey$block, perm=1e3)


#2017 main effects
adonis(plotspec2017 ~ precinct*grazetrt, data=plotkey2017, strata=plotkey2017$block, perm=1e3) # grazing and precinct significant, but not interaction
#Df SumsOfSqs MeanSqs F.Model      R2   Pr(>F)    
#    precinct           1    1.8246 1.82464  8.8260 0.12010 0.000999 ***
#    grazetrt           1    0.7296 0.72962  3.5293 0.04802 0.003996 ** 
#    precinct:grazetrt  1    0.2344 0.23437  1.1337 0.01543 0.256743    
#    Residuals         60   12.4040 0.20673         0.81645             
#    Total             63   15.1927                 1.00000 

#off precinct test centroids by grazetrt
adonis(plotspec2017n ~ grazetrt, data=plotkey2017n, strata=plotkey2017n$block, perm=1e3) # off mound w/wo grazing=significant diff
#Df SumsOfSqs MeanSqs F.Model      R2   Pr(>F)   
#   grazetrt   1    0.6139 0.61388  4.0682 0.13995 0.001998 **
#   Residuals 25    3.7725 0.15090         0.86005            
#   Total     26    4.3864                 1.00000

#on precinct test centroids by grazetrt
adonis(plotspec2017p ~ grazetrt, data=plotkey2017p, strata=plotkey2017p$block, perm=1e3) # on mound w/wo graizng not significant diff
#          Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
#   grazetrt   1    0.3501 0.35011  1.4197 0.03898 0.0959 .
#   Residuals 35    8.6315 0.24661         0.96102         
#   Total     36    8.9816                 1.00000   


#Dispersion stats
specdist2017<-vegdist(plotspec2017, method="bray")
specdist2017n<-vegdist(plotspec2017n, method="bray")
specdist2017p<-vegdist(plotspec2017p, method="bray")

#2017 disp tests

#precinct main effect
dispersion<-betadisper(specdist2017, group=plotkey2017$precinct)
permutest(dispersion)
#Groups     1 0.15346 0.153460 16.638    999  0.001 ***

#Grazing main effect
dispersion<-betadisper(specdist2017, group=plotkey2017$grazetrt)
permutest(dispersion)
#Groups     1 0.02125 0.021246 1.0835    999  0.295

#interaction effect test
dispersion<-betadisper(specdist2017, group=interaction(plotkey2017$precinct, plotkey2017$grazetrt))
permutest(dispersion)
# Groups     3 0.28245 0.094151 9.9103    999  0.001 ***

#interaction on precinct - grazing
dispersion<-betadisper(specdist2017p, group=plotkey2017p$grazetrt)
permutest(dispersion)
#Groups     1 0.008188 0.0081878 1.2487    999   0.27 NOOOOOOOO

dispersion<-betadisper(specdist2017n, group=plotkey2017n$grazetrt)
permutest(dispersion)
#Groups     1 0.04796 0.047959 3.4495    999  0.066 . NOOOOO



########################
# RACs/abundance clocks- actually communities are changing, who is leading the cause

topspp<-vegtog_vegonly%>%
  group_by(year, grazetrt, precinct, year, code)%>%
  summarize(meancount=mean(count))%>%
  filter(meancount>24.5) %>%
  filter(code!="bare")%>%
  filter(code!="litter")%>%
  filter(code!="bare ")%>%
  filter(code!="litter ")

topspp2<-unique(topspp$code)

vegtog.agg<-vegtog_vegonly%>%
  group_by(year, grazetrt, precinct, year, code, precip)%>%
  summarize(meancount=mean(count), secount=calcSE(count))%>%
  filter(code%in%topspp2)

ggplot(vegtog.agg, aes(x=year, y=meancount))+
  geom_bar(aes(x=as.integer(year), y=precip/50), stat="identity", fill='lightgrey')+
  geom_line(aes(group=code, color=code))+
  geom_errorbar(aes(x=year, ymin=meancount-secount, ymax=meancount+secount, color=code), width=.1)+
  facet_grid(grazetrt~precinct)+
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Annual Precipitation in mm"))+
  ylab("Mean % Cover/Plot")+
  scale_color_hue(direction=3)+
  theme_classic()

vegsum0<-vegtog_vegonly%>%
  group_by(year, code, site, block, grazetrt, precinct)%>%
  summarize(count=sum(count))%>%
  ungroup()%>%
  mutate(alltrt=as.factor(paste(precinct, grazetrt, sep="_")))%>%
  mutate(site.trt=as.factor(paste(site, alltrt, sep="_")))


turnover<-turnover(vegsum0,
                            time.var="year",
                            species.var="code",
                            abundance.var="count",
                            replicate.var="site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  group_by(alltrt, year)%>%
  summarize(mean.turnover=mean(as.numeric(total)), se.turnover=calcSE(as.numeric(total)))

ggplot(turnover)+geom_line(aes(as.factor(year), mean.turnover, group=alltrt, color=alltrt))

rankshift <- rank_shift(vegsum0,
                            time.var = "year",
                            species.var = "code",
                            abundance.var = "count", 
                            replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  group_by(alltrt, year_pair)%>%
  summarize(mean.MRS=mean(MRS), se.MRS=calcSE(MRS))%>%
  mutate(year=substr(year_pair, 6,9))

ggplot(rankshift)+geom_line(aes(as.factor(year_pair), mean.MRS, group=alltrt, color=alltrt))

richsum<-vegsum0%>%
  group_by(year, site, code, alltrt, site.trt)%>%
  summarize(count=sum(count))
richsum <- left_join(richsum, community_diversity(richsum, time.var = "year", abundance.var="count", replicate.var="site.trt", metric = c("Shannon")))%>%
  group_by(year, site, alltrt, Shannon)%>%
  summarize(richness=length(unique(as.factor(code))))%>%
  group_by(year, alltrt)%>%
  summarize(meanrich=mean(richness), meanshan=mean(Shannon))

ggplot(richsum)+geom_line(aes(as.factor(year), meanshan, group=alltrt, color=alltrt))
