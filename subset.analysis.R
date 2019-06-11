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
### II. NMDS Ordination, individual species, turnover etc. 


########################################
## I.  2014 vs 2017 direct comparison ##
########################################
library(codyn)
library(tidyverse)
############################
### 1. Alpha Diversity
#############################
#each year by quadrat
alpha.rich <- vegtog_vegonly %>%
  group_by(year, grazetrt, block, quadrat, site, precinct) %>%
  summarize(richness=length(unique(code)))#
alpha.shan <- vegtog_vegonly
alpha.shan <- left_join(alpha.shan, community_diversity(alpha.shan, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shan<-left_join(alpha.shan, community_structure(alpha.shan, time.var="year", abundance.var="count", replicate.var="quadrat", metric=c("SimpsonEvenness")))
alpha.shan <- alpha.shan %>%
  group_by(year, site, block, quadrat, precinct, grazetrt, Shannon, SimpsonEvenness) %>%
  summarize()%>%
  mutate(alltrt=(paste(grazetrt, precinct, sep="_")))%>%
  ungroup()
alpha.shan<-left_join(alpha.shan, alpha.rich)
alpha.shan<-mutate(alpha.shan, alltrt=factor(alltrt))
alpha.shan<-mutate(alpha.shan, grazetrt=factor(grazetrt))
alpha.shan<-left_join(alpha.shan, climate, by = c("year" = "rainyear"))%>%
  filter(!is.na(precip))%>%
  mutate(wet.trt=paste(alltrt, wetordry, sep=""))
alpha.shan<-mutate(alpha.shan, wet.trt=factor(wet.trt))

#2014/2017
ggplot(subset(alpha.shan, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  #geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

#wetordry
ggplot(subset(alpha.shan, !is.na(precip)), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~wetordry) + #wetordry
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

#alltime
ggplot(subset(alpha.shan), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")
ggplot(alpha.shan, aes(x=grazetrt, y=Shannon)) + 
  geom_boxplot()+facet_wrap(~block)

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
alpha.allyear<-mutate(alpha.allyear, grazetrt=factor(grazetrt))


ggplot(subset(alpha.allyear), 
       aes(x=interaction(grazetrt, precinct), 
           y=richness, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Shannon Diversity")

ggplot(alpha.shan, 
       aes(x=precip, 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_point() + geom_smooth(method=lm, se=F)+
  scale_color_manual(labels=c("Grazed, Off-Mound", "Not Grazed, Off-Mound", "Grazed, On-Mound", "Not Grazed, On-Mound"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  ylab("Shannon Diversity") +xlab("Precipitation (mm)")




## SHANNON DIVERSITY STATS ##

#not summed across years
l <- lme(Shannon~grazetrt, random=~1|factor(block),  subset(alpha.shan), na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(grazetrt="Tukey")))

#summed across years
l <- lme(richness~grazetrt, random=~1|factor(block),  alpha.allyear, na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(grazetrt="Tukey")))

#rel to precip continuous
summary(lm(Shannon~precip, data=alpha.shan))
ggplot(alpha.shan, aes(x=precip, y=Shannon)) +geom_point(aes(color=alltrt)) +geom_smooth(method="lm", aes(color=alltrt))

mm<- lme(Shannon~precip*grazetrt*precinct, random=~1|factor(block),  alpha.shan, na.action = na.omit)
summary(mm)

##gamma
gammadiv<-vegtog_vegonly%>%
  group_by(year, grazetrt, block, site, precinct) %>%
  summarize(richness=length(unique(code)))%>%
  ungroup()%>%
  mutate(grazetrt=factor(grazetrt))%>%
  mutate(alltrt=as.factor(paste(grazetrt, precinct, sep="_")))

l <- lme(richness~alltrt, random=~1|factor(block),  gammadiv, na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(alltrt="Tukey")))

ggplot(gammadiv, aes(x=alltrt, y=richness)) + geom_boxplot()

gamma.allyear <-vegtog_vegonly %>%
  group_by(grazetrt, block, site, precinct, code)%>%
  summarize(count=sum(count))
gamma.allyear<-left_join(gamma.allyear, community_structure(gamma.allyear, abundance.var="count", replicate.var="site", metric="SimpsonEvenness"))
gamma.allyear <- left_join(gamma.allyear, community_diversity(gamma.allyear,  abundance.var="count", replicate.var="site", metric = c("Shannon")))%>%
  group_by(grazetrt, block, site, precinct, Shannon, SimpsonEvenness)%>%
  summarize(richness=length(unique(code)))%>%
  ungroup()%>%
  mutate(alltrt=as.factor(paste(grazetrt, precinct, sep="_")))
gamma.allyear<-mutate(gamma.allyear, grazetrt=factor(grazetrt))

ggplot(gamma.allyear, aes(x=alltrt, y=richness)) + geom_boxplot() +geom_point()

l <- lme(richness~grazetrt, random=~1|factor(block),  subset(gamma.allyear, precinct=="P"), na.action = na.omit)
anova(l)
summary(glht(l, linfct=mcp(grazetrt="Tukey")))

#gamma summary across all years
gammasum<-vegtog_vegonly%>%
  group_by(grazetrt, precinct, year)%>%
  mutate(yrrichness=length(unique(code)))%>%
  group_by(grazetrt, precinct)%>%
  summarize(meanrich=mean(yrrichness), alltimerich=length(unique(code)))

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
func.agg<-vegtog_vegonly%>%
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
  dplyr::select(quadrat, precinct, grazetrt, year, wetordry, extremeyear)%>%
  mutate(quadyr=paste(quadrat, year, sep = "_"))%>%
  dplyr::select(quadyr, precinct, grazetrt, wetordry, extremeyear)%>%
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
  mutate(trt=paste(grazetrt, precinct, sep="_"))%>%
  mutate(year=as.numeric(year))
data.scores<- left_join(data.scores, climate, by=c("year"="rainyear"))

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)


##### NMDS VISUALIZATOIN #######
ggplot() +
 # geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores, year==2014|year==2017),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
  stat_ellipse(data=subset(data.scores, year==2014|year==2017), aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)), type='t',size =1)+
  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                     values=c("pink", "lightblue", "brown",  "darkblue")) +
  coord_equal() +
  theme_classic() + facet_wrap(~year, scales="free")+
  ggtitle("d, e ")+
  scale_x_continuous(limits = c(-2.5, 2.5))+scale_y_continuous(limits = c(-2.5, 2.5))

#all.years
ggplot() +
#  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.3) +
  geom_jitter(color="darkblue", data=subset(data.scores, trt=="ungrazed_P"),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
  stat_ellipse(color="darkblue", data=subset(data.scores, trt=="ungrazed_P"), aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)), type='t',size =1)+
  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
 
  coord_equal() +
  theme_classic() +  scale_x_continuous(limits = c(-2.5, 2.5))+scale_y_continuous(limits = c(-2.5, 2.5))

  

#wet or dry
ggplot() +
  #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores, wetordry.y=="wet"|wetordry.y=="dry"),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
  stat_ellipse(data=subset(data.scores, wetordry.y=="wet"|wetordry.y=="dry"), aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)), type='t',size =1)+
  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                     values=c("pink", "lightblue", "brown",  "darkblue")) +
  coord_equal() +
  theme_classic() +facet_wrap(~wetordry.y)+
  ggtitle("b, c")+
  scale_x_continuous(limits = c(-2.5, 2.5))+scale_y_continuous(limits = c(-2.5, 2.5))

ggarrange(Figure4a, Figure4bc, Figure4de, common.legend=TRUE, legend.position="bottom")

###  DISPERSION AND CENTROID STATS - multivairate view of communities
# multivariate difference: centroids and dispersion between treatments (quad level composition - no blocks)
# multivariate change: change in species comp from 2007/2014 to 2017
# PERMANOVA - statistical testing of centroid difference
# betadisper - dispersion using vegan/to verify my results

#choose your own subset/treatment var
multivariate_difference(subset(vegtog_vegonly0, year==2017), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadyr",
                        treatment.var="precinct")



# PERMANOVAS

# make all the plot keys
plotkey2<-left_join(plotkey2, plotkey)%>% # new plot key with shifting quadrats
  mutate(year=substr(quadyr, 11, 14))%>%
  mutate(block=substr(quadrat, 3,3))

plotkeyn<-plotkey%>% #
  filter(precinct=="N")
plotkeyp<-plotkey%>% #
  filter(precinct=="P")
plotkeyg<-plotkey%>% #
  filter(grazetrt=="grazed")
plotkeyu<-plotkey%>% #
  filter(grazetrt=="ungrazed")


plotkey2014<-plotkey%>% #
  filter(year==2014)
plotkey2014n<-plotkey2014%>% #
  filter(precinct=="N")
plotkey2014p<-plotkey2014%>% #
  filter(precinct=="P")
plotkey2014g<-plotkey2014%>% #
  filter(grazetrt=="grazed")
plotkey2014u<-plotkey2014%>% #
  filter(grazetrt=="ungrazed")

plotkey2017<-plotkey%>% #
  filter(year==2017)
plotkey2017n<-plotkey2017%>%#p
  filter(precinct=="N")
plotkey2017p<-plotkey2017%>% #
  filter(precinct=="P")
plotkey2017g<-plotkey2017%>%#
  filter(grazetrt=="grazed")
plotkey2017u<-plotkey2017%>% #
  filter(grazetrt=="ungrazed")

plotkeywet<-plotkey%>% #plotkey for only 2017
  filter(extremeyear=="wet")
plotkeywetn<-plotkeywet%>%#plotkey for 2017 only off mound
  filter(precinct=="N")
plotkeywetp<-plotkeywet%>% #plot key for 2017 only on mound
  filter(precinct=="P")
plotkeywetg<-plotkeywet%>%#plotkey for 2017 only off mound
  filter(grazetrt=="grazed")
plotkeywetu<-plotkeywet%>% #plot key for 2017 only on mound
  filter(grazetrt=="ungrazed")

plotkeydry<-plotkey%>% #plotkey for only 2017
  filter(extremeyear=="dry")
plotkeydryn<-plotkeydry%>%#plotkey for 2017 only off mound
  filter(precinct=="N")
plotkeydryp<-plotkeydry%>% #plot key for 2017 only on mound
  filter(precinct=="P")
plotkeydryg<-plotkeydry%>%#plotkey for 2017 only off mound
  filter(grazetrt=="grazed")
plotkeydryu<-plotkeydry%>% #plot key for 2017 only on mound
  filter(grazetrt=="ungrazed")

#make all the plotspecs
plotspec2017<-plotspec
plotspec2014<-plotspec
plotspecwet<-plotspec
plotspecdry<-plotspec

plotspecn <-subset(plotspec, rownames(plotspec)%in%plotkeyn$quadyr)
plotspecp <-subset(plotspec, rownames(plotspec)%in%plotkeyp$quadyr)
plotspecg <-subset(plotspec, rownames(plotspec)%in%plotkeyg$quadyr)
plotspecu <-subset(plotspec, rownames(plotspec)%in%plotkeyu$quadyr)

plotspec2017a<-subset(plotspec, rownames(plotspec2017)%in%plotkey2017$quadyr)
plotspec2017n<-subset(plotspec2017, rownames(plotspec2017)%in%plotkey2017n$quadyr)
plotspec2017p<-subset(plotspec2017, rownames(plotspec2017)%in%plotkey2017p$quadyr)
plotspec2017g<-subset(plotspec2017, rownames(plotspec2017)%in%plotkey2017g$quadyr)
plotspec2017u<-subset(plotspec2017, rownames(plotspec2017)%in%plotkey2017u$quadyr)

plotspec2014a<-subset(plotspec, rownames(plotspec2014)%in%plotkey2014$quadyr)
plotspec2014n<-subset(plotspec2014, rownames(plotspec2014)%in%plotkey2014n$quadyr)
plotspec2014p<-subset(plotspec2014, rownames(plotspec2014)%in%plotkey2014p$quadyr)
plotspec2014g<-subset(plotspec2014, rownames(plotspec2014)%in%plotkey2014g$quadyr)
plotspec2014u<-subset(plotspec2014, rownames(plotspec2014)%in%plotkey2014u$quadyr)

plotspecweta<-subset(plotspec, rownames(plotspecwet)%in%plotkeywet$quadyr)
plotspecwetn<-subset(plotspecwet, rownames(plotspecwet)%in%plotkeywetn$quadyr)
plotspecwetp<-subset(plotspecwet, rownames(plotspecwet)%in%plotkeywetp$quadyr)
plotspecwetg<-subset(plotspecwet, rownames(plotspecwet)%in%plotkeywetg$quadyr)
plotspecwetu<-subset(plotspecwet, rownames(plotspecwet)%in%plotkeywetu$quadyr)

plotspecdrya<-subset(plotspec, rownames(plotspecdry)%in%plotkeydry$quadyr)
plotspecdryn<-subset(plotspecdry, rownames(plotspecdry)%in%plotkeydryn$quadyr)
plotspecdryp<-subset(plotspecdry, rownames(plotspecdry)%in%plotkeydryp$quadyr)
plotspecdryg<-subset(plotspecdry, rownames(plotspecdry)%in%plotkeydryg$quadyr)
plotspecdryu<-subset(plotspecdry, rownames(plotspecdry)%in%plotkeydryu$quadyr)


# analysis..
### all yr
adonis(plotspec ~ precinct*grazetrt, data=plotkey, strata=plotkey$block, perm=1e3) # nothing significant
adonis(plotspecn ~ grazetrt, data=plotkeyn, strata=plotkeyn$block, perm=1e3) # not 
adonis(plotspecp ~ grazetrt, data=plotkeyp, strata=plotkeyp$block, perm=1e3) # not
adonis(plotspecg ~ precinct, data=plotkeyg, strata=plotkeyg$block, perm=1e3) # not
adonis(plotspecu ~ precinct, data=plotkeyu, strata=plotkeyu$block, perm=1e3) # not

###wet
adonis(plotspecweta ~ precinct*grazetrt, data=plotkeywet, strata=plotkeywet$block, perm=1e3) # not normal, yes extreme graze main
adonis(plotspecwetn ~ grazetrt, data=plotkeywetn, strata=plotkeywetn$block, perm=1e3) # not
adonis(plotspecwetp ~ grazetrt, data=plotkeywetp, strata=plotkeywetp$block, perm=1e3) # not
adonis(plotspecwetg ~ precinct, data=plotkeywetg, strata=plotkeywetg$block, perm=1e3) # not
adonis(plotspecwetu ~ precinct, data=plotkeywetu, strata=plotkeywetu$block, perm=1e3) # not

### dry
adonis(plotspecdrya ~ precinct*grazetrt, data=plotkeydry, strata=plotkeydry$block, perm=1e3) # not normal or extreme
adonis(plotspecdryn ~ grazetrt, data=plotkeydryn, strata=plotkeydryn$block, perm=1e3) # not
adonis(plotspecdryp ~ grazetrt, data=plotkeydryp, strata=plotkeydryp$block, perm=1e3) # not
adonis(plotspecdryg ~ precinct, data=plotkeydryg, strata=plotkeydryg$block, perm=1e3) # not
adonis(plotspecdryu ~ precinct, data=plotkeydryu, strata=plotkeydryu$block, perm=1e3) # not

### 2014 
adonis(plotspec2014a ~ precinct*grazetrt, data=plotkey2014, strata=plotkey2014$block, perm=1e3) #only grazetrt sig, no int
adonis(plotspec2014n ~ grazetrt, data=plotkey2014n, strata=plotkey2014n$block, perm=1e3) # not
adonis(plotspec2014p ~ grazetrt, data=plotkey2014p, strata=plotkey2014p$block, perm=1e3) # not
adonis(plotspec2014g ~ precinct, data=plotkey2014g, strata=plotkey2014g$block, perm=1e3) # not
adonis(plotspec2014u ~ precinct, data=plotkey2014u, strata=plotkey2014u$block, perm=1e3) #not

#2017 main effects
adonis(plotspec2017a ~ precinct*grazetrt, data=plotkey2017, strata=plotkey2017$block, perm=1e3) #both significant, no interaction
adonis(plotspec2017n ~ grazetrt, data=plotkey2017n, strata=plotkey2017n$block, perm=1e3) # sig
adonis(plotspec2017p ~ grazetrt, data=plotkey2017p, strata=plotkey2017p$block, perm=1e3) # sig
adonis(plotspec2017g ~ precinct, data=plotkey2017g, strata=plotkey2017g$block, perm=1e3) # sig
adonis(plotspec2017u ~ precinct, data=plotkey2017u, strata=plotkey2017u$block, perm=1e3) # sig

      
#distances
specdist<-vegdist(plotspec, method="bray")

specdistn<-vegdist(plotspecn, method="bray")
specdistp<-vegdist(plotspecp, method="bray")
specdistg<-vegdist(plotspecg, method="bray")
specdistu<-vegdist(plotspecu, method="bray")

specdist2017a<-vegdist(plotspec2017a, method="bray")
specdist2017n<-vegdist(plotspec2017n, method="bray")
specdist2017p<-vegdist(plotspec2017p, method="bray")
specdist2017g<-vegdist(plotspec2017g, method="bray")
specdist2017u<-vegdist(plotspec2017u, method="bray")

specdist2014a<-vegdist(plotspec2014a, method="bray")
specdist2014n<-vegdist(plotspec2014n, method="bray")
specdist2014p<-vegdist(plotspec2014p, method="bray")
specdist2014g<-vegdist(plotspec2014g, method="bray")
specdist2014u<-vegdist(plotspec2014u, method="bray")

specdistweta<-vegdist(plotspecweta, method="bray")
specdistwetn<-vegdist(plotspecwetn, method="bray")
specdistwetp<-vegdist(plotspecwetp, method="bray")
specdistwetg<-vegdist(plotspecwetg, method="bray")
specdistwetu<-vegdist(plotspecwetu, method="bray")

specdistdrya<-vegdist(plotspecdrya, method="bray")
specdistdryn<-vegdist(plotspecdryn, method="bray")
specdistdryp<-vegdist(plotspecdryp, method="bray")
specdistdryg<-vegdist(plotspecdryg, method="bray")
specdistdryu<-vegdist(plotspecdryu, method="bray")


# tests
dispersion<-betadisper(specdist, group=plotkey$grazetrt) #sig
dispersion<-betadisper(specdist, group=plotkey$precinct)#ns
dispersion<-betadisper(specdistn, group=plotkeyn$grazetrt)#ns
dispersion<-betadisper(specdistp, group=plotkeyp$grazetrt)#ns
dispersion<-betadisper(specdistg, group=plotkeyg$precinct)#ns
dispersion<-betadisper(specdistu, group=plotkeyu$precinct)#ns
                       
dispersion<-betadisper(specdist2017, group=plotkey2017$grazetrt)#ns
dispersion<-betadisper(specdist2017, group=plotkey2017$precinct)#sig
dispersion<-betadisper(specdist2017n, group=plotkey2017n$grazetrt)#ns
dispersion<-betadisper(specdist2017p, group=plotkey2017p$grazetrt)#ns
dispersion<-betadisper(specdist2017g, group=plotkey2017g$precinct)#sig
dispersion<-betadisper(specdist2017u, group=plotkey2017u$precinct)#ns

dispersion<-betadisper(specdist2014a, group=plotkey2014$grazetrt)#ns
dispersion<-betadisper(specdist2014a, group=plotkey2014$precinct)#ns
dispersion<-betadisper(specdist2014n, group=plotkey2014n$grazetrt)#ns
dispersion<-betadisper(specdist2014p, group=plotkey2014p$grazetrt)#ns
dispersion<-betadisper(specdist2014g, group=plotkey2014g$precinct)#ns
dispersion<-betadisper(specdist2014u, group=plotkey2014u$precinct)#ns

dispersion<-betadisper(specdistweta, group=plotkeywet$grazetrt)#ns
dispersion<-betadisper(specdistweta, group=plotkeywet$precinct)#ns
dispersion<-betadisper(specdistwetn, group=plotkeywetn$grazetrt)#.1 sig
dispersion<-betadisper(specdistwetp, group=plotkeywetp$grazetrt)#ns
dispersion<-betadisper(specdistwetg, group=plotkeywetg$precinct)#ns
dispersion<-betadisper(specdistwetu, group=plotkeywetu$precinct)#ns

dispersion<-betadisper(specdistdrya, group=plotkeydry$grazetrt)#ns
dispersion<-betadisper(specdistdrya, group=plotkeydry$precinct)#ns
dispersion<-betadisper(specdistdryn, group=plotkeydryn$grazetrt)#ns
dispersion<-betadisper(specdistdryp, group=plotkeydryp$grazetrt)#ns
dispersion<-betadisper(specdistdryg, group=plotkeydryg$precinct)#ns
dispersion<-betadisper(specdistdryu, group=plotkeydryu$precinct)#ns

permutest(dispersion)
TukeyHSD(dispersion)



#ALL TIME DISPERSION / PRECIP visualization
plotkey.alltrt<-mutate(plotkey, alltrt=paste(grazetrt, precinct, sep="_"))%>%
  mutate(trtyr=paste(alltrt, year, sep="_"))
dispersion<-betadisper(specdist, group=plotkey.alltrt$trtyr)
dispersion0<-as.tibble(tapply(dispersion$distances, plotkey.alltrt$trtyr, mean))
dispersion0<-mutate(dispersion0, trtyr=rownames(dispersion0))%>%
  separate(trtyr, into=c("grazetrt", "precinct", "year"), sep="_", remove=TRUE)%>%
  mutate(year=as.numeric(year))
dispersion0<-left_join(dispersion0, climate, by = c("year" = "rainyear"))%>%
  ungroup()%>%
  dplyr::select(1:4, 7, 8, 11)%>%
  filter(!is.na(precip))

ggplot(dispersion0) + geom_bar(data=subset(dispersion0), aes(x=as.factor(year), y=value, fill=interaction(grazetrt, precinct)), stat="identity", position=
                                 "dodge") +
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))


ggplot(dispersion0, aes(x=precip, y=value, group=interaction(grazetrt, precinct), 
                        color=interaction(grazetrt, precinct))) +
  geom_smooth(, method=lm, se=FALSE)+ylab("dispersion")+
  geom_point()


##################################
### INDIVIDUAL SPECIES TRENDS
###################################
plantkey <- read_csv("plant list.csv")%>%      # species codes interpreted
  dplyr::select(4, 5, 13, 27)
names(plantkey)<-c("code", "sciname", "func", "native")
allspp<-vegtog_vegonly%>%
  group_by(year, grazetrt, precinct, year, code, precip)%>%
  summarize(meancount=mean(count), secount=calcSE(count))%>%
  filter(code!="bare")%>%
  filter(code!="litter")%>%
  filter(code!="bare ")%>%
  filter(code!="litter ")
allspp<-left_join(allspp, plantkey)%>%
  dplyr::select(-func, -native)

func<-vegtog_vegonly%>%
  group_by(code, native, growthhabit)%>%
  summarize() %>%
  filter(!is.na(native))%>%
  mutate(func=paste(growthhabit, native, sep="_"))

allspp<-left_join(allspp, func)
unique(allspp$sciname)

#### IG: bromus madritensis, schismus arabicus, hordeum murinum, vulpia myuros
###  IF: capsella bursa-pastoris, descuraninia sophia, erodium cicutarium, herniaria hirsuta, salsola tragus, sisymbrium irio
###  NG: poa secunda, vulpia microstachys
###  NF: all others (25 spp)

keyspp<- allspp %>%
  ungroup()%>%
 mutate(code=ifelse(code=="guilas"|code=="lasmin"|code=="lepnit"|code=="trigra", code, ifelse(func=="forb_n", "wildflower", code)))%>%
  mutate(code=ifelse(code=="erocic", code, ifelse(func=="forb_i", "iforb", code)))%>%
  group_by(year, grazetrt, precinct, code, precip, growthhabit, native, func)#%>%
    summarize(meancount=sum(meancount))



  vegtog.agg<-vegtog_vegonly%>%
  group_by(year, grazetrt, precinct, year, code, precip)%>%
  summarize(meancount=mean(count), secount=calcSE(count))#%>%
  filter(code%in%topspp2)

  
#allspp
ggplot(keyspp, aes(x=as.factor(year), y=meancount))+
  geom_bar(aes(x=as.factor(year), y=precip/180), stat="identity", fill='lightgrey')+
  geom_line(aes(group=code, color=code))+
  #geom_errorbar(aes(x=year, ymin=meancount-secount, ymax=meancount+secount, color=code), width=.1)+
  facet_grid(grazetrt~precinct)+
  scale_y_continuous(sec.axis = sec_axis(~.*180, name = "Annual Precipitation in mm"))+
  ylab("Mean % Cover/Plot")+
  theme_classic()

#grasses
ggplot(subset(keyspp, growthhabit=="grass"), aes(x=year, y=meancount))+
  geom_bar(aes(x=as.integer(year), y=precip/180), stat="identity", fill='lightgrey')+  # weird in precip, adding them up?
  geom_line(aes(group=code, color=code))+
  #geom_errorbar(aes(x=year, ymin=meancount-secount, ymax=meancount+secount, color=code), width=.1)+
  facet_grid(grazetrt~precinct)+
  scale_y_continuous(sec.axis = sec_axis(~.*180, name = "Annual Precipitation in mm"))+
  ylab("Mean % Cover/Plot")+
  theme_classic()

#native forbs
ggplot(subset(keyspp, func=="forb_n"), aes(x=year, y=meancount))+
  geom_bar(aes(x=as.integer(year), y=precip/50), stat="identity", fill='lightgrey')+
  geom_line(aes(group=code, color=code))+
 # geom_errorbar(aes(x=year, ymin=meancount-secount, ymax=meancount+secount, color=code), width=.1)+
  facet_grid(grazetrt~precinct)+
  scale_y_continuous(sec.axis = sec_axis(~.*50, name = "Annual Precipitation in mm"))+
  ylab("Mean % Cover/Plot")+
  theme_classic()

#inv forbs
ggplot(subset(keyspp, func=="forb_i"), aes(x=year, y=meancount))+
  geom_bar(aes(x=as.integer(year), y=precip/20), stat="identity", fill='lightgrey')+
  geom_line(aes(group=code, color=code))+
  # geom_errorbar(aes(x=year, ymin=meancount-secount, ymax=meancount+secount, color=code), width=.1)+
  facet_grid(grazetrt~precinct)+
  scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Annual Precipitation in mm"))+
  ylab("Mean % Cover/Plot")+
  theme_classic()



### 5. Specific Native and Invasive species % cover

# a. Percent cover of key weeds. Log-transformed.
ggplot(subset(vegtog, native=="n"&growthhabit=="forb"&(code!="guilas"&code!="lepnit"&code!="lasmin"&code!="trilan")), 
       aes(x=interaction(grazetrt, precinct), 
           y=(count), color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_grid(~wetordry, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") + ylab ("log(Count)") + 
  ggtitle("Invasive grass cover")

## spec STATS ##

alltrt.anova <- aov(count~factor(grazetrt), subset(vegtog, wetordry=='dry'&precinct=="N"&native=="n"&growthhabit=="forb"&(code!="guilas"&code!="lepnit"&code!="lasmin"&code!="trilan")))
summary(alltrt.anova)
TukeyHSD(alltrt.anova) 

precinct.anova <- aov(count~factor(precinct), subset(vegtog, native=="n"&growthhabit=="forb"&(code!="guilas"&code!="lepnit"&code!="lasmin"&code!="trilan")))
summary(precinct.anova)
TukeyHSD(precinct.anova) #lower precinct -1.224386, factor(precinct)   1    349   348.8   7.176 0.00752 **

grazetrt.anova <- aov(count~factor(grazetrt), subset(vegtog, wetordry=="dry"&native=="n"&growthhabit=="forb"&(code!="guilas"&code!="lepnit"&code!="lasmin"&code!="trilan")))
summary(grazetrt.anova)
TukeyHSD(grazetrt.anova) 


l<-lme(count~precip, random=~1|factor(block), subset(vegtog_vegonly, native=="n"&growthhabit=="grass"), na.action=na.omit)
anova(l)
summary(glht(l, linfct=mcp(wet.trt="Tukey")))

vegtog_nomodal<-vegtog_vegonly%>%
  group_by(native, growthhabit, year, quadrat, site,  alltrt, block, precip)%>%
  summarize(count=sum(count))
vegtog_unimodal<-vegtog_vegonly%>%
  mutate(count=count*count)%>%
  group_by(native, growthhabit, year, quadrat, site,  alltrt, block, precip)%>%
  summarize(count=sum(count))
vegtog_unimodal2<-vegtog_vegonly%>%
  mutate(precip=precip*precip)%>%
  group_by(native, growthhabit, year, quadrat, site, alltrt, block, precip)%>%
  summarize(count=sum(count))

l<-lme(count~precip, random=~1|factor(block), subset(vegtog_nomodal, native=="n"&growthhabit=="forb"), na.action=na.omit)
anova(l)
summary(glht(l, linfct=mcp(wet.trt="Tukey")))

summary(lm(count~precip, subset(vegtog_unimodal, native=="n"&growthhabit=="forb")))

ggplot(vegtog_nomodal, )
####################
### TURNOVER ETC
###################

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


rankshift <- rank_shift(vegsum0,
                        time.var = "year",
                        species.var = "code",
                        abundance.var = "count", 
                        replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  group_by(alltrt, year_pair)%>%
  summarize(mean.MRS=mean(MRS), se.MRS=calcSE(MRS))%>%
  mutate(year=substr(year_pair, 6,9))

Fig5a<-ggplot(rankshift)+
  geom_line(aes(as.factor(year), mean.MRS, group=alltrt, color=alltrt))+
  geom_errorbar(aes(color=alltrt, as.factor(year), ymin=mean.MRS-se.MRS, ymax=mean.MRS+se.MRS, ), width=.1)+
  labs(x="Year", y="Mean Rank Shift")+
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))+
  ggtitle("a")
Fig5b<-ggplot(turnover)+
  geom_line(aes(as.factor(year), mean.turnover, group=alltrt, color=alltrt))+
  labs(x="Year", y="Turnover")+
  geom_errorbar(aes(color=alltrt, as.factor(year), ymin=mean.turnover-se.turnover, ymax=mean.turnover+se.turnover, ), width=.1)+
  
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))+
  ggtitle("b")
ggarrange(Fig5a, Fig5b, ncol=1, nrow=2,  common.legend = TRUE, legend="right")

richsum<-vegsum0%>%
  group_by(year, site, code, alltrt, site.trt)%>%
  summarize(count=sum(count))
richsum <- left_join(richsum, community_diversity(richsum, time.var = "year", abundance.var="count", replicate.var="site.trt", metric = c("Shannon")))%>%
  group_by(year, site, alltrt, Shannon)%>%
  summarize(richness=length(unique(as.factor(code))))%>%
  group_by(year, alltrt)%>%
  summarize(meanrich=mean(richness), meanshan=mean(Shannon))

ggplot(richsum)+geom_line(aes(as.factor(year), meanshan, group=alltrt, color=alltrt))


##########
# Indicator Species
###########
library(indicspecies)
plotspecrel2017<-decostand(plotspec2017a, "total")
indicators<-multipatt(plotspecrel2017, interaction(plotkey2017$grazetrt, plotkey2017$precinct), func="r.g", control=how(nperm=999))
summary(indicators)
summary(indicators, alpha=1)
library(labdsv)
indicators2<-indval(plotspecrel2017, interaction(plotkey2017$grazetrt, plotkey2017$precinct))
summary(indicators2)

#r.g 2017
# grazed.n (lepnit)
# ungrazed.n(trilan, trigra, micele)
# ungrazed.p(guilas)
# ungraed both (lasmin)
# both p (hormur)

#indval 
# ungrazed N trilan mecele
# ungrazed both lasmin
# all but ungrazed P : lepnit, vulmic


#nothing turns up across all years, but after 10 years of grazing removal there are some indicators. 

#cluster (how to I test against treatment?)
library(cluster)
plotspecbray<-vegdist(plotspec, method="bray")
plotpseccluster<-agnes(plotspecbray, diss=TRUE, method="average", keep.diss=TRUE)
summary(plotpseccluster)
pltree(plotpseccluster)
cut_tree<-cutree(plotpseccluster, k=4)
ind_species<-multipatt(plotspecrel, cut_tree, duleg=TRUE)
summary(ind_species)

## directional change
vegtog_vegonly0<-mutate(vegtog_vegonly, quadyr=paste(quadrat, year, sep="_"))

rate_change_interval(df=subset(vegtog_vegonly0, alltrt="grazedN"), 
                     time.var="year", 
                     species.var="code", 
                     abundance.var="count", 
                     replicate.var = "quadyr")



ratechin_pre <- rate_change_interval(subset(vegsum0, year<=2014),
                        time.var = "year",
                        species.var = "code",
                        abundance.var = "count", 
                        replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  mutate(timing="pre_drought")

ratechin_post <- rate_change_interval(subset(vegsum0, year>=2014),
                                      time.var = "year",
                                      species.var = "code",
                                      abundance.var = "count", 
                                      replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  mutate(timing="post_drought")

ratechin<-rbind(ratechin_pre, ratechin_post)

ggplot(ratechin, aes(x=(interval), y=distance)) +geom_point(aes(color=alltrt)) +geom_smooth(method='lm', aes(color=alltrt) , se=F )+
  facet_wrap(~timing, scales="free")+
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))


ratech.alltime <- rate_change(subset(vegsum0),
                              time.var = "year",
                              species.var = "code",
                              abundance.var = "count", 
                              replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  mutate(timing="all_time (2007-2017")        

ratech.pre <- rate_change(subset(vegsum0, year<=2014),
                                 time.var = "year",
                                 species.var = "code",
                                 abundance.var = "count", 
                                 replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  mutate(timing="pre_drought (2007-2014)")


ratech.post <- rate_change(subset(vegsum0, year>=2014),
                          time.var = "year",
                          species.var = "code",
                          abundance.var = "count", 
                          replicate.var = "site.trt")%>%
  mutate(site=substr(site.trt, 1, 3), alltrt=substr(site.trt, 5, 7))%>%
  mutate(timing="post_drought (2014-2017)")

ratech<-rbind(ratech.pre, ratech.post, ratech.alltime)


ggplot(ratech, aes(x=alltrt, y=rate_change)) +geom_boxplot(aes(color=alltrt)) +facet_wrap(~timing, scales="free") +
  geom_hline(yintercept=0)+
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))+
  xlab("")

       