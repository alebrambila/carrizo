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

### 1. Species Richness

# Quadrat Level
alpha.rich <- vegtog %>%
  filter(code!="nohit") %>%
  group_by(year, grazetrt, quadrat, site, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(subset(alpha.rich, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=richness, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Species Richness")

# Plot Level -- not enough replicates
#gamma.rich <- vegtog %>%
#  filter(code!="nohit") %>%
#  group_by(year, grazetrt, site, precinct) %>%
#  summarize(richness=length(unique(code)))%>%
#  group_by(grazetrt, precinct)

#ggplot(subset(gamma.rich, year==2014|year==2017), 
#       aes(x=interaction(grazetrt, precinct), 
#           y=richness, color=interaction(grazetrt, precinct))) + 
#  geom_boxplot() +
#  theme(axis.text.x=element_blank()) +
#  geom_jitter(width=.1, color="black") +
#  facet_wrap(~year) + 
 # scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
 #                    values=c("pink", "brown", "lightblue", "darkblue")) +
#  xlab("Treatment") +
#  ggtitle("Plot-level Species Richness")


### 2. Shannon Diversity

#Quadrat level 
#overall quad level shannon diversity
alpha.shan <- vegtog %>%
  filter(year==2014|year==2017)
alpha.shan <- left_join(alpha.shan, community_diversity(alpha.shan, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shan <- alpha.shan %>%
  group_by(year, site, quadrat, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  group_by()

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

#native quad level shannon diversity
alpha.shan.n <- vegtog %>%
  filter(year==2014|year==2017)%>%
  filter(native=="n")
alpha.shan.n <- left_join(alpha.shan.n, community_diversity(alpha.shan.n, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shan.n <- alpha.shan.n %>%
  group_by(year, site, quadrat, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  group_by()

ggplot(subset(alpha.shan.n, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Quad-level Native Shannon Diversity")

## alpha.div STATS ##
shan.2017.anova <- aov(Shannon~factor(grazetrt)*factor(precinct)+factor(site), subset(alpha.shan, year==2017))
summary(shan.2017.anova)
TukeyHSD(shan.2017.anova) #ungrazed p-n is significantly different, but grazed p-n is not. 
plot(TukeyHSD(shan.2017.anova)) 

shan.2014.anova <- aov(Shannon~factor(grazetrt)*factor(precinct)+factor(site), subset(alpha.shan, year==2014))
summary(shan.2014.anova) #grazetrt is a very significant main effect
TukeyHSD(shan.2014.anova) #p and n no longer differentiat, everything else does
plot(TukeyHSD(shan.2014.anova))

### 3. Biomass
biomass<-vegtog%>%
  filter(!is.na(april), !is.na(october))%>%
  group_by(year, site, quadrat, site, precinct, grazetrt, april, october) %>%
  summarize()

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
func.agg<-left_join(func.agg, totalStems)
func.agg<-func.agg%>%
  mutate(prop=count/stems)
  
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

ggplot(subset(funcTrendsum, year>2013), aes(x=func, 
                      y=mean, fill=interaction(grazetrt, precinct)))+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position="dodge") +
  facet_wrap(~year)+
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))+
  labs(x="Functional Group", y="%cover")


# Vis option 2
ggplot(subset(func.agg, year==2014|year==2017),   
       aes(x=func,
           y=count, color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  geom_jitter(width=.1, color="black") +
  facet_grid(interaction(grazetrt, precinct)~year, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Functional Group Cover")


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


################################################### Nothin much, look at with key native too. NOT MUCH NATIVE GRASS AT ALL



ggplot(subset(vegtog, (year>2013)&(code=="hormur"|code=="schara"|code=="bromad")), 
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

# now with proportion of the community that they are:
#ggplot(subset(vegtog, (year==2014|year==2017)&(code=="hormur"|code=="schara"|code=="bromad")), 
#       aes(x=interaction(grazetrt, precinct), 
#           y=prop, color=interaction(grazetrt, precinct))) +
#  geom_boxplot() +
#  theme(axis.text.x=element_blank()) +
#  geom_jitter(width=.1, color="black") +
#  facet_grid(code~year, scales="free") + 
#  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
#                     values=c("pink", "brown", "lightblue", "darkblue")) +
#  xlab("Treatment") + ylab ("log(Count)") + 
#  ggtitle("Invasive grass cover")


# b. How many quadrats are key weeds present in?
#spec.quad <- vegtog%>%
#  group_by(year, grazetrt, precinct, code)%>%
#  mutate(qcount=length(unique(quadrat)))%>%
#  group_by(year, grazetrt, precinct, code, qcount)%>%
#  summarize()

##ggplot(subset(spec.quad, (year==2014|year==2017)&(code=="hormur"|code=="schara"|code=="bromad")), 
##       aes(x=interaction(grazetrt, precinct), 
#           y=qcount, fill=interaction(grazetrt, precinct))) +
#  geom_bar(stat="identity") +
#  theme(axis.text.x=element_blank()) +
#  facet_grid(code~year, scales="free") + 
#  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
#                     values=c("pink", "brown", "lightblue", "darkblue")) +
#  xlab("Treatment") + ylab ("Number of occupied quadrats") + 
#  ggtitle("Invasive grass distribution")
# grazing slows spread of invasives on-precinct, but acellerates it off-precinct! 

# TO DO: what is the native species of concern in this system?  Repeat:
#ggplot(subset(spec.quad, (year==2014|year==2017)&(code=="???????")), 
#       aes(x=interaction(grazetrt, precinct), 
#           y=qcount, fill=interaction(grazetrt, precinct))) +
#  geom_bar(stat="identity") +
#  theme(axis.text.x=element_blank()) +
#  facet_grid(code~year, scales="free") + 
#  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
#                    values=c("pink", "brown", "lightblue", "darkblue")) +
#  xlab("Treatment") + ylab ("Number of occupied quadrats") + 
#  ggtitle("Native bunchgrass distribution")

# TO DO: eventually work this in to a time block (before during after drought repeated measures ANOVA, but not yet)

##############################################
## II. Community NMDS Ordinations 2014-2017 ##
##############################################
# note: both NMDS had to be run with dimensions k=3 to find solutions.
# multivariate view of communities - not worried about who is who

### All Species
plotspec <- select(vegtog, year, quadrat, code, count)%>%
  mutate(quadyr=paste(quadrat, year, sep = "_"))%>%
  group_by(quadyr, code) %>%
  summarize(meancount=mean(count))
plotspec <-spread(plotspec, code, meancount, fill=0)%>%
  as.data.frame()
plotspec<- select(plotspec, -7, -29, -23, -30)
rownames(plotspec) <-plotspec$quadyr
plotspec <- select(plotspec, -quadyr)

t<-as.tibble(rowSums(plotspec))
t<-rownames_to_column(t)
t<-t%>%
  filter(value==0)%>%
  select(1)
t<-as.list(t[,1])
t<-t$rowname

plotspec<-subset(plotspec, !(rownames(plotspec)%in%t))

plotspecNMDS <- metaMDS(plotspec, scale = T, k=4)
plot(plotspecNMDS)

# draw dispersion ellipses around data points
ordiellipse(plotspecNMDS, data.scores$trt, kind = "sd", label = T)

data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(data.scores)
data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
  select(-quadrat2)%>%
  mutate(quadyr=paste(quadrat, year, sep="_"))

plotkey <- vegtog %>%
  select(quadrat, precinct, grazetrt, year)%>%
  mutate(quadyr=paste(quadrat, year, sep = "_"))%>%
  group_by(quadyr, precinct, grazetrt)%>%
  summarize()
plotkey<-subset(plotkey, (plotkey$quadyr%in%rownames(plotspec)))
#plotspec<-subset(plotspec, rownames(plotspec)%in%plotkey$quadyr)

data.scores <- left_join(data.scores, plotkey)%>%
  mutate(trt=paste(grazetrt, precinct, sep="_"))

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)


##### NMDS VISUALIZATOIN #######
ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores, year==2014|year==2017),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
   # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                    values=c("pink", "lightblue", "brown",  "darkblue"))+
  theme_bw() + facet_wrap(~year, scales="free")



### Functional Groups
#plotfunc <- select(vegtog, year, quadrat, code, lifecycle, growthhabit, native, count)%>% 
#  filter(code!="nohit"&code!="bare") %>%
#  mutate(func=paste(lifecycle, growthhabit, native, sep="_"))%>%
#  group_by(quadrat, func, year) %>%
#  summarize(meancount=mean(count))
#plotfunc <-spread(plotfunc, func, meancount, fill=0)%>%
#  as.data.frame()%>%
#  select(-NA_NA_NA, -perennial_grass_n) #only one perennial grass ever, run without column
##rownames(plotfunc) <-paste(plotfunc$quadrat, plotfunc$year, sep = "_")
#plotfunc <- select(plotfunc, -quadrat, -year)
#plotfuncNMDS <- metaMDS(plotfunc, scale = T, k=3)
#plot(plotfuncNMDS)

#data.scores <- as.data.frame(scores(plotfuncNMDS, display=c("sites")))
#data.scores$ID <- row.names(data.scores)
#data.scores <- as_tibble(data.scores) %>%
#  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
#  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
#  select(-quadrat2)

#plotkey <- vegtog %>%
#  select(quadrat, precinct, grazetrt)
#data.scores <- right_join(plotkey, data.scores)

# Extract and format species scores
#species.scores <- as.data.frame(scores(plotfuncNMDS, display=c("species")))
#species.scores$species <- row.names(species.scores)
#species.scores <- as_tibble(species.scores)

# NMDS PLOT by FN Group
#ggplot() +
#  geom_point(data=subset(data.scores, year>2013),aes(x=NMDS1,y=NMDS2,shape=interaction(precinct, grazetrt), color=interaction(precinct, grazetrt)),size=3) + # add the point markers
#  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
#  coord_equal() +
#  theme_bw() + facet_wrap(~year)


### ADD ELLIPSES TO ORDINATIONS
data(dune)
# calculate distance for NMDS
sol <- metaMDS(dune)
# Create meta data for grouping
MyMeta = data.frame(
  sites = c(2,13,4,16,6,1,8,5,17,15,10,11,9,18,3,20,14,19,12,7),
  amt = c("hi", "hi", "hi", "md", "lo", "hi", "hi", "lo", "md", "md", "lo", 
          "lo", "hi", "lo", "hi", "md", "md", "lo", "hi", "lo"),
  row.names = "sites")
# same in ggplot2
NMDS = data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2],group=MyMeta$amt)
ord<-ordiellipse(sol, MyMeta$amt, display = "sites", 
                 kind = "se", conf = 0.95, label = T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}


df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                   veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                ,group=g))
}

ggplot(data = NMDS, aes(MDS1, MDS2)) + geom_point(aes(color = group)) +
  geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2)



###  DISPERSION AND CENTROID STATS - multivairate view of communities
      # multivariate difference: centroids and dispersion between treatments (quad level composition - no blocks)
      # multivariate change: change in species comp from 2007/2014 to 2017
      # PERMANOVA - statistical testing of centroid difference
      # betadisper - dispersion using vegan/to verify my results


multivariate_difference(subset(vegtog, year==2017&precinct=="N"&code!="nohit"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="grazetrt")
multivariate_difference(subset(vegtog, year==2017&precinct=="P"&code!="nohit"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="grazetrt")
multivariate_difference(subset(vegtog, year==2017&grazetrt=="grazed"&code!="nohit"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="precinct")
multivariate_difference(subset(vegtog, year==2017&grazetrt=="ungrazed"&code!="nohit"), 
                        species.var="code", 
                        abundance.var="count",
                        replicate.var="quadrat",
                        treatment.var="precinct")


# PERMANOVA

plotkey2<-as.tibble(rownames(plotspec))%>%
  mutate(quadrat=substr(value, 1,9))%>%
  mutate(quadyr=value)%>%
  select(-value)
plotkey2<-left_join(plotkey2, plotkey)%>% # new plot key with shifting quadrats
  mutate(year=substr(quadyr, 11, 14))
plotkey2017<-plotkey2%>% #plotkey for only 2017
  filter(year==2017)
plotkey2017n<-plotkey2017%>%#plotkey for 2017 only off mound
  filter(precinct=="N")
plotkey2017p<-plotkey2017%>% #plot key for 2017 only on mound
  filter(precinct=="P")

plotspec2017<-plotspec%>%
  mutate(year=substr(rownames(plotspec), 11, 14))%>%
  filter(year==2017)%>%
  select(-year)

### Incorrect (no strata) - all years
adonis(plotspec ~ precinct*grazetrt, data=plotkey2, perm=1e3)

#Df SumsOfSqs MeanSqs F.Model      R2   Pr(>F)    
#precinct            1     1.810 1.80995  6.3466 0.00909 0.000999 ***
#  grazetrt            1     1.960 1.95993  6.8725 0.00984 0.000999 ***
#  precinct:grazetrt   1     0.339 0.33948  1.1904 0.00170 0.294705    
#Residuals         684   195.065 0.28518         0.97937             
#Total             687   199.175                 1.00000             
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# just 2017
adonis(plotspec2017 ~ precinct*grazetrt, data=plotkey2017, perm=1e3)

### Example of use with strata, for nested (e.g., block) designs.

dat <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1) )
dat
Agropyron <- with(dat, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2
Schizachyrium <- with(dat, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2
total <- Agropyron + Schizachyrium
library(lattice)
dotplot(total ~ NO3, dat, jitter.x=TRUE, groups=field,
        type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )

Y <- data.frame(Agropyron, Schizachyrium)
mod <- metaMDS(Y)
plot(mod)
### Hulls show treatment
ordihull(mod, group=dat$NO3, show="0")
ordihull(mod, group=dat$NO3, show="10", col=3)
### Spider shows fields
ordispider(mod, group=dat$field, lty=3, col="red")

### Correct hypothesis test (with strata)
adonis(Y ~ NO3, data=dat, strata=dat$field, perm=1e3)

### Incorrect (no strata)
adonis(Y ~ NO3, data=dat, perm=1e3)

########################
# RACs/abundance clocks- actually communities are changing, who is leading the cause
