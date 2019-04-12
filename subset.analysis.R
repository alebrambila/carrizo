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

# Plot Level 
gamma.rich <- vegtog %>%
  filter(code!="nohit") %>%
  group_by(year, grazetrt, site, precinct) %>%
  summarize(richness=length(unique(code)))%>%
  group_by(grazetrt, precinct)

ggplot(subset(gamma.rich, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=richness, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Plot-level Species Richness")


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

#Plot Level
#overall plot level shannon diversity
gamma.shan <- vegtog %>%
  filter(year==2014|year==2017)%>%
  group_by(year, site, code, precinct, grazetrt)%>%
  summarize(count=sum(count))
gamma.shan <- left_join(gamma.shan, community_diversity(gamma.shan, time.var = "year", abundance.var="count", replicate.var="site", metric = c("Shannon")))
gamma.shan <- gamma.shan %>%
  group_by(year, site, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  group_by()

ggplot(subset(gamma.shan, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Plot-level Shannon Diversity")

# Native plot level shannon diversity
gamma.shan.n <- vegtog %>%
  filter(year==2014|year==2017)%>%
  filter(native=="n")%>%
  group_by(year, site, code, precinct, grazetrt)%>%
  summarize(count=sum(count))
gamma.shan.n <- left_join(gamma.shan.n, community_diversity(gamma.shan.n, time.var = "year", abundance.var="count", replicate.var="site", metric = c("Shannon")))
gamma.shan.n <- gamma.shan.n %>%
  group_by(year, site, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  group_by()

ggplot(subset(gamma.shan.n, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=Shannon, color=interaction(grazetrt, precinct))) + 
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_wrap(~year) + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Plot-level Native Shannon Diversity")

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

# Vis option 1
ggplot(subset(func.agg, year==2014|year==2017), 
       aes(x=interaction(grazetrt, precinct), 
           y=count, color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_grid(func~year, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") +
  ggtitle("Functional Group Cover")

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
# Nothin much, look at with key native too. 


# b. How many quadrats are key weeds present in?
spec.quad <- vegtog%>%
  group_by(year, grazetrt, precinct, code)%>%
  mutate(qcount=length(unique(quadrat)))%>%
  group_by(year, grazetrt, precinct, code, qcount)%>%
  summarize()

ggplot(subset(spec.quad, (year==2014|year==2017)&(code=="hormur"|code=="schara"|code=="bromad")), 
       aes(x=interaction(grazetrt, precinct), 
           y=qcount, fill=interaction(grazetrt, precinct))) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_blank()) +
  facet_grid(code~year, scales="free") + 
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") + ylab ("Number of occupied quadrats") + 
  ggtitle("Invasive grass distribution")
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

### All Species
plotspec <- select(vegtog, year, quadrat, code, count)%>%
  group_by(quadrat, code, year) %>%
  summarize(meancount=mean(count))
plotspec <-spread(plotspec, code, meancount, fill=0)%>%
  as.data.frame()
plotspec<- select(plotspec, -nohit, -moss, -24, -8)
rownames(plotspec) <-paste(plotspec$quadrat, plotspec$year, sep = "_")
plotspec <- select(plotspec, -quadrat, -year)

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
\

### Functional Groups
plotfunc <- select(vegtog, year, quadrat, code, lifecycle, growthhabit, native, count)%>% 
  filter(code!="nohit"&code!="bare") %>%
  mutate(func=paste(lifecycle, growthhabit, native, sep="_"))%>%
  group_by(quadrat, func, year) %>%
  summarize(meancount=mean(count))
plotfunc <-spread(plotfunc, func, meancount, fill=0)%>%
  as.data.frame()%>%
  select(-NA_NA_NA, -perennial_grass_n) #only one perennial grass ever, run without column
rownames(plotfunc) <-paste(plotfunc$quadrat, plotfunc$year, sep = "_")
plotfunc <- select(plotfunc, -quadrat, -year)
plotfuncNMDS <- metaMDS(plotfunc, scale = T, k=3)
plot(plotfuncNMDS)

data.scores <- as.data.frame(scores(plotfuncNMDS, display=c("sites")))
data.scores$ID <- row.names(data.scores)
data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
  select(-quadrat2)

plotkey <- vegtog %>%
  select(quadrat, precinct, grazetrt)
data.scores <- right_join(plotkey, data.scores)

# Extract and format species scores
species.scores <- as.data.frame(scores(plotfuncNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)

# NMDS PLOT by FN Group
ggplot() +
  geom_point(data=subset(data.scores, year>2013),aes(x=NMDS1,y=NMDS2,shape=interaction(precinct, grazetrt), color=interaction(precinct, grazetrt)),size=3) + # add the point markers
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() + facet_wrap(~year)







