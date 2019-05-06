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

ggplot(funcTrendsum, aes(x=interaction(grazetrt, precinct), 
                      y=mean, fill=func))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(x=year, ymin=mean-se, ymax=mean+se)) +
  facet_wrap(~year)#+
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue"))


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

# now with proportion of the community that they are:
ggplot(subset(vegtog, (year==2014|year==2017)&(code=="hormur"|code=="schara"|code=="bromad")), 
       aes(x=interaction(grazetrt, precinct), 
           y=prop, color=interaction(grazetrt, precinct))) +
  geom_boxplot() +
  theme(axis.text.x=element_blank()) +
  geom_jitter(width=.1, color="black") +
  facet_grid(code~year, scales="free") + 
  scale_color_manual(labels=c("Grazed, Off-Precinct", "Ungrazed, Off-Precinct", "Grazed, On-Precinct", "Ungrazed, On-Precinct"), 
                     values=c("pink", "brown", "lightblue", "darkblue")) +
  xlab("Treatment") + ylab ("log(Count)") + 
  ggtitle("Invasive grass cover")


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

# draw dispersion ellipses around data points
ordiellipse(plotspecNMDS, data.scores$trt, kind = "sd", label = T)

data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(data.scores)
data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
  select(-quadrat2)

plotkey <- vegtog %>%
  select(quadrat, precinct, grazetrt)%>%
  group_by(quadrat, precinct, grazetrt)%>%
  summarize()
data.scores <- left_join(data.scores, plotkey)%>%
  mutate(trt=paste(grazetrt, precinct, sep="_"))

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)

ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.1) +
  geom_jitter(data=subset(data.scores, year>2013),aes(x=NMDS1,y=NMDS2, color=interaction(precinct, grazetrt)),size=2) + # add the point markers
   # add the species labels
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  scale_fill_manual(labels=c("Grazed, Off-Precinct", "Grazed, On-Precinct","Ungrazed, Off-Precinct",  "Ungrazed, On-Precinct"), 
                    values=c("pink", "lightblue", "brown",  "darkblue"))+
  theme_bw() + facet_wrap(~year, scales="free")



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

