### Question: How does cattle grazing affect % of native vs. exotic cover?
### Plan:Only looking at Center Well plots (10 grazed/ungrazed, each with multiple quadrats)
### 1. Each observation is a unique pin drop result with associated count
### 2. Convert pin drops to % covers (81 total, #/81)
### 3. Aggregate all native and exotic species by summing % covers within a quadrat/year (WILL NOT ADD TO 100%)
### 3b. Do we want to know how proportion of bare ground changes? Seems like it might be relevant. 
### 4a. Visualize annual effect (V1) by taking mean and SE of quadrats each year.
### 4b. Visualize annual effect (V2) log response ratio of means (what about SE's?)
### 5a. Aggregate across years (V1) (mean across years)
### 5b. Aggregate across years (V2) 
###     (mean of the log response ratio for each year: log(grazed/ungrazed))

# load libraries to use
library(tidyverse)
library(vegan)

# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))

#calculate percent cover of native and invasive grasses and forbs by quadrat
natcover <- vegtog %>%
  mutate(percover=count/81*100) %>%
  mutate(native=substr(form, 1, 1)) %>%
  mutate(native=as.factor(native)) %>%
  mutate(growthhabit=as.factor(growthhabit))%>%
  #for years that were grazed (grazed/ungrazed), for years that were not grazed (gnograze/nograze), respectively.
  mutate(grazetrt=ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="grazed", "gnograze", ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="ungrazed", "nograze", grazetrt)))%>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(percover=sum(percover)) %>%
  filter(!is.na(native), !is.na(growthhabit))
levels(natcover$native) <- c("Invasive", "Native")
levels(natcover$growthhabit) <- c("Forb", "Grass")


#average native and invasive total cover across all quadrats within a year
natcover.annual<-natcover %>%
  group_by(year, grazetrt, native, growthhabit) %>%
  summarize(meancover=mean(percover), secover=calcSE(percover))

#calculate yearly log-response ratio
natcova.spread <- natcover.annual %>%
  select(-secover) %>%
  filter(grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  spread(grazetrt, meancover) %>%
  mutate(logresp=(log(grazed/ungrazed)))

#visualize yearly log-response ratio
ggplot(natcova.spread, aes(x=year, y=logresp)) +
  geom_bar(stat="identity", position="dodge", aes(fill=native)) +
  facet_wrap(~growthhabit) +
  labs(x="Year",y="log(Grazed/Ungrazed)")


#Yearly comparison of mean % cover by group (quadrat averages)
ggplot(subset(natcover.annual), aes(year, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  facet_grid(growthhabit~native)+
  scale_fill_manual("Result", values = c("grey","brown", "grey", "darkblue"), 
                    labels = c("No Grazing", " Grazed", "No Grazing", "Ungrazed")) +
  labs(x="Year",y="Percent Cover") +
  annotate("segment", x = 2012, xend = 2012, y = 65, yend = 46, colour = "black", size=.5, alpha=1, arrow=arrow(type="closed"))+ 
  theme(legend.position = "none")

#Same as above, but sum up forbs and grasses (JUST OVERLAYING NOT SUMMING!)
ggplot(natcover.annual, aes(year, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  facet_grid(~native)+
  scale_fill_manual("Result", values = c("grey","brown", "grey", "darkblue"), 
                    labels = c("No Grazing", " Grazed", "No Grazing", "Ungrazed")) +
  labs(x="Year",y="Percent Cover") +
  annotate("segment", x = 2012, xend = 2012, y = 65, yend = 46, colour = "black", size=.5, alpha=1, arrow=arrow(type="closed"))+ 
  theme(legend.position = "none")

#aggregated means (mean of ALL QUADRATS) across grazed years grouped by grazing, native, growthhabit
natcover.agg <- natcover %>%
  filter(grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  group_by(grazetrt, growthhabit, native, precinct) %>%
    summarize(meancover=mean(percover), secover=calcSE(percover))

#plot mean and SE of all quads
ggplot(natcover.agg, aes(native, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(precinct~growthhabit) +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  scale_fill_manual("Result", values = c("brown","darkblue"))+
  labs(x="",y="Percent Cover") + 
  theme(legend.position = "none")

#calculate/visualize mean and SE of log response ratio over years. 
LRR.agg <- natcova.spread %>%
  group_by(native, growthhabit) %>%
  summarize(meanLRR=mean(logresp), seLRR=calcSE(logresp))
ggplot(LRR.agg, aes(native, meanLRR)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=growthhabit)) +
  geom_errorbar(aes(ymin=(meanLRR-seLRR), ymax=meanLRR+seLRR, group=growthhabit), position="dodge", color = "black", lwd = .1)

##MIXED EFFECTS MODEL
# There is a bit of a debate in mixed-effect world about p values
# Technically better to use a t value, which Bates forces you to do with lme4
# But nlme gives pseudo p values that are useful here
#? is there a big difference in bare ground? precinct or graze, relativize? -sum up and compare

# The lme4 way. Note nesting year in plot for repeated meausres
library(lme4)
grazing.model <- lmer(percover~grazetrt*native + (1|plot/year), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
summary(grazing.model)

# The nlme way. Note nesting year in plot for repeated meausres
library(nlme)
l <- lme(percover ~ grazetrt*native,  random = ~1|plot/year,  data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"))
summary(l)

# To do your Tukey test use multcomp
library(multcomp)

# Need to remove the grazing factors that you don't include and don't want to test
# Otherwise multcomp thinks these are empty factors and gets hung up
natcover2 <- subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  tbl_df() %>%
  mutate(grazetrt = as.character(grazetrt))

# For interactions, need to make a single variable that is the interaction term
# And it must be a factor for multcomp
natcover2$grznat <- as.factor(paste(natcover2$grazetrt, natcover2$native, sep = "_"))

# Run the model with the interaction variable 
l <- lme(percover ~ grznat,  random = ~1|plot/year,  data=natcover2)
summary(l)

# Run the Tukey test
summary(glht(l, linfct=mcp(grznat="Tukey")))



#Look for interaction effect
grazing.null <- lmer(meancover~grazetrt+native + (1|year) + (1|plot), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
anova(grazing.null,grazing.model)


### PRECINCT INTERACTION FOR INVASIVE GRASSES
#redo including precincts
natcover.p <- vegtog %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(nativecount=sum(count)) %>%
  group_by(year, precinct, grazetrt, growthhabit, native) %>%
  mutate(meancount=mean(nativecount), secount=calcSE(nativecount)) %>%
  filter(growthhabit=="grass", native=="i")

#Count invasive grasses with and without grazing, on and off mound,
#grazed years are 2008-2011, 2016-2017
ggplot(data=natcover.p, aes(year, meancount)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancount-secount), ymax=meancount+secount, group=grazetrt),position="dodge", color = "black", lwd = .1) +
  facet_wrap(~precinct)

#AGGREGATED NATIVES AND INVASIVES
native_cover<- vegtog %>%
 # filter(!is.na(cover)) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, native) %>%
  summarize(nativecount=sum(count)) %>%
  group_by(year, grazetrt, native) %>%
  mutate(meancount=mean(nativecount), secount=calcSE(nativecount))

ggplot(subset(native_cover, native=="n"), aes(year, meancount)) + 
  geom_point(aes(color=grazetrt)) +
  geom_line(aes(color=grazetrt)) +
  geom_errorbar(aes(ymin=(meancount-secount), ymax=meancount+secount, color=grazetrt)) +
  theme_bw() + scale_color_brewer(palette="Dark2")

