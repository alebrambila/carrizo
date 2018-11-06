### Question: What is the effect of cattle grazing on biomass (quad level)?
### Plan: Only looking at Center Well plots (10 grazed/ungrazed, each with multiple quadrats)
### 1. Each observation is a harvest weight for a given quadrat (either in April or October), 
###  ...there are also June weights but dont match precinct numbers.  Treatments are precinct and grazing
### 2. Remove duplicate rows of vegtog (each pindrop obs has the same wt within treatments)
### 3. Calculate mass reduction within a quadrat from april to october, mean across all and SE across all(/yr)
### 4a. Visualize annual effect (V1) by taking mean and SE of quadrats each year.
### 4b. Visualize annual effect (V2) log response ratio of means (what about SE's?)
### 5a. Aggregate across years (V1) (mean across years)
### 5b. Aggregate across years (V2) 
###     (mean of the log response ratio for each year: log(grazed/ungrazed))

# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))

#2, 3
grazing_biomass<-vegtog %>%
  mutate(native=substr(form, 1, 1)) %>%
  dplyr::select(-flowermonth, -form, -fullform, -lifecycle, -growthhabit, -count, -preciptrt) %>%
  mutate(precinct=as.factor(precinct), year=as.factor(year)) %>%
  filter(!is.na(April), !is.na(October)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, April, October) %>%
  summarize() %>%
  filter(year==2008|year==2009|year==2010|year==2011|year==2016|year==2017) %>%
  group_by(year, precinct, grazetrt) %>%
  mutate(massred=(April-October), meanred=mean(massred), sered=calcSE(massred), meanoct=mean(October), seoct=calcSE(October))

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")

#4a. annual biomass Consumed
ggplot(grazing_biomass)  +
  geom_bar(aes(x=year, y=meanred, fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(~precinct) +
  scale_fill_manual("Treatment", values = c("brown","darkblue"), 
  labels = c("Grazed","Ungrazed")) +
  labs(x="Year",y="Biomass Consumed") + 
  geom_errorbar(aes(x=year, ymin=(meanred-sered), 
                    ymax=meanred+sered, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1)+
  theme(legend.position = "none")

#4ai.Annual residual biomass
ggplot(grazing_biomass)  +
  geom_bar(aes(x=year, y=meanoct, fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(~precinct) +
  scale_fill_manual("Treatment", values = c("brown","darkblue"), 
                    labels = c("Grazed","Ungrazed")) +
  labs(x="Year",y="Residual Biomass (lb/quadrat)") + 
  geom_errorbar(aes(x=year, ymin=(meanoct-seoct), 
                    ymax=meanoct+seoct, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1)+
  theme(legend.position = "none")

#aggregated residual biomass
agg.biomass <- vegtog %>%
  mutate(native=substr(form, 1, 1)) %>%
  dplyr::select(-flowermonth, -form, -fullform, -lifecycle, -growthhabit, -count, -preciptrt) %>%
  mutate(precinct=as.factor(precinct), year=as.factor(year), grazetrt=as.factor(grazetrt)) %>%
  filter(!is.na(April), !is.na(October)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, April, October) %>%
  summarize() %>%
  filter(year==2008|year==2009|year==2010|year==2011|year==2016|year==2017) %>%
  group_by(precinct, grazetrt) %>%
  mutate(meanoct=mean(October), seoct=calcSE(October))
levels(agg.biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")
levels(agg.biomass$grazetrt) <- c("Grazed", "Ungrazed")
  
ggplot(agg.biomass, aes(grazetrt, meanoct)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(~precinct) +
  geom_errorbar(aes(ymin=(meanoct-seoct), ymax=meanoct+seoct, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  scale_fill_manual("Result", values = c("brown","darkblue"), labels=c("Grazed", "Ungrazed"))+
  labs(x="",y="Residual Biomass (lb/quadrat)") + 
  theme(legend.position = "none")

#4b. - LRR residual biomass
biomass.spread <- grazing_biomass %>%
  dplyr::select(-April, -October, -massred, -sered) %>%
  filter(grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  dplyr::select(-quadrat) %>%
  group_by(year, precinct, grazetrt, meanoct) %>%
  summarize()%>%
  spread(grazetrt, meanoct) %>%
  mutate(logresp=(log(grazed/ungrazed))) %>%
  group_by(precinct) %>%
  summarize(meanLRR=mean(logresp), seLRR=calcSE(logresp))

#visualizelog-response ratio
ggplot(biomass.spread, aes(precinct, meanLRR)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=precinct)) +
  geom_errorbar(aes(ymin=(meanLRR-seLRR), ymax=meanLRR+seLRR), position="dodge", color = "black", lwd = .1)+
  scale_fill_manual("", values = c("brown","darkblue"))+
  labs(x="",y="Log Response Ratio")

### MIXED EFFECTS MODEL ###
# To do your Tukey test use multcomp
library(multcomp)

# Need to remove the grazing factors that you don't include and don't want to test
# Otherwise multcomp thinks these are empty factors and gets hung up
mem.biomass <- subset(grazing_biomass, grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  dplyr::select(-April, -massred, -meanred, -sered, -meanoct, -seoct) %>%
  tbl_df() %>%
  mutate(grazetrt = as.character(grazetrt))

# For interactions, need to make a single variable that is the interaction term
# And it must be a factor for multcomp
mem.biomass$grzpre <- as.factor(paste(mem.biomass$grazetrt, mem.biomass$precinct, sep = "_"))

# Run the model with the interaction variable 
l <- lme(October ~ grzpre,  random = ~1|plot/quadrat/year,  data=mem.biomass)
summary(l)

# Run the Tukey test
summary(glht(l, linfct=mcp(grzpre="Tukey")))


##MIXED EFFECTS MODEL #OLD
#library(lme4)
#biomass.model <- lmer(biomass~grazetrt+wtmonth + (1|year) + (1|plot), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
#summary(grazing.model)
#Look for interaction effect
#grazing.null <- lmer(meancover~grazetrt+native + (1|year) + (1|plot), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
#anova(grazing.null,grazing.model)

#rarifications - diversity (stem densities) - species # curve, m SAR, IAR - Josh Grenoff, Carrizo paper
#create SAR and situate on it based on richness/number
#precinct for NNGRASS