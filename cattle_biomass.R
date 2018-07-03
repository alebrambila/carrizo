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
  select(-flowermonth, -form, -fullform, -lifecycle, -growthhabit, -count, -preciptrt) %>%
  mutate(precinct=as.factor(precinct), year=as.factor(year)) %>%
  filter(!is.na(April), !is.na(October)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, April, October) %>%
  summarize() %>%
  filter(year==2008|year==2009|year==2010|year==2011|year==2016|year==2017) %>%
  group_by(year, precinct, grazetrt) %>%
  mutate(massred=(April-October), meanred=mean(massred), sered=calcSE(massred))

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")

#4a. Biomass Consumed
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

#4b. - LRR average native and invasive total cover across all quadrats within a year
biomass.spread <- grazing_biomass %>%
  select(-April, -October, -massred, -sered) %>%
  filter(grazetrt=="grazed"|grazetrt=="ungrazed") %>%
  select(-quadrat) %>%
  group_by(year, precinct, grazetrt, meanred) %>%
  summarize()%>%
  spread(grazetrt, meanred) %>%
  mutate(logresp=(log(grazed/ungrazed)))

#visualize yearly log-response ratio
ggplot(biomass.spread, aes(x=year, y=logresp)) +
  geom_bar(stat="identity", position="dodge", aes(fill=precinct)) +
  labs(x="Year",y="log(Grazed/Ungrazed)")




##MIXED EFFECTS MODEL
library(lme4)
biomass.model <- lmer(biomass~grazetrt+wtmonth + (1|year) + (1|plot), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
summary(grazing.model)

#Look for interaction effect
grazing.null <- lmer(meancover~grazetrt+native + (1|year) + (1|plot), data=subset(natcover, grazetrt=="grazed"|grazetrt=="ungrazed"), REML=FALSE)
anova(grazing.null,grazing.model)

biomass.agg <-grazing_biomass %>%
  group_by(grazetrt, precinct, wtmonth) %>%
  summarize(meanwt=mean(netwt), sewt=calcSE(netwt))

biomas.agg <-grazing_biomass %>%
  group_by(grazetrt, wtmonth)%>%
  summarize(meanwt=mean(netwt), sewt=calcSE(netwt))
  

ggplot(biomass.agg, aes(grazetrt, meanwt)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(wtmonth~precinct) +
  geom_errorbar(aes(ymin=(meanwt-sewt), ymax=meanwt+sewt, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  scale_fill_manual("Result", values = c("brown","darkblue"))+
  labs(x="",y="Biomass") + 
  theme(legend.position = "none")

#rarifications - diversity (stem densities) - species # curve, m SAR, IAR - Josh Grenoff, Carrizo paper
#create SAR and situate on it based on richness/number
#precinct for NNGRASS