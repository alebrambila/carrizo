### Biomass Analysis.  What is the effect of cattle grazing on biomass (quad and plot level)?
### To be run AFTER running all of data_cleaning.R


# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))


### Biomass was measured in April, July(?) and September(?) according to the report. 
## ** data says April and October  

grazing_biomass<-vegtog %>%
  mutate(native=substr(form, 1, 1)) %>%
  select(-flowermonth, -form, -fullform, -lifecycle, -growthhabit, -count, -preciptrt) %>%
  mutate(wtmonth=tolower(wtmonth), 
         precinct=as.factor(precinct), wtmonth=as.factor(wtmonth),
         year=as.factor(year)) %>%
  filter(!is.na(wtmonth)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, netwt, wtmonth) %>%
  summarize() %>%
  filter(year==2008|year==2009|year==2010|year==2011|year==2016|year==2017) %>%
  group_by(year, precinct, grazetrt, wtmonth) %>%
  mutate(avgwt=mean(netwt), sewt=calcSE(netwt))

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")
levels(grazing_biomass$wtmonth) <-c("Pre-Grazing", "Post-Grazing")


ggplot(grazing_biomass)  +
  geom_bar(aes(x=year, y=avgwt, fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(wtmonth~precinct) +
  scale_fill_manual("Treatment", values = c("brown","darkblue"), 
  labels = c("Grazed","Ungrazed")) +
  labs(x="Year",y="Biomass") + 
  geom_errorbar(aes(x=year, ymin=(avgwt-sewt), 
                    ymax=avgwt+sewt, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1)+
  theme(legend.position = "none")

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