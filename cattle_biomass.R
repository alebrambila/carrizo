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
  mutate(avgwt=mean(netwt), sewt=calcSE(netwt)) %>%
  mutate(anlgrp=year) %>%
  mutate(anlgrp=ifelse(precinct=="N", anlgrp+.1, anlgrp)) %>%
  mutate(anlgrp=ifelse(wtmonth=="april", anlgrp+.01, anlgrp)) %>%
  mutate(anlgrp=ifelse(grazetrt=="grazed", anlgrp+.001, anlgrp)) %>%
  mutate(anlgrp=anlgrp*1000)

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")
levels(grazing_biomass$wtmonth) <-c("Pre-Grazing", "Post-Grazing")

#SIG ANALYSIS (tukey)
#Tukey test, tells us which groups are significantly different
library(agricolae)
biomass.lm <- lm(netwt ~ anlgrp, data = grazing_biomass)
biomass.av <- aov(biomass.lm)
summary(biomass.av)
tukey.test2 <- HSD.test(biomass.av, trt = 'anlgrp')
tukey.test2
tukeyresults <- tukey.test2$groups
tukeyresults <- rownames_to_column(tukeyresults, var = "anlgrp") %>%
  select(-netwt) %>%
  mutate(anlgrp=as.numeric(anlgrp))
grazing_biomass <- left_join(grazing_biomass, tukeyresults)
#covergroups gives each bar in the visualization a letter that puts it in a significance group
biomassgroups <- grazing_biomass %>%
  group_by(year, grazetrt, precinct, wtmonth, avgwt, sewt, groups) %>%
  summarize()


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


#rarifications - diversity (stem densities) - species # curve, m SAR, IAR - Josh Grenoff, Carrizo paper
#create SAR and situate on it based on richness/number
#precinct for NNGRASS