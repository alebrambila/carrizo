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
  mutate(wtmonth=tolower(wtmonth), year=as.factor(year), precinct=as.factor(precinct)) %>%
  filter(!is.na(wtmonth)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, netwt, wtmonth) %>%
  summarize() %>%
  filter(year==2008|year==2009|year==2010|year==2011|year==2016|year==2017) %>%
  group_by(year, precinct, grazetrt, wtmonth) %>%
  summarize(avgwt=mean(netwt), sewt=calcSE(netwt))

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")

ggplot(grazing_biomass)  +
  geom_bar(aes(x=year, y=avgwt, fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(wtmonth~precinct) + theme_bw() +
  scale_fill_manual("Treatment", values = c("brown","darkblue"), 
  labels = c("Grazed","Ungrazed")) +
  labs(x="Year",y="Biomass") + geom_errorbar(aes(ymin=avgwt-sewt, ymax=avgwt+sewt))


#group not color to get all the error bars the same
#add error bars to 

#rarifications - diversity (stem densities) - species # curve, m SAR, IAR - Josh Grenoff, Carrizo paper
#create SAR and situate on it based on richness/number
#precinct for NNGRASS