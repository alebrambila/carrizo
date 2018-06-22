### Biomass Analysis.  What is the effect of cattle grazing on biomass (quad and plot level)?
### To be run AFTER running all of data_cleaning.R

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
  summarize(avgwt=mean(netwt))

levels(grazing_biomass$precinct) <- c("", "Off-Precinct", "Precinct", "Precinct")

ggplot(grazing_biomass)  +
  geom_bar(aes(x=year, y=avgwt, fill=grazetrt), stat="identity", position="dodge") +
  facet_grid(wtmonth~precinct) + theme_bw() +
  scale_fill_manual("Treatment", values = c("brown","darkblue"), 
  labels = c("Grazed","Ungrazed")) +
  labs(x="Year",y="Biomass")
