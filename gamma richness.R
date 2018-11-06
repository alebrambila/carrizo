## Look at other types of diversity besides alpha species diversity
## alpha diveristy is diversity across quadrats that are on and off precincts


#calculate plot (gamma) trends
trends_plot <- vegtog%>%
  group_by(year, grazetrt, precinct, plot) %>%
  summarize(richness=length(unique(code))) %>%
  group_by(year, grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness))



#plot plot (gamma) trends
ggplot(trends_plot, aes(color=grazetrt))+ geom_line(aes(x=year, y=meanrich)) +
  facet_wrap(~precinct, labeller=) +geom_errorbar(aes(x=year, ymin=meanrich-SE, ymax=meanrich+SE))+
  theme_classic() + ggtitle("Plot-Level Richness (±SE)")+
  xlab("Year")+ylab("Species Richness") +
  scale_fill_discrete(name="Grazing Treatments", labels =(c("Grazed", "Ungrazed")))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14), title=element_text(size=18),
        axis.text.x = element_text(angle = 90, hjust = 1))





## gamma diversity is total diversity on a PLOT including on and off mound (ignore precinct)
## how does grazing affect gamma diversity across years and plots
gamma <- vegtog %>%
  group_by(plot, year, grazetrt) %>%
  summarize(richness = length(unique(plantID)))

ggplot(gamma, aes(x=grazetrt, y=richness)) +geom_boxplot() +facet_wrap(~year)


gamma <- vegtog %>%
  filter(year!=2007, year!=2008, year!=2013, year!=2014, year!=2015, year!=2016) %>%
  group_by(year, grazetrt, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(gamma, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()
ggplot(gamma, (aes(x=richness))) +geom_histogram() + facet_grid(grazetrt~precinct)


##beta diversity is the difference in species composition between mound and off mound


##Visualize uneven distribution of species across the treatments
ggplot(vegtog) +geom_bar(aes(x=reorder(binomial, count), fill=grazetrt), position = "fill") +
  coord_flip() + theme_classic() + ggtitle("Simplified Grazing Response")+
  xlab("Species")+ylab("Proportion") +
  scale_fill_discrete(name="", labels =(c("Grazed", "Ungrazed")))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=18,face="bold"), title=element_text(size=18),
        axis.text.x = element_text(size=0))



ggplot(vegtog) +geom_bar(aes(x=reorder(binomial, -table(count)[count]), fill=precinct), position = "fill") +coord_flip() + facet_wrap(~grazetrt)                       
ggplot(vegtog) +geom_bar(aes(x=reorder(binomial, count), fill=precinct)) +coord_flip() + facet_wrap(~grazetrt)                       

ggplot(theTable, aes(x=reorder(Position, -table(Position)[Position]))) + geom_bar()
#rearrange most to least

