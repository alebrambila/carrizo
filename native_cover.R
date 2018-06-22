#calculate counts of native and invasive grasses and forbs by quadrat
native_coverGF <- vegtog %>%
  mutate(percover=count/81) %>%
  mutate(native=substr(form, 1, 1)) %>%
  mutate(native=as.factor(native)) %>%
  mutate(grazetrt=ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="grazed", "gnograze", ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="ungrazed", "nograze", grazetrt)))%>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(nativecover=sum(percover)) %>%
  group_by(year, grazetrt, growthhabit, native) %>%
  mutate(meancover=mean(nativecover), secover=calcSE(nativecover))

levels(native_coverGF$native) <- c("Invasive", "Native")

#counts of native and invasive grasses with and without grazing,
#grazed years are 2008-2011, 2016-2017
ggplot(subset(native_coverGF, growthhabit=="grass"), aes(year, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, color=grazetrt), position="dodge") +
  theme_bw() + scale_color_brewer() +facet_wrap(~native)+
  scale_fill_manual("Result", values = c("grey","brown", "grey", "darkblue"), 
                    labels = c("No Grazing", " Grazed", "No Grazing", "Ungrazed")) +
  labs(x="Year",y="% Cover")+
  annotate("segment", x = 2012, xend = 2012, y = .5, yend = .4, colour = "black", size=.5, alpha=1, arrow=arrow())

#redo including precincts
native_coverGFfilter <- vegtog %>%
  # filter(!is.na(cover)) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(nativecount=sum(count)) %>%
  group_by(year, precinct, grazetrt, growthhabit, native) %>%
  mutate(meancount=mean(nativecount), secount=calcSE(nativecount)) %>%
  filter(growthhabit=="grass", native=="i") 

#Count invasive grasses with and without grazing, on and off mound,
#grazed years are 2008-2011, 2016-2017
ggplot(subset(native_coverGFfilter), aes(year, meancount)) + 
  geom_point(aes(color=grazetrt)) +
  geom_line(aes(color=grazetrt)) +
  geom_errorbar(aes(ymin=(meancount-secount), ymax=meancount+secount, color=grazetrt)) +
  theme_bw() + scale_color_brewer(palette="Dark2") +facet_wrap(~precinct)

#calculate counts of total native and invasive plants by quadrat
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



#calculate counts of total native and invasive plants by plot
native_coverPL<- vegtog %>%
 # filter(!is.na(cover)) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, plot, grazetrt, native) %>%
  summarize(nativecount=sum(count)) %>%
  group_by(year, grazetrt, native) %>%
  mutate(meancount=mean(nativecount), secount=calcSE(nativecount))

ggplot(subset(native_coverPL, native=="n"), aes(year, meancount)) + 
  geom_point(aes(color=grazetrt)) +
  geom_line(aes(color=grazetrt)) +
  geom_errorbar(aes(ymin=(meancount-secount), ymax=meancount+secount, color=grazetrt)) +
  theme_bw() + scale_color_brewer(palette="Dark2")
