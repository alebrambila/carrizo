#calculate counts of native and invasive grasses and forbs by quadrat
native_coverGF <- vegtog %>%
 # filter(!is.na(cover)) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(nativecount=sum(count)) %>%
  group_by(year, grazetrt, growthhabit, native) %>%
  mutate(meancount=mean(nativecount), secount=calcSE(nativecount))

#counts of native and invasive grasses with and without grazing,
#grazed years are 2008-2011, 2016-2017
ggplot(subset(native_coverGF, growthhabit=="grass"), aes(year, meancount)) + 
  geom_point(aes(color=grazetrt)) +
  geom_line(aes(color=grazetrt)) +
  geom_errorbar(aes(ymin=(meancount-secount), ymax=meancount+secount, color=grazetrt)) +
  theme_bw() + scale_color_brewer(palette="Dark2") +facet_wrap(~native)

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
