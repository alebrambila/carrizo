# load libraries to use
library(tidyverse)
library(vegan)

# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))


#calculate counts of native and invasive grasses and forbs by quadrat
native_coverGF <- vegtog %>%
  mutate(percover=count/81*100) %>%
  mutate(native=substr(form, 1, 1)) %>%
  mutate(native=as.factor(native)) %>%
  mutate(growthhabit=as.factor(growthhabit))%>%
  mutate(grazetrt=ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="grazed", "gnograze", ifelse(year%in% c(2007,2012,2013,2014,2015) & grazetrt=="ungrazed", "nograze", grazetrt)))%>%
  group_by(year, site, quadrat, plot, precinct, grazetrt, growthhabit, native) %>%
  summarize(nativecover=sum(percover)) %>%
  group_by(year, grazetrt, growthhabit, native) %>%
  mutate(meancover=mean(nativecover), secover=calcSE(nativecover)) %>%
  mutate(anlgrp=year) %>%
  mutate(anlgrp=ifelse(native=="i", anlgrp+.1, anlgrp)) %>%
  mutate(anlgrp=ifelse(growthhabit=="grass", anlgrp+.01, anlgrp)) %>%
  mutate(anlgrp=ifelse(grazetrt=="grazed", anlgrp+.001, anlgrp)) %>%
  mutate(anlgrp=anlgrp*1000)

native_coverGF <- native_coverGF %>%
  mutate(anlgrp=as.numeric(anlgrp)) %>%
  mutate(anlgrp=as.character(anlgrp)) %>%
mutate(anlgrp=as.factor(anlgrp))

levels(native_coverGF$native) <- c("Invasive", "Native")
levels(native_coverGF$growthhabit) <- c("Forb", "Grass")

#Tukey test, tells us which groups are significantly different
library(agricolae)
percentcover.lm <- lm(meancover ~ anlgrp, data = native_coverGF)
percentcover.av <- aov(percentcover.lm)
summary(percentcover.av)
tukey.test <- TukeyHSD(percentcover.av)
tukey.test
plot(tukey.test)
tukey.test2 <- HSD.test(percentcover.av, trt = 'anlgrp')
tukey.test2
tukeyresults <- tukey.test2$groups
tukeyresults <- rownames_to_column(tukeyresults, var = "anlgrp") %>%
  select(-meancover)
native_coverGF <- left_join(native_coverGF, tukeyresults)
#covergroups gives each bar in the visualization a letter that puts it in a significance group
covergroups <- native_coverGF %>%
  group_by(year, grazetrt, growthhabit, native, meancover, secover, groups) %>%
  summarize()

### ALL GRAZED YEARS HAVE SIGNIFICANT DIFFERENCES EXCEPT NF2008 AND IF2009.
#make a marker for NO significance to add to the plot
sig_marker <- data.frame(
  growthhabit= c("Forb", "Forb"),
  native= c("Native", "Invasive"),
  x = c(2008, 2009), y = c(70, 70))

#Native and invasive Grasses and forbs %cover effect from grazing
ggplot(subset(native_coverGF), aes(year, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, group=grazetrt), position="dodge", color = "black", lwd = .1) +
  facet_grid(growthhabit~native)+
  scale_fill_manual("Result", values = c("grey","brown", "grey", "darkblue"), 
                    labels = c("No Grazing", " Grazed", "No Grazing", "Ungrazed")) +
  labs(x="Year",y="Percent Cover") +
  annotate("segment", x = 2012, xend = 2012, y = 65, yend = 46, colour = "black", size=.5, alpha=1, arrow=arrow(type="closed")) + 
  theme(legend.position = "none")+ geom_text(
    data    = sig_marker,
    mapping = aes(x = x, y = y, label = "*"), size=15)



#counts of native and invasive grasses with and without grazing,
#grazed years are 2008-2011, 2016-2017
ggplot(subset(native_coverGF, growthhabit=="grass"), aes(year, meancover)) + 
  geom_bar(aes(fill=grazetrt), stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=(meancover-secover), ymax=meancover+secover, group=grazetrt), position="dodge", color = "black", lwd = .1) +
   scale_color_brewer() +facet_wrap(~native)+
  scale_fill_manual("Result", values = c("grey","brown", "grey", "darkblue"), 
                    labels = c("No Grazing", " Grazed", "No Grazing", "Ungrazed")) +
  labs(x="Year",y="Percent Cover") +
  annotate("segment", x = 2012, xend = 2012, y = 55, yend = 46, colour = "black", size=.5, alpha=1, arrow=arrow(type="closed")) + theme(legend.position = "none")




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

