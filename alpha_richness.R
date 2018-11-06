## ALPHA (QUADRAT LEVEL) RICHNESS AND SHANNON DIVERSITY (also did plot level for shannon)

# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))


# gamma richness
gamma_rich <- vegtog%>%
  filter(year!=2007, year!=2008, year!=2013, year!=2014, year!=2015, year!=2016) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, grazetrt, native, precinct, site) %>%
  mutate(richness.n=length(unique(code))) %>%
  group_by(year, grazetrt, native, precinct, site) %>%
  summarize(richness=length(unique(code)))
  group_by(grazetrt, precinct) %>%
  summarize(meanrich=mean(richness), SE=calcSE(richness)) %>%
  filter(!is.na(SE))

ggplot(gamma_rich, aes(x=precinct,y=meanrich))+ geom_bar(aes(x=precinct, y=meanrich, fill=grazetrt), stat="identity", position="dodge") +
  scale_fill_manual("", values = c("brown","darkblue"))+
  xlab("")+
  ylab("Species Richness")+
  geom_errorbar(aes(ymin=meanrich-SE, ymax=meanrich+SE, group=grazetrt), position="dodge", color = "black", lwd = .1)


#treatment aggregated across years
alpha <- vegtog %>%
  filter(year!=2007, year!=2008, year!=2013, year!=2014, year!=2015, year!=2016) %>%
  group_by(year, grazetrt, quadrat, plot, precinct) %>%
  summarize(richness=length(unique(code)))

ggplot(alpha, aes(interaction(grazetrt, precinct), richness)) + geom_boxplot()
ggplot(alpha, (aes(x=richness))) +geom_histogram() + facet_grid(grazetrt~precinct)

#Cowpie density by quadrat interaction with precinct
#cowpierich <- vegtog %>%
  #filter the years cowpies weren't counted
 # filter((!is.na(cowpietotal)))%>%
 # group_by(year, cowpietotal, quadrat, plot, precinct) %>%
  #summarize(richness=length(unique(code)))
#ggplot(cowpierich, aes(x=cowpietotal, y=richness)) +geom_point() +geom_smooth() +facet_wrap(~precinct)

#SHANNON DIVERSITY
library(codyn)

#quad level total and native diversity
alpha.shannon <- vegtog %>%
  group_by(year, grazetrt, quadrat, plot, precinct) %>%
  mutate(native=substr(form, 1, 1)) %>%
  mutate(grznat=as.factor(paste(grazetrt, native, sep = "_")))
alpha.shannon <- left_join(alpha.shannon, community_diversity(alpha.shannon, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shannon <- alpha.shannon %>%
  group_by(year, plot, site, quadrat, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  group_by()
alpha.shannon.n <- vegtog %>%
  group_by(year, grazetrt, quadrat, plot, precinct) %>%
  mutate(native=substr(form, 1, 1)) %>%
  filter(native=="n") %>%
  mutate(grznat=as.factor(paste(grazetrt, native, sep = "_")))
alpha.shannon.n <- left_join(alpha.shannon.n, community_diversity(alpha.shannon.n, time.var = "year", abundance.var="count", replicate.var="quadrat", metric = c("Shannon")))
alpha.shannon.n <- alpha.shannon.n %>%
  group_by(year, plot, site, quadrat, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  rename("shannon.n"=Shannon)

##plot level total and native diversity
gamma.shannon <- vegtog %>%
  filter(year!=2007, year!=2008, year!=2013, year!=2014, year!=2015, year!=2016) %>%
  mutate(native=substr(form, 1, 1)) %>%
  group_by(year, site, plot, precinct, grazetrt, native, code) %>%
  summarize(count=sum(count))%>%
  group_by(year, grazetrt, plot) %>%
  mutate(grznat=as.factor(paste(grazetrt, native, sep = "_"))) %>%
  mutate(siteprec=paste(site, precinct, sep = "_"))
gamma.shannon <- left_join(gamma.shannon, community_diversity(gamma.shannon, time.var = "year", abundance.var="count", replicate.var="siteprec", metric = c("Shannon")))
gamma.shannon <- gamma.shannon %>%
  group_by(year, plot, site, precinct, grazetrt, Shannon) %>%
  summarize()
gamma.shannon.n <- vegtog %>%  
  mutate(native=substr(form, 1, 1)) %>%
  mutate(grznat=as.factor(paste(grazetrt, native, sep = "_"))) %>%
  filter(native=="n") %>%
  group_by(year, grazetrt, plot) %>%
  group_by(year, site, plot, precinct, grazetrt, native, code) %>%
  summarize(count=sum(count))%>%
  group_by(year, grazetrt, plot) %>%
  mutate(grznat=as.factor(paste(grazetrt, native, sep = "_"))) %>%
  mutate(siteprec=paste(site, precinct, sep = "_"))
gamma.shannon.n <- left_join(gamma.shannon.n, community_diversity(gamma.shannon.n, time.var = "year", abundance.var="count", replicate.var="siteprec", metric = c("Shannon")))
gamma.shannon.n <- gamma.shannon.n %>%
  group_by(year, plot, site, precinct, grazetrt, Shannon) %>%
  summarize() %>%
  rename("shannon.n"=Shannon)
alpha.shannon <- left_join(alpha.shannon, alpha.shannon.n)
gamma.shannon <- left_join(gamma.shannon, gamma.shannon.n)
rm(alpha.shannon.n, gamma.shannon.n)

##Yearly aggregates
annual.shannon.a <-alpha.shannon %>%
  filter(!is.na(Shannon), !is.na(shannon.n))%>%
  group_by(year, precinct, grazetrt) %>%
  summarize(meanshan=mean(Shannon), seshan=calcSE(Shannon), 
            meanshan.n=mean(shannon.n), seshan.n=calcSE(shannon.n)) %>%
  filter(!is.na(seshan), !is.na(seshan.n))
  
annual.shannon.g <-gamma.shannon %>%
  filter(!is.na(Shannon), !is.na(shannon.n))%>%
  group_by(year, grazetrt, precinct) %>%
  summarize(meanshan=mean(Shannon), seshan=calcSE(Shannon), 
            meanshan.n=mean(shannon.n), seshan.n=calcSE(shannon.n)) %>%
  filter(!is.na(seshan), !is.na(seshan.n)) %>%
  filter(!is.na(seshan), !is.na(seshan.n)) %>%
 gather("native", "shannon", 4,6) %>%
  mutate(seshan=ifelse(native=="meanshan", seshan, seshan.n)) %>%
  mutate(native=ifelse(native=="meanshan", "all", "native")) %>%
  dplyr::select(-seshan.n)
  
ggplot(annual.shannon.g, aes(year, shannon)) +
  geom_bar(stat="identity", position="dodge", aes(fill=grazetrt)) +
 geom_errorbar(aes(ymin=(shannon-seshan), 
                    ymax=shannon+seshan, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1) +
  scale_fill_manual("", values = c("brown","darkblue"), 
                    labels = c("Grazed","Ungrazed")) +
  labs(x="",y="Shannon Diversity") +facet_wrap(native~precinct)


##Aggregated across years
agg.shannon.a <-alpha.shannon %>%
  filter(!is.na(Shannon), !is.na(shannon.n))%>%
  group_by(precinct, grazetrt) %>%
  summarize(meanshan=mean(Shannon), seshan=calcSE(Shannon), 
            meanshan.n=mean(shannon.n), seshan.n=calcSE(shannon.n)) %>%
  filter(!is.na(seshan), !is.na(seshan.n)) %>%
  gather("native", "shannon", 3,5) %>%
  mutate(seshan=ifelse(native=="meanshan", seshan, seshan.n)) %>%
  mutate(native=ifelse(native=="meanshan", "all", "native")) %>%
  dplyr::select(-seshan.n)

##Visualize alpha shannon diversity aggregated
ggplot(agg.shannon.a, aes(native, shannon)) +
  geom_bar(stat="identity", position="dodge", aes(fill=grazetrt)) +
  geom_errorbar(aes(ymin=(shannon-seshan), 
                    ymax=shannon+seshan, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1)+
  facet_wrap(~precinct)+
  scale_fill_manual("", values = c("brown","darkblue"), 
                    labels = c("Grazed","Ungrazed")) +
  labs(x="",y="Shannon Diversity")

agg.shannon.g <-gamma.shannon %>%
  filter(!is.na(Shannon), !is.na(shannon.n))%>%
  group_by(grazetrt, precinct) %>%
  summarize(meanshan=mean(Shannon), seshan=calcSE(Shannon), 
            meanshan.n=mean(shannon.n), seshan.n=calcSE(shannon.n))%>%
  filter(!is.na(seshan), !is.na(seshan.n))%>%
  gather("native", "shannon", 3,5)%>%
  mutate(seshan=ifelse(native=="meanshan", seshan, seshan.n)) %>%
  mutate(native=ifelse(native=="meanshan", "all", "native")) %>%
  dplyr::select(-seshan.n)

##Visualize gamma shannon diversity aggregated
ggplot(agg.shannon.g, aes(native, shannon)) +
  geom_bar(stat="identity", position="dodge", aes(fill=grazetrt)) +
  geom_errorbar(aes(ymin=(shannon-seshan), 
                    ymax=shannon+seshan, 
                    group=grazetrt), 
                position="dodge", color = "black", lwd = .1) +
  scale_fill_manual("", values = c("brown","darkblue"), 
                    labels = c("Grazed","Ungrazed")) +
  labs(x="",y="Shannon Diversity") +facet_wrap(~precinct)


### MIXED EFFECTS MODEL ON GAMMA.SHANNON ###
# To do your Tukey test use multcomp
library(multcomp)

# Need to remove the grazing factors that you don't include and don't want to test
# Otherwise multcomp thinks these are empty factors and gets hung up
mem.shannon <- gamma.shannon %>%
  filter(!is.na(shannon.n)) %>%
  tbl_df() %>%
  mutate(grazetrt = as.character(grazetrt))

# For interactions, need to make a single variable that is the interaction term
# And it must be a factor for multcomp
mem.shannon$grzpre <- as.factor(paste(mem.shannon$grazetrt, mem.shannon$precinct, sep = "_"))

# Run the model with the interaction variable 
l <- lme(shannon.n ~ grzpre,  random = ~1|plot/year,  data=mem.shannon)
summary(l)

# Run the Tukey test
summary(glht(l, linfct=mcp(grzpre="Tukey")))
