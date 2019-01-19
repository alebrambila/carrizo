##################################
# TREATMENT EFFECTS ON KEY WEEDS #
##################################

## Look at the effects of grazing and GKR mound treatment on key weedy species:
## Hordeum murinum,  Bromus hordaceous,  Schedonorus arundinacea. 

## Part 1: Calculate total hits/quadrat total and by species
## Part 2: Yearly visualization to see all points
## Part 3: Species Counts
## Part 4: Species Proportions
## Part 5: Species Presence-Absence


##############################################################
## Part 1: Calculate total hits/quadrat total and by species #
##############################################################

## Total hits/quadrat and plot - to calculate proportional cover below
totalHits<- vegtog %>%
  group_by(year, grazetrt, precinct, site, quadrat) %>% 
  summarize(qStems=sum(count)) %>% #count by quad
  group_by(year, grazetrt, precinct, site)%>%
  mutate(pStems=sum(qStems))
p1<-ggplot(totalHits, aes(x=interaction(grazetrt, precinct), y=qStems)) +
         geom_boxplot() +
          labs(x="Treatment", y="Total Stems/Quadrat")
p2<-ggplot(totalHits, aes(x=interaction(grazetrt, precinct), y=pStems)) +
  geom_boxplot() +
  labs(x="Treatment", y="Total Stems/Plot")
#Vizualize
ggarrange(p1, p2)


#weedQuad - quadrat level species count, proportion, p/a
horquad <- subset(vegtog, vegtog$code == "hormur")%>%
  select(year, quadrat, site, grazetrt, precinct, code, count)
horquad<-left_join(totalHits, horquad)%>%
  mutate(prop=count/qStems, code="hormur")
horquad[is.na(horquad)] <- 0 
horquad <- mutate(horquad, qpa=as.factor(ifelse(count==0,"absent","present")))
broquad<- subset(vegtog, vegtog$code == "bromad")%>%
  select(year, quadrat, site, grazetrt, precinct, code, count)
broquad<-left_join(totalHits, broquad)%>%
  mutate(prop=count/qStems, code="bromad")
broquad[is.na(broquad)] <- 0 
broquad <- mutate(broquad, qpa=as.factor(ifelse(count==0,"absent","present")))
schquad<- subset(vegtog, vegtog$code == "schara")%>%
  select(year, quadrat, site, grazetrt, precinct, code, count)
schquad<-left_join(totalHits, schquad)%>%
  mutate(prop=count/qStems, code="schara")
schquad[is.na(schquad)] <- 0 
schquad <- mutate(schquad, qpa=as.factor(ifelse(count==0,"absent","present")))

weedQuad <- rbind(horquad, broquad, schquad)

#weedPlot: plot level count, proportion, p/a (goal, less ones and zeroes in counts)
weedPlot <-weedQuad %>%
  group_by(year, grazetrt, precinct, site, code, pStems) %>%
  summarize(totalcount=sum(count))%>%
  mutate(prop=totalcount/pStems)%>%
  mutate(ppa=as.factor(ifelse(totalcount==0,"absent","present")))


###################################################
## Part 2: Yearly visualization to see all points #
###################################################

#"N" cluster very low, "grazed P" are most of the high ones!
#  Also.. looks like in grazed years, especially towards the end grazed "N" are getting some. 
p1<-ggplot(weedQuad, aes(x=year, y=count, color=interaction(grazetrt, precinct))) +
  geom_jitter() +
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue")) +
  labs(y="Count/Quadrat")+
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=80, alpha = .2)+
  facet_wrap(~code)
p2<-ggplot(weedPlot, aes(x=year, y=totalcount, color=interaction(grazetrt, precinct))) +
  geom_jitter() +
  scale_color_manual(values=c("pink", "brown", "lightblue", "darkblue")) +
  annotate("rect", xmin = 2011.5, xmax = 2015.5, ymin=0, ymax=240, alpha = .2)+
  labs(y="Count/Plot")+
  facet_wrap(~code)

#Visualize
ggarrange(p1, p2, ncol=1, nrow=2)



###########################
## Part 3: Species Counts #
###########################

p3<-ggplot(weedQuad, aes(x=interaction(grazetrt, precinct), y=count)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y="Count/Quadrat", x=" ")+
  ggtitle("All Quadrats")+
  facet_wrap(~code)+
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")
p4<-ggplot(subset(weedQuad, count>=2), aes(x=interaction(grazetrt, precinct), y=count)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y=" ", x=" ")+
  facet_wrap(~code)+
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")+
  ggtitle("Quadrats with >1 Individuals") #is this sketchy?
p5<-ggplot(weedPlot, aes(x=interaction(grazetrt, precinct), y=totalcount)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y="Count/Plot", x="Treatment")+
  ggtitle("All Plots")+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")+
  facet_wrap(~code)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6<-ggplot(subset(weedPlot, totalcount>=1), aes(x=interaction(grazetrt, precinct), y=totalcount)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")+
  labs(y="", x="Treatment")+
  ggtitle("Plots with >1 Individuals")+
  facet_wrap(~code)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Visualize
ggarrange(p3, p4, p5, p6, nrow=2, ncol=2) #lots of zeroes, lots of ones... 



################################
## Part 4: Species Proportions #
################################

# Boxplot, proportion
p7<-ggplot(weedQuad, aes(x=interaction(grazetrt, precinct), y=prop)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y="Proportion/Quadrat", x=" ")+
  ggtitle("All Quadrats")+
  facet_wrap(~code)+
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")
p8<-ggplot(subset(weedQuad, prop>=.05), aes(x=interaction(grazetrt, precinct), y=prop)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  facet_wrap(~code)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y=" ", x=" ") +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")+
  ggtitle("Only Quadrats with >5% Weed") #is this sketchy?
p9<-ggplot(weedPlot, aes(x=interaction(grazetrt, precinct), y=prop)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y="Proportion/Plot", x="Treatment")+
  ggtitle("All Plots")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~code)+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")
p10<-ggplot(subset(weedPlot, prop>=.05), aes(x=interaction(grazetrt, precinct), y=prop)) + 
  geom_boxplot(aes(fill=interaction(grazetrt, precinct))) +
  labs(y="", x="Treatment")+
  ggtitle("Plots with >5% Weed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~code)+
  scale_fill_manual(values=c("pink", "brown", "lightblue", "darkblue"))+
  theme(legend.position="none")
#Visualize
ggarrange(p7, p8, p9, p10, nrow=2, ncol=2)
#predictably still zeroes, but dominance should be a better representation than count?
# for hormur at least (and maybe schara) there seems to be more very high proportion 50%+ in grazed vs ungrazed



#####################################
## Part 5: Species Presence-Absence #
#####################################

p11<-ggplot(weedQuad, aes(x=interaction(grazetrt, precinct))) +
  geom_bar(aes(fill=qpa), position="dodge")+
  labs(y="# Quadrats with/without weed", x=" ")+
  facet_wrap(~code)+
  theme(axis.text.x = element_blank())
p12<-ggplot(weedPlot, aes(x=interaction(grazetrt, precinct))) +
  geom_bar(aes(fill=ppa), position="dodge")+
  labs(y="# Plots with/without weed", x="Treatment")+
  facet_wrap(~code)
ggarrange(p11,p12, nrow=2, ncol=1)




#read about error structure nlme - nonlinear mixed effect model and lme4 - linear mixed effect model
#book by doug bates on mixed effects and blocks
#block effects means you cant do ANOVA

# check out broom for pulling out model info
weedQuad<-weedQuad%>%
  mutate(trt=paste(grazetrt, precinct, sep="_"))

l2 <-aov(count~grazetrt*precinct, data=subset(weedQuad, code=="hormur"))
summary(l2)
HSD.test(l2, weedQuad$trt)
TukeyHSD(l2)

l3 <-aov(prop~grazetrt*precinct, data=subset(weedQuad, code=="hormur"))
summary(l3)
HSD.test(l3, weedQuad$trt)
TukeyHSD(l3)

