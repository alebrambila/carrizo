## Look at other types of diversity besides alpha species diversity
## alpha diveristy is diversity across quadrats that are on and off precincts

## gamma diversity is total diversity on a PLOT including on and off mound (ignore precinct)
## how does grazing affect gamma diversity across years and plots
gamma <- vegtog %>%
  group_by(plot, year, grazetrt) %>%
  summarize(richness = length(unique(plantID)))
ggplot(gamma, aes(x=grazetrt, y=richness)) +geom_boxplot() +facet_wrap(~year)

##beta diversity is the difference in species composition between mound and off mound


##some basic visualizations
ggplot(vegtog) +geom_bar(aes(x=reorder(genus, count), fill=grazetrt), position = "fill") +coord_flip()                        
ggplot(vegtog) +geom_bar(aes(x=reorder(genus, count), fill=precinct), position = "fill") +coord_flip() + facet_wrap(~grazetrt)                       
ggplot(vegtog) +geom_bar(aes(x=reorder(genus, count), fill=precinct)) +coord_flip() + facet_wrap(~grazetrt)                       

#rearrange most to least

#vegan package - function RDA: PCA or RDA depending on how much information - do colors for different treatments 
#legendre on ordinations


#NMDS

#spread plotID and species into a new data frame for NMDS
plotspec <- select(vegtog, year, quadrat, code, count)%>%
  group_by(quadrat, code) %>%
  summarize(meancount=mean(count))
plotspec <-spread(plotspec, quadrat, meancount, fill=0)%>%
  as.data.frame()
rownames(plotspec) <-plotspec[,1]
plotspec <- select(plotspec, -code)
plotspecNMDS<-metaMDS(plotspec)
#base r plot
plot(plotspecNMDS)

ggplot()+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()