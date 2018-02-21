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


#NMDS for each plot and year 
# Check to make sure this converges... might consider running a separate one for each year

#spread plotID and species into a new data frame for NMDS
plotspec <- select(vegtog, year, quadrat, code, count)%>%
  group_by(quadrat, code, year) %>%
  summarize(meancount=mean(count))
plotspec <-spread(plotspec, code, meancount, fill=0)%>%
  as.data.frame()

# add rownames
rownames(plotspec) <-paste(plotspec$quadrat, plotspec$year, sep = "_")
plotspec <- select(plotspec, -quadrat, -year)

# run the NMDS
plotspecNMDS <- metaMDS(plotspec, scale = T)
#base r plot
plot(plotspecNMDS)

# Make a key of quadrat info
plotkey <- vegtog %>%
  select(quadrat, precinct, grazetrt, pasturetrt, rodenttrt=)

# Extract and format axis scores
data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(data.scores)
data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("quadrat", "quadrat2", "year"), by="=") %>%
  mutate(quadrat = paste(quadrat, quadrat2, sep = "-")) %>%
  select(-quadrat2)

data.scores <- right_join(plotkey, data.scores)
  
# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)



ggplot() +
 # geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=interaction(precinct, grazetrt), color=interaction(precinct, grazetrt)),size=3) + # add the point markers
#  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
#  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() + facet_wrap(~year)



### NMDS when averaged values across years ##

#spread plotID and species into a new data frame for NMDS
plotspec <- select(vegtog, year, quadrat, precinct, code, count)%>%
  group_by(quadrat, precinct,  code, year) %>%
  summarize(meancount=mean(count))  %>%
  group_by(quadrat, precinct, code) %>%
  summarize(meancount = mean(meancount))
plotspec <-spread(plotspec, code, meancount, fill=0)%>%
  as.data.frame()
plotspec <- plotspec %>%
  mutate(quadratNEW = paste(quadrat,precinct))
rownames(plotspec) <-paste(plotspec$quadratNEW)
plotspec <- plotspec %>%
  select(-quadrat, -precinct, -quadratNEW)


# run NMDS
plotspecNMDS <- metaMDS(plotspec, scale = T, trymax=500, k=3)
#base r plot
plot(plotspecNMDS)

# create a key for joining
plotkey <- vegtog %>%
  select(quadrat, precinct, grazetrt) %>%
  unique() %>%
  mutate(quadratNEW = paste(quadrat,precinct)) %>%
  mutate(interaction = paste(precinct ,grazetrt))

# Extract and format site scores
data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$quadratNEW <- row.names(data.scores)
data.scores <- as_tibble(data.scores)

data.scores <- right_join(plotkey, data.scores, by="quadratNEW")

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)


#ELLIPSES#
data.scores <- data.scores %>%
  select(-quadrat, -precinct, -grazetrt, -quadratNEW, -NMDS3)

#plotspecNMDS <- metaMDS(plotspec, scale = T)
ellipses2 <- ordiellipse(plotspecNMDS, data.scores$interaction, display="sites")

# Plot it
ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=interaction(precinct, grazetrt), color=interaction(precinct, grazetrt)),size=3) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(data.scores$interaction)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$interaction==g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                ,group=g))
}
