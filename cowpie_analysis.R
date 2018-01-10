#create a dataframe, richness_cowpies, which summarizes vegtog by newplotID that will allow for 
#a direct comparison of grazing vs. non grazing as well as grazing pressure as measured
#by cowpie count in a plot
richness_cowpies <- vegtog %>%
  filter(count != 0, !is.na(count)) %>%
  filter(cover != 0, !is.na(cover)) %>%
  filter(exclosure != "CW cow excl") %>%
  group_by(newplotID, year, grazetrt) %>%
  summarize(richness = n())

