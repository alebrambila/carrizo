## Look at some other categories to see if grazetrt affects it.

#Native/Invasive
ggplot((subset(vegtog, vegtog$native=="n"))) +
  geom_boxplot(aes(x=interaction(grazetrt, precinct), y=count))

#Annual/Perennial
ggplot((subset(vegtog, vegtog$lifecycle=="annual"))) +
  geom_boxplot(aes(x=interaction(grazetrt, precinct), y=count))


               