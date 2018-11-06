#Linear fixed effect model example (merced guide)
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex,pitch)
xmdl = lm(pitch ~ sex, my.df)
summary(xmdl)

#Linear mixed effects model example
library(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
which(is.na(politeness$frequency))
which(!complete.cases(politeness))

boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)

#only fixed (error!)
lmer(frequency ~ attitude, data=politeness)

#mixed: The last command created a model that used the fixed effect “attitude”
#   (polite vs. informal) to predict voice pitch,
#   controlling for by-subject and by-item variability.
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)

summary(politeness.model)
politeness.model = lmer(frequency ~ attitude +
                          gender + (1|subject) +
                          (1|scenario), data=politeness)
summary(politeness.model)

#construct null model, change REML to false when comparing for likelihood
politeness.null = lmer(frequency ~ gender +
                         (1|subject) + (1|scenario), data=politeness,
                       REML=FALSE)
#redo full model with REML=False
politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)

#ANOVA the models!
anova(politeness.null,politeness.model)

#“... politeness affected pitch (χ2(1)=11.62, p=0.00065), 
#lowering it by about 19.7 Hz ± 5.6 (standard errors) ...”