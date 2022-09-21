install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg", "gplots"))
library(ggplot2)
install.packages("extrafont")
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)


crop.data <- read.csv("C:/Users/drang/raw_data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "factor", "factor", "numeric"))
summary(crop.data)
colnames(crop.data)
df <- data.frame(crop.data)

set.seed(1234)
dplyr::sample_n(crop.data, 10)
levels(crop.data$group)
# $ return for features
library("ggpubr")
ggline(crop.data, x = "Pclass", y = "Survived", 
       add = c("mean_se", "jitter"), 
       order=c("1","2","3"),
       ylab = "survive", xlab = "pclas")

one.way <- aov(Survived ~ Pclass , data = crop.data)
summary(one.way)


two.way <- aov(Survived ~ Sex_Code + Pclass , data = crop.data)
summary(two.way)

blocking <- aov(Survived ~ Sex_Code +Pclass +Embarked_Code +Title_Code +FamilySize+ AgeBin_Code+FareBin_Code, data = crop.data)
summary(blocking)

interaction <- aov(Survived ~ Sex_Code +Pclass +Embarked_Code +Title_Code +FamilySize* AgeBin_Code*FareBin_Code, data = crop.data)
summary(interaction)



# Find the best-fit model using aic criterion l by balancing the variation explained against the number of parameters used.
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way","interaction", "blocking")
aictab(model.set, modnames = model.names)

#Check for homoscedasticity

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#a post-hoc test
tukey.interaction<-TukeyHSD(interaction)
tukey.interaction


tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

#Find the groupwise differences
tukey.plot.aov<-aov(Survived ~ Sex_Code:Pclass, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


#Make a data frame with the group labels  error throw
mean.Survived.data <- crop.data %>%
  group_by( Embarked_Code ) %>%
  summarise(Survived = mean(Survived))


mean.Survived.data


#Plot the raw data
two.way.plot <- ggplot(crop.data, aes(x = Sex_Code, y =Pclass), group=Survived) + geom_point(cex = 1, pch = 1,position = position_jitter(w = 0.1, h = 0.1))
two.way.plot

#Add the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.Pclass.data, aes(x="Sex_Code", y="Pclass"))

two.way.plot


#CHI SQUARE TEST


lit <- list("Sex_Code", "Pclass", "Embarked_Code", "Title_Code", "FamilySize", "AgeBin_Code", "FareBin_Code")
for (i in lit)
  {
  print(i)
  
  chis <- chisq.test(crop.data[i], crop.data$Survived, simulate.p.value = TRUE)
  print(chis)
  }


library(tidyverse)
library(extrafont)
library(extrafontdb)
library(ggplot2)

#preprocessing survived
crop.data <- crop.data %>% 
  mutate(Survived = factor(Survived, levels = c(0, 1), labels = c("Died", "Survived")))
head(crop.data)


#change fill for pclass etc also + geom_histogram() will give hist
crop.data %>% 
  ggplot(aes(x = Sex_Code, fill = Survived)) +
  geom_bar(width = 0.4) +
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Sex", x = NULL, y = "Passenger count")

#hist for discrete count variable
crop.data %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(width = 0.4) + 
  geom_histogram(stat = "count")+
  theme_classic() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Sex", x = NULL, y = "Passenger count")

# facet wrap 

crop.data %>% 
  ggplot(aes(x = Sex_Code, fill = Survived)) +
  geom_bar(width = 0.4) + 
  facet_wrap(~ Pclass) +
  theme_test() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Sex and PCLASS", x = NULL, y = "Passenger count")


#if given continuous x will give density like
crop.data %>% 
  ggplot(aes(x = AgeBin_Code, fill = Survived)) +
  geom_histogram(stat="count") + 
  facet_wrap(~ Pclass + Sex_Code) +
  theme_test() +
  theme(
    plot.title = element_text(family = "Times New Roman", hjust = 0.5),
    axis.text = element_text(family = "Times New Roman",face = "bold"),
    axis.title = element_text(family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Times New Roman")
    
  ) +
  labs(title = "Survival rates by Sex and PCLASS", x = NULL, y = "Passenger count")