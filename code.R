#Q1
dataset1<-read_csv("~/Desktop/dataset1.csv")
dataset1_anova<-aov(Mileage~Company,data=dataset1)
summary(dataset1_anova)

library(multcomp)
pairwise.t.test(dataset1$Mileage, dataset1$Company, p.adjust.method = "none")

library(ggplot2)
#ggplot(dataset1, aes(x=Company, y=Mileage, fill=Company)) + geom_boxplot()
ggplot(dataset1, aes(x=Company, y=Mileage, fill=Company)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + 
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red")

#Q2
dataset2<- read_csv("~/Desktop/dataset2.csv")
dataset2_anova<-aov(Mileage~Company+Error(user/Company),data=dataset2)
summary(dataset2_anova)

library(multcomp)
pairwise.t.test(dataset2$Mileage, dataset2$Company, p.adjust.method = "none")

ggplot(dataset2, aes(x = Company, y = Mileage, fill = Company)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + 
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", width = 0.2, color = "red")
