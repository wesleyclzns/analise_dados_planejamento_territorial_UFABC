getwd()
#"D:/CM/ADPT"
od <- read.csv("D:/CM/ADPT/dados/od.csv")
names(od)
head(od)
str(od)

mean(od$popFem, na.rm = TRUE)
boxplot(od$populacao)
hist(od$populacao)

qqnorm(od$populacao)
qqline(od$populacao, col = "red")

hist(od$renPerCap)
boxplot(od$renPerCap)

qqnorm(od$renPerCap)
qqline(od$renPerCap, col = "red")
