getwd()
#"D:/CM/ADPT"
odJuntada <- read.csv("D:/CM/ADPT/odJuntada.csv")
names(odJuntada)
head(odJuntada)
str(odJuntada)

mean(odJuntada$popFem, na.rm = TRUE)
boxplot(odJuntada$populacao)
hist(odJuntada$populacao)

qqnorm(odJuntada$populacao)
qqline(odJuntada$populacao, col = "red")

