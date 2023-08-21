# D:/CM/ADPT (setwd)

#Instalando pacotes
install.packages("tidyverse")
install.packages("performance")
install.packages("broom")

#Importando
library(tidyverse)
library(performance)
library(broom)

agua_rede1 <- read.csv2("https://raw.githubusercontent.com/luisfelipebr/mti/master/dados/agua_rede1.csv", encoding="UTF-8")

names(agua_rede1)
modelo1 <- lm(formula = CONSUMO1 ~ RENDAPITA, data = agua_rede1, na.action = na.exclude)
summary(modelo1)

pesOD <- read.csv("D:/CM/ADPT/dados/pesOD_taxas.csv")

head(pesOD)
names(pesOD)

#Coeficiente de correlação
cor(x = (pesOD$vp_taxiNCom+pesOD$vp_taxiCom),
    y = pesOD$rendF5,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.4054405

cor.test(x = (pesOD$vp_taxiNCom+pesOD$vp_taxiCom),
         y = pesOD$rendF5,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)


taxis <- (pesOD$vp_taxiNCom+pesOD$vp_taxiCom)

#REgressão
modelo <- lm(formula = taxis ~ rendF5, data = pesOD, na.action = na.exclude)

summary(modelo)
plot(modelo, which = 1)
plot(modelo, which = 2)
plot(modelo, which = 3)
plot(modelo, which = 4)
plot(modelo, which = 5)
plot(modelo, which = 6)


plot(x = (pesOD$vp_taxiNCom+pesOD$vp_taxiCom),
     y = pesOD$rendF5,
     xlab = "Taxis comuns e APPs",
     ylab = "Pessoas com renda acima de R$ 11.448,00")
abline(modelo, col = "red")

