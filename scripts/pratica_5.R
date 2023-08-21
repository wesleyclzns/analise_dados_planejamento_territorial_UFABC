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
