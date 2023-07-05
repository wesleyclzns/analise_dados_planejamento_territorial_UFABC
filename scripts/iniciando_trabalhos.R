#Baseado no roteiro 3

# INSTALAR A TIDYVERSE EM NOVOS PCs ANTES DE RODAR O SCRIPT
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("Hmisc")

#RODAR SEMPRE PARA CARREGAR A LIB
library(tidyverse)
library(corrplot)
library(Hmisc)

od <- read.csv("D:/CM/ADPT/dados/od.csv")

od["tmv_somaTotalModal"] <- od$tmv_coletivo + od$tmv_individual + od$tmv_pe + od$tmv_bike
od$tmv_somaTotalModal

plot(x = od$renPerCap,
     y = od$tmv_somaTotalModal,
     xlab = "Renda Per Capita",
     ylab = "∑ tempo medio de viagem (min)")

#Acredito que não deveria estar fazendo o somatorio do TMV e sim a Media das Medias do TMV

#Renda media / Media do Tempo Medio de Viagem
od["tmv_mediaTotalModal"] <- od$tmv_somaTotalModal/4
od$tmv_mediaTotalModal

plot(x = od$renPerCap,
     y = od$tmv_mediaTotalModal,
     xlab = "Renda Per Capita",
     ylab = "Media dos tempo medio de viagem (min)")

#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_mediaTotalModal,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.3256155