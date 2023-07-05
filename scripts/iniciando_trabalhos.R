#Baseado no roteiro 3
#Caminho para a pasta: D:\CM\ADPT

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

ggplot(data = od, aes(x = renPerCap, y =tmv_somaTotalModal)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_somaTotalModal ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("∑ tempo medio de viagem (min)")

#Acredito que não deveria estar fazendo o somatorio do TMV e sim a Media das Medias do TMV

#Renda media / Media do Tempo Medio de Viagem
od["tmv_mediaTotalModal"] <- od$tmv_somaTotalModal/4
od$tmv_mediaTotalModal

plot(x = od$renPerCap,
     y = od$tmv_mediaTotalModal,
     xlab = "Renda Per Capita",
     ylab = "Media dos tempo medio de viagem (min)")

ggplot(data = od, aes(x = renPerCap, y =tmv_mediaTotalModal)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_mediaTotalModal ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("Media dos tempo medio de viagem (min)")


#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_mediaTotalModal,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.3256155

cor.test(x = od$renPerCap,
         y = od$tmv_mediaTotalModal,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 7.8153
#df = 515
#p-value = 3.108e-14 ou 0.00000000000003108
# Rejeitamos a hipotese nula
#Pode existir alguma correlação entre a renda percapita e a media do tempo medio de viagens

#Renda media / TMV Individual
plot(x = od$renPerCap,
     y = od$tmv_individual,
     xlab = "Renda per capita",
     ylab = "Tempo medio de viagens em transporte individual (min)")

ggplot(data = od, aes(x = renPerCap, y =tmv_individual)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_individual ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("Tempo medio de viagens em transporte individual (min)")


#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_individual,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.08418179

cor.test(x = od$renPerCap,
         y = od$tmv_individual,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 1.9172
#df = 515
#p-value = 0.05577
# Aceitamos a hipotese nula
#Nãoe existi alguma correlação entre a renda per capita e o tempo medio de viagens em transporte individual

#Renda media / TMV A Pe
plot(x = od$renPerCap,
     y = od$tmv_pe,
     xlab = "Renda per capita",
     ylab = "Tempo medio de viagens em a pe (min)")

ggplot(data = od, aes(x = renPerCap, y =tmv_pe)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_pe ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("Tempo medio de viagens em a pe (min)")


#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_pe,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  -0.1017915

cor.test(x = od$renPerCap,
         y = od$tmv_pe,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = -2.3221
#df = 515
#p-value = 0.02062
#Não existe correlação entre o tempo de viagens a pe e a renda per capita

#Renda media / TMV Bike
plot(x = od$renPerCap,
     y = od$tmv_bike,
     xlab = "Renda per capita",
     ylab = "Tempo medio de viagens em bicicleta (min)")

ggplot(data = od, aes(x = renPerCap, y =tmv_bike)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_bike ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("Tempo medio de viagens em bicicleta (min)")


#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_bike,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.1787979

cor.test(x = od$renPerCap,
         y = od$tmv_bike,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 4.124
#df = 515
#p-value = 4.34e-05 ou 0.0000434
#Rejeitamos a hipotese Nula
#Pode existir correlação entre o tempo de viagens de bicicleta e a renda per capita

#Renda media / TMV Coletivo
plot(x = od$renPerCap,
     y = od$tmv_coletivo,
     xlab = "Renda per capita",
     ylab = "Tempo medio de viagens em transporte coletivo (min)")

ggplot(data = od, aes(x = renPerCap, y =tmv_coletivo)) +   geom_point() +   geom_smooth(data = lm(formula = tmv_coletivo ~ renPerCap, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda per capita") +   ylab("Tempo medio de viagens em transporte coletivo (min)")


#Coeficiente de correlação
cor(x = od$renPerCap,
    y = od$tmv_coletivo,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.4034475

cor.test(x = od$renPerCap,
         y = od$tmv_coletivo,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 10.006
#df = 515
#p-value = 2.2e-16
# Aceitamos a hipotese nula
#Nãoe existi alguma correlação entre a renda per capita e o tempo medio de viagens em transporte coletivo
