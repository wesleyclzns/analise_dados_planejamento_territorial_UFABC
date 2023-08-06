setwd("D:/CM/ADPT/scripts")
getwd()
# "D:/CM/ADPT/scripts" 

#Importando dados
pesqOD <- read_csv("D:/CM/ADPT/dados/od.csv")

#olhando os dados
names(pesqOD)
head(pesqOD)
tail(pesqOD)

str(pesqOD)
summary(pesqOD)

#Criando um subconjunto
pesqOD[1:6,]
pesqOD[1:6,6] #Matricula escolar
pesqOD[1:6,"familias"] #numero de familias na zona
pesqOD[1:6,]$renTotalZona #Renda Total da zona
pesqOD[1:6,c("nomeZona", "domicilios")] #Nome da Zona e Domicilio

pesqOD[which(pesqOD$renMedFam > 1000),c("nomeZona", "vt_coletivo", "renMedFam")]

table(pesqOD$nomeZona) #Não tem variaiveis categoricas nos dados q estou usando

#Definindo uma nova variavel
pesOD["tmv_somaTotalModal"] <- pesOD$tmv_coletivo + pesOD$tmv_individual + pesOD$tmv_pe + pesOD$tmv_bike

pesOD[1:20,c("nomeZona", "tmv_somaTotalModal")]


#Estatisticas Basicas
mean(pesOD$vp_bike, na.rm = TRUE)
# 729.1528

median(pesOD$vp_bike, na.rm = TRUE)
#283

var(pesOD$vp_bike, na.rm = TRUE)
#1522838

sd(pesOD$vp_bike, na.rm = TRUE)
#1234.033

#Graficos

boxplot(pesOD$vp_bike)
hist(pesOD$vp_bike)

qqnorm(pesOD$vp_bike)
qqline(pesOD$vp_bike, col = "red")

##SALVAR DADOS
write.csv2(pesOD, "D:/CM/ADPT/dados/pesOD.csv", row.names = FALSE)
