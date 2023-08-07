getwd()
# "C:/Users/w.cafe/Documents/analise_dados_planejamento_territorial_UFABC"

pesOD <- read.csv("~/analise_dados_planejamento_territorial_UFABC/dados/pesOD.csv")
head(pesOD)
names(pesOD)

#install.packages(dplyr)
library(dplyr)

#Criando Campus de Somatorias
pesOD <- pesOD %>%
  mutate(
    tmv_somaTotalModal = tmv_coletivo + tmv_individual + tmv_pe + tmv_bike,
    tmv_mediaTotalModal = tmv_somaTotalModal / 4,
    
    crianças = ((popIdd_0.3 + popIdd_4.6 + popIdd_7.10) /popTotal )*100, 
    adolecentes = ((popIdd_11.14 + popIdd_15.17) /popTotal )*100, 
    jovens = ((popIdd_18.22 + popIdd_23.29) /popTotal) *100, 
    adultos = ((popIdd_30.39 + popIdd_40.49 + popIdd_50.59) / popTotal) *100, 
    idosos = popIdd_60.mais 
  )
names(pesOD)

#Criando Taxas
pesOD <- pesOD %>%
  mutate(
    tax_rendF1 = (pesOD$rendF1 / pesOD$rendTotal) *100,
    tax_rendF2 = (pesOD$rendF2 / pesOD$rendTotal) *100,
    tax_rendF3 = (pesOD$rendF3 / pesOD$rendTotal) *100,
    tax_rendF4 = (pesOD$rendF4 / pesOD$rendTotal) *100,
    tax_rendF5 = (pesOD$rendF5 / pesOD$rendTotal) *100,
    
    tax_vp_coletivoPub = (vp_trem + vp_bus + vp_metro),
    idosos = popIdd_60.mais
  )
names(pesOD)





