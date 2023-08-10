#Baseado no roteiro 4
#Caminho para a pasta: D:\CM\ADPT

# INSTALAR EM NOVOS PCs ANTES DE RODAR O SCRIPT
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("Hmisc")
#install.packages("ggplot2")



#RODAR SEMPRE PARA CARREGAR A LIB
library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)

getwd()
#"D:/CM/ADPT/scripts"

od <- read.csv("D:/CM/ADPT/dados/od.csv")
head(od)
names(od) #117 colunas

od["tmv_somaTotalModal"] <- od$tmv_coletivo + od$tmv_individual + od$tmv_pe + od$tmv_bike
od$tmv_somaTotalModal

#Renda media / Media do Tempo Medio de Viagem
od["tmv_mediaTotalModal"] <- od$tmv_somaTotalModal/4

Dispercao <- function(df, dadox, dadoy, labelDadoX, labelDadoy, outputFolder) {
  
  df_clean <- na.omit(df) # Remove observações com valores NA
  
  # Ajustar o modelo de regressão linear com os dados limpos
  lm_model <- lm(df_clean[[dadoy]] ~ df_clean[[dadox]], data = df_clean)
  
  #Cria o gráfico da plot
  plot(x = df[[dadox]],
       y = df[[dadoy]],
       xlab = labelDadoX,
       ylab = labelDadoy)
  
  #Salve o gráfico plot em um arquivo png
  outputFileName <- paste(labelDadoX, labelDadoy, "_plot.png", sep = "-")
  outputFilePath <- file.path(outputFolder, outputFileName)
  dev.copy(png, file = outputFilePath, width = 800, height = 600)
  dev.off()
  
  # Cria o Grafico do GGPLOT
  library(ggplot2)
  gg <- ggplot(data = df_clean, aes(x = .data[[dadox]], y = .data[[dadoy]])) +   
    geom_point() +   
    geom_smooth(method = "lm", col = "red", se = FALSE) + 
    theme_bw() +   
    xlab(labelDadoX) +   
    ylab(labelDadoy)
  
  # Salvar o gráfico do GG em um arquivo png
  outputFileName <- paste(labelDadoX, labelDadoy, "_gg.png", sep = "-")
  outputFilePath <- file.path(outputFolder, outputFileName)
  ggsave(filename = outputFilePath, plot = gg, width = 8, height = 6)
}


# Exemplo de uso da Função dispersão
outputFolder <- "./graficos/teste_funcao/"
dadoX <- "renPerCap"
dadoY <- "tmv_somaTotalModal"
labelX <- "Renda Per Capita"
labelY <- "∑ tempo medio de viagem (min)"
Dispercao(od, dadoX, dadoY, labelX, labelY, outputFolder)

library(dplyr)
library(corrplot)

MatrizCorrelacao <- function(data, cols_excluir = character(0), tamanho = 1500) {
  # Excluir colunas especificadas
  data_selecionada <- data %>% select(-all_of(cols_excluir))
  
  # Matriz de Correlação
  cor_matrix <- cor(data_selecionada, method = "pearson", use = "pairwise.complete.obs") %>%
    round(digits = 2)
  
  # Personalizar a matriz de correlação
  png("matriz_correlacao.png", width = tamanho, height = tamanho, res = 300)
  corrplot(cor_matrix, method = "color")
  dev.off()
}

# Exemplo de uso
MatrizCorrelacao(od, cols_excluir = c("zona", "nomeZona"), tamanho = 3000)  # Substitua pelas colunas que deseja excluir e pelo tamanho desejado





names(od)
head(od)


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


# Realize o teste de correlação
corTest_Resultado <- cor.test(x = od$renPerCap,
                              y = od$tmv_mediaTotalModal,
                              method = "pearson",
                              alternative = "two.sided",
                              conf.level = 0.95)

# Adicione os resultados à coluna do conjunto de dados
od$corTest <- paste("t =", corTest_Resultado$statistic,
                             "p-value =", format(corTest_Resultado$p.value, scientific = TRUE),
                             "CI =", paste(corTest_Resultado$conf.int, collapse = "-"),
                             "cor =", corTest_Resultado$estimate)

names(od)
head(od)
od <- subset(od, select = -corTest) #codigo para deletar coluna

od["mo_trabTotal"] <- od$mo_traIndus + od$mo_trabCome + od$mo_trabServ
od$tmv_mo_trabTotal

od["crianças"] <- od$popIdd_0.3 + od$popIdd_4.6 + od$popIdd_7.10
od["adolecentes"] <- od$popIdd_11.14 + od$popIdd_15.17 
od["jovens"] <- od$popIdd_18.22 + od$popIdd_23.29
od["adultos"] <- od$popIdd_30.39 + od$popIdd_40.49 + od$popIdd_50.59
od["idosos"] <- od$popIdd_60.mais
od[1:6,c("nomeZona", "crianças", "adolecentes", "jovens", "adultos", "idosos")] 

#Matriz de Correlação
names(od)
od %>%
  select(populacao, matriEscolar, empregos, renMedFam, res_assSem, res_autonomo, res_trabFamiliar, vp_dirigindoAuto, vp_passAuto, vp_taxiCom, vp_taxiNCom, vp_dirigindoMoto, vp_passMoto, vp_bike, vp_pe, tmv_coletivo, tmv_individual, tmv_pe, tmv_bike, mo_educacao, mo_procEmp, mo_lazer, mo_saude, mo_trabTotal, crianças, adolecentes, jovens, adultos, idosos, popFem, popMasc, renPerCap, renMedianFam, renMedFam) %>%
  cor(method = "pearson",
      use = "complete.obs") %>%
      round(digits = 2) %>%
      corrplot(method = c("pie"),
               tl.cex = 1.5, cl.cex = 1.5)         


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


#Viagem atraída -> renda total da zona

plot(x = od$rendTotal,
     y = od$viagAtraid,
     xlab = "Renda total",
     ylab = "Viagens atraías")

ggplot(data = od, aes(x = rendTotal, y =viagAtraid)) +   geom_point() +   geom_smooth(data = lm(formula = viagAtraid ~ rendTotal, data = od), method = "lm", col = "red", se = FALSE) +   theme_bw() +   xlab("Renda total") +   ylab("Viagens atraidas")


#Coeficiente de correlação
cor(x = od$rendTotal,
    y = od$viagAtraid,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.8188885

cor.test(x = od$rendTotal,
         y = od$viagAtraid,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 32.378
#df = 515
#p-value = 2.2e-16 ou 0.00000000000000022
#Não existe correlação entre o viagens atraidas e renda total da zona

#APOS A PRESENTAÇÃO DA P1

#População / viagem produz = popProdut

od["popProdViagem"] <- od$populacao / od$viagProduz
od$popProdViagem

#População  / viagem atraída = popAtratora (isso faz sentido? Acho que não)

od["popAtratoraViag"] <- od$populacao / od$viagAtraid
od$popAtratoraViag

#Viagem atraída & produzida -> local emprego (dentro/fora resid sem end)

#Dispersão

#População atratora de viagens -> Empregos
plot(x = od$popAtratoraViag,
     y = od$empregos,
     xlab = "Pop Atratora",
     ylab = "Empregos")

ggplot(data = od, aes(x = popAtratoraViag, y =empregos)) +   geom_point() +   geom_smooth(data = lm(formula = popAtratoraViag ~ empregos, data = od), method = "lm", col = "blue", se = FALSE) +   theme_bw() +   xlab("Pop Atratora") +   ylab("Empregos")


#População produtora de viagens -> Empregos
plot(x = od$popProdViagem,
     y = od$empregos,
     xlab = "Pop Produtora",
     ylab = "Empregos")

ggplot(data = od, aes(x = popProdViagem, y =empregos)) +   geom_point() +   geom_smooth(data = lm(formula = popProdViagem ~ empregos, data = od), method = "lm", col = "blue", se = FALSE) +   theme_bw() +   xlab("Pop Produtora") +   ylab("Empregos")


#Não faço ideia do que isso signifique!
