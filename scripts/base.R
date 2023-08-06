#Nome da variavel

#=======================
#Dados Para Troca
dadox = od$renPerCa
labelDadoX = "Teste"
dadoy = od$taxaColetivoTotal
labelDadoy = "tessteeeyy"
#=======================

plot(x = dadox,
     y = dadoy,
     xlab = labelDadoX,
     ylab = labelDadoy)

od <- na.omit(od) # Remove observações com valores NA

# Remove observações com valores ausentes ou infinitos da variável y
data_clean <- na.omit(od)

# Ajustar o modelo de regressão linear com os dados limpos
lm_model <- lm(y ~ x, data = data_clean)

ggplot(data = od, aes(x = dadox, y = dadoy)) +   
  geom_point() +   geom_smooth(data = lm(formula = dadoy ~ dadox, data = od), 
  method = "lm", col = "red", se = FALSE) + theme_bw() +   
  xlab(labelDadoX) +   
  ylab(labelDadoy)


#Coeficiente de correlação
cor(x = dadox,
    y = dadoy,
    method = "pearson",
    use = "complete.obs")
# Resultado - >  0.4034475

cor.test(x = odadox,
         y = dadoy,
         method = "pearson",
         alternative = "two.sided",
         conf.level = 0.95)
#t = 10.006
#df = 515
#p-value = 2.2e-16
# Aceitamos a hipotese nula
#Nãoe existi alguma correlação entre a renda per capita e o tempo medio de viagens em transporte coletivo