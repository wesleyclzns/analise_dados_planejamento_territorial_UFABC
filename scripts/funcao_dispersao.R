pesOD <- read.csv("D:/CM/ADPT/dados/pesOD_taxas.csv")

head(pesOD)
names(pesOD)

Dispercao <- function (df, dadox , dadoy, labelDadoX, labelDadoy) {
  
  # PLOT
  plot(x = df[[dadox]],
       y = df[[dadoy]],
       xlab = labelDadoX,
       ylab = labelDadoy)
  
  df_clean <- na.omit(df) # Remove observações com valores NA
  
  # Ajustar o modelo de regressão linear com os dados limpos
  lm_model <- lm(df_clean[[dadoy]] ~ df_clean[[dadox]], data = df_clean)
  # Adicione a linha de tendência ao gráfico base
  abline(lm_model, col = "red")
  
  # Salvar a imagem
  nome_arquivo <- paste(dadox, "_", dadoy, ".png", sep = "")
  nome_completo_arquivo <- file.path("D:/CM/ADPT/graficos/funcao", nome_arquivo)
  dev.copy(png, nome_completo_arquivo)
  dev.off()
  
} 


# Loop através das colunas para gráficos de dispersão
colunas_excluir <- c("zona", "nomeZona")  # Colunas para excluir

for (dadox in setdiff(names(pesOD), colunas_excluir)) {
  for (dadoy in setdiff(names(pesOD), c(colunas_excluir, dadox))) {
    labelDadoX <- dadox
    labelDadoy <- dadoy
    
    Dispercao(pesOD, dadox, dadoy, labelDadoX, labelDadoy)
    
    # Adicionar uma pausa entre os gráficos (opcional)
    Sys.sleep(.01)  # Ajuste a duração do intervalo conforme necessário
  }
}
