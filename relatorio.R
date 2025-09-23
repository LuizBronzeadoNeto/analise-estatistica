dados <- read.csv("dados_consumo_energia.csv", sep = ",", dec = ".", stringsAsFactors = TRUE)
print("summary")
print(summary(dados)) #summary
print("checando por ausências")
print(colSums(is.na(dados))) 
print("dados duplicados")
print(sum(duplicated(dados)))

#dados$num_moradores
#dados$area_m2
#dados$temperatura_media
#dados$renda_familiar
#dados$uso_ar_condicionado
#dados$tipo_construcao
#dados$isolamento_termico
#dados$equipamentos_eletro

outliers_iqr <- function(x) #detectar outliers usando método iqr
{
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limites <- c(q1 - 1.5 * iqr, q3 + 1.5 * iqr)
  which(x < limites[1] | x > limites[2])
}
print("outliers:")
outliers_consumo <- outliers_iqr(dados$consumo_energia)
print(cbind(indice = outliers_consumo, 
      valor = dados$consumo_energia[outliers_consumo]))
boxplot(dados$consumo_energia, 
        main = "Consumo de energia - Outliers",
        ylab = "kWh")
