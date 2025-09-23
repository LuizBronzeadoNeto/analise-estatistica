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
#dados$potencia_total_equipamentos
outliers_iqr <- function(x) #detectar outliers usando método iqr
{
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limites <- c(q1 - 1.5 * iqr, q3 + 1.5 * iqr)
  which(x < limites[1] | x > limites[2])
}

outliers_consumo <- outliers_iqr(dados$consumo_energia)
outliers_moradores <- outliers_iqr(dados$num_moradores)
outliers_area <- outliers_iqr(dados$area_m2)
outliers_temperatura <- outliers_iqr(dados$temperatura_media)
outliers_renda <- outliers_iqr(dados$renda_familiar)
outliers_eletro <- outliers_iqr(dados$equipamentos_eletro)
outliers_pot_eletro <- outliers_iqr(dados$potencia_total_equipamentos)

print("outliers consumo:")
print(cbind(indice = outliers_consumo, 
      valor = dados$consumo_energia[outliers_consumo]))
print("outliers moradores:")
print(cbind(indice = outliers_moradores, 
      valor = dados$num_moradores[outliers_moradores]))
print("outliers área:")
print(cbind(indice = outliers_area, 
      valor = dados$area_m2[outliers_area]))
print("outliers temperatura:")
print(cbind(indice = outliers_temperatura, 
      valor = dados$temperatura_media[outliers_temperatura]))
print("outliers equipamento elétrico:")
print(cbind(indice = outliers_eletro, 
      valor = dados$equipamentos_eletro[outliers_eletro]))
print("outliers potencia equipamento:")
print(cbind(indice = outliers_pot_eletro, 
      valor = dados$potencia_total_equipamentos[outliers_pot_eletro]))
print("outliers renda:")
print(cbind(indice = outliers_renda, 
      valor = dados$renda_familiar[outliers_renda]))
