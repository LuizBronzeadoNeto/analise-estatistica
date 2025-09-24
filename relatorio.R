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

par(mfrow = c(3, 3))
#Plots de análise univariável
hist(dados$consumo_energia, main = "Consumo")
hist(dados$area_m2, main = "Área")
hist(dados$num_moradores, main = "número moradores")
hist(dados$temperatura_media, main = "temperatura")
hist(dados$renda_familiar, main = "Renda")
hist(dados$equipamentos_eletro, main = "equipamentos")
hist(dados$potencia_total_equipamentos, main = "Pot total")
barplot(table(dados$uso_ar_condicionado), main = "uso ar condicionado")
barplot(table(dados$tipo_construcao),main = "Distribuição dos tipos de construção")


#boxplot(dados$consumo_energia, main = "Consumo")
#boxplot(dados$area_m2, main = "Área")
#boxplot(dados$num_moradores, main = "número moradores")
#boxplot(dados$temperatura_media, main = "Temperatura")
#boxplot(dados$renda_familiar, main = "Renda")
#boxplot(dados$equipamentos_eletro, main = "equipamentos")
#boxplot(dados$potencia_total_equipamentos, main = "pot total")

num_vars <- dados[, c("num_moradores", "area_m2", "temperatura_media",
                      "renda_familiar", "equipamentos_eletro",
                      "potencia_total_equipamentos", "consumo_energia")]
print("matriz de correlação completa:")
print(cor(num_vars, use = "complete.obs"))
pairs(num_vars, main = "Matriz de dispersão")

#calculando Variance inflation factor
vif <- function(modelo) {
  vifs <- c()
  X <- model.matrix(modelo)[, -1]  # matriz de preditores (sem intercepto)
  
  for (i in 1:ncol(X)) {
    y <- X[, i]
    X_aux <- X[, -i, drop = FALSE]
    modelo_aux <- lm(y ~ X_aux)
    R2 <- summary(modelo_aux)$r.squared
    vifs[i] <- 1 / (1 - R2)
  }
  
  names(vifs) <- colnames(X)
  return(vifs)
}
modelo_aux <- lm(consumo_energia ~ num_moradores + area_m2 + temperatura_media +
                   renda_familiar + equipamentos_eletro +
                   potencia_total_equipamentos, data = dados)

print("vif:")
print(vif(modelo_aux))


#modelo mlrs

#modelo_inicial <- lm(consumo_energia ~ num_moradores + area_m2 + temperatura_media +
#                       renda_familiar + uso_ar_condicionado + tipo_construcao +
#                       + equipamentos_eletro + potencia_total_equipamentos, data = dados)

cat("\n", "\n", "\n")
#selecionando o melhor modelo com stepwise regression
modelo_inicial <- lm(consumo_energia ~ ., data = dados)
modelo_step <- step(modelo_inicial, direction = "both")
print(summary(modelo_step))
