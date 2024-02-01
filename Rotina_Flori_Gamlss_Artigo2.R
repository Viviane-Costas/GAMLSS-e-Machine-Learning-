## Análise GAMLSS do Segundo Artigo 
## Florianopólis 10 anos 

## Pacotes necessários
library(gamlss)
library(e1071)
library(gamlss.foreach)
library(zoo)
library(tictoc)
library(forecast)

## Diretório do R
setwd("G:\\Meu Drive\\Mestrado\\Dissertação\\Análises\\Temp de Flori - 2º Artigo\\Florianopolis 10 anos")

## Banco de Dados
dados <- readxl::read_xlsx("Atualizada.xlsx")
names(dados)
head(dados)
summary(dados)
colnames(dados)<-c("Data_Medicao", "PRECIPITACAO", "PRESSAO",      "TEMP_ORV",     "TEMP_MEDIA",   "UMIDADE",      "VENT_MED")
dados$Data_Medicao <- as.Date(dados$Data_Medicao)

## Calculando estatísticas
skewness(dados$TEMP_MEDIA)
kurtosis(dados$TEMP_MEDIA)
var(dados$TEMP_MEDIA)
sd(dados$TEMP_MEDIA)


## Relação das covariáveis com a resposta

plot(TEMP_MEDIA ~ Data_Medicao, data = dados, type = "l", xlab = "Tempo (Anos)", ylab = "Temperatura (ºC)", col = 1, lty = 1)
plot(TEMP_MEDIA~PRECIPITACAO, data = dados, xlab = "Precipitação (mm)", ylab = "Temperatura (ºC)", col= 1,pch = 16, cex = 0.5)
plot(TEMP_MEDIA~PRESSAO, data = dados,  xlab = "Pressão Atmosférica (mB)", ylab = "Temperatura (ºC)", col=1,pch = 16, cex = 0.5)
plot(TEMP_MEDIA~TEMP_ORV, data = dados, xlab = "Temperatura de Orvalho (ºC)", ylab = "Temperatura (ºC)", col=1,pch = 16, cex = 0.5)
plot(TEMP_MEDIA~UMIDADE, data = dados, xlab = "Umidade (%)", ylab = "Temperatura (ºC)", col=1,pch = 16, cex = 0.5)
plot(TEMP_MEDIA ~ VENT_MED, data = dados, xlab = expression("Velocidade do vento (m.s"^{-1} * ")"), ylab = "Temperatura (ºC)", col = 1, pch = 16, cex = 0.5)

## Histograma e BloxPlot

hist(dados$TEMP_MEDIA, main="", xlab="Temperatura (ºC)", ylab="Frequência", col="gray")
lines(density(dados$TEMP_MEDIA,na.rm=TRUE),col=1)
boxplot(dados$TEMP_MEDIA,horizontal=FALSE, col = "gray", ylab = "Temperatura (ºC)")


# Calcular estatísticas do boxplot
boxplot_stats <- boxplot.stats(dados$TEMP_MEDIA)
boxplot_stats 

# Obtendo os limites superior e inferior
lower_limit <- boxplot_stats$out[1]
upper_limit <- boxplot_stats$out[length(boxplot_stats$out)]

# Identificando outliers
outliers <- dados$TEMP_MEDIA[dados$TEMP_MEDIA < upper_limit ]
outliers

## ________________________________________________________________________

## Divisão dos dados - Treinamento e Teste

seed(123)
fim<-round(length(dados$Data_Medicao)*0.7) # 70% dos dados
training <- dados[1:fim,]
dim(training)
test <- dados[(fim+1):length(dados$Data_Medicao),]
dim(test)

y2<-dados$Data_Medicao[length(dados$Data_Medicao)]
y1<-dados$Data_Medicao[fim+1]

# Posição das legendas
x2<-dados$Data_Medicao[fim+500]
x1<-dados$Data_Medicao[1+500]


f1 <- fitDist(training$TEMP_MEDIA, type = 'realplus')
f1$fits[1:10]

## Análise dos modelos Gamlss - (BCTo, BCPEo, BCCGo e WEI3)

# ___________________________
# Com funções de  suavização 
# ___________________________

Model_BCTo <- gamlss(TEMP_MEDIA~1,data = training, method = mixed(20,100), family = BCTo) # modelo nulo
Model_BCTo <- stepGAICAll.A(Model_BCTo, scope = list(lower=~1,
                                                     upper = ~ pb(Data_Medicao) + pb(PRECIPITACAO) + pb(PRESSAO) +
                                                       pb(TEMP_ORV) +  pb(UMIDADE) +  pb(VENT_MED)))
summary(Model_BCTo)
term.plot(Model_BCTo,pages =1, ask=FALSE, ylim = "free")


Model_BCPEo <- gamlss(TEMP_MEDIA~1,data = training, method = mixed(20,100), family = BCPEo) # modelo nulo
Model_BCPEo <- stepGAICAll.A(Model_BCPEo, scope = list(lower=~1,
                                                       upper = ~ pb(Data_Medicao) + pb(PRECIPITACAO) + pb(PRESSAO) +
                                                         pb(TEMP_ORV) +  pb(UMIDADE) +  pb(VENT_MED)))
summary(Model_BCPEo)
term.plot(Model_BCPEo,pages = 1, ask = FALSE, ylim = "free")

Model_BCCGo <- gamlss(TEMP_MEDIA~1,data = training, method = mixed(20,100), family = BCCGo) # modelo nulo
Model_BCCGo <- stepGAICAll.A(Model_BCCGo, scope = list(lower=~1,
                                                       upper = ~ pb(Data_Medicao) + pb(PRECIPITACAO) + pb(PRESSAO) +
                                                         pb(TEMP_ORV) +  pb(UMIDADE) +  pb(VENT_MED)))
summary(Model_BCCGo)
term.plot(Model_BCCGo,pages = 1,ask = FALSE, ylim = "free")


Model_WEI3 <- gamlss(TEMP_MEDIA~1,data =training, method = mixed(20,100), family = WEI3) # modelo nulo
Model_WEI3 <- stepGAICAll.A(Model_WEI3, scope = list(lower=~1,
                                                   upper = ~ pb(Data_Medicao) + pb(PRECIPITACAO) + pb(PRESSAO) +
                                                     pb(TEMP_ORV) +  pb(UMIDADE) +  pb(VENT_MED)))
summary(Model_WEI3)
term.plot(Model_WEI3,pages = 1,ask = FALSE, ylim = "free")


r1 <- gamlss(TEMP_MEDIA ~ pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
               pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
               pb(PRESSAO) + pb(VENT_MED), nu.formula = ~pb(TEMP_ORV) +  
               pb(PRESSAO), tau.formula = ~pb(TEMP_ORV) + pb(PRECIPITACAO),  
             family = BCTo, data = training, method = mixed(20, 100)) # familia BCTo
summary(r1)

r2 <- gamlss(TEMP_MEDIA ~ pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
               pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
               pb(PRESSAO) + pb(UMIDADE) + pb(VENT_MED) + pb(PRECIPITACAO),  
             nu.formula = ~pb(VENT_MED), tau.formula = ~pb(VENT_MED) +  
               pb(PRESSAO) + pb(PRECIPITACAO), family = BCPEo, data = training,  
             method = mixed(20, 100)) # familia BCPEo
summary(r2)

r3 <- gamlss(TEMP_MEDIA ~ pb(TEMP_ORV) + pb(PRECIPITACAO) +  
               pb(VENT_MED) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
               pb(PRESSAO) + pb(VENT_MED) + pb(PRECIPITACAO), nu.formula = ~pb(VENT_MED),  
             family = BCCGo, data = training, method = mixed(20, 100)) # familia BCCGo

r4 <- gamlss(TEMP_MEDIA ~ pb(TEMP_ORV) + pb(UMIDADE) + pb(PRESSAO) +  
               pb(VENT_MED) + pb(PRECIPITACAO), sigma.formula = ~pb(UMIDADE) +  
               pb(TEMP_ORV) + pb(PRESSAO) + pb(VENT_MED), family = WEI3,  
             data = training, method = mixed(20, 100)) # familia WEI3

summary(r4)



# Comparação entre os modelos ajustados (Critério de Informaçao de Akaike)

AIC(r1,r2,r3,r4)
 

# Comportamento das funções de suavização para mu

term.plot(r1, pages =1, ask = FALSE, ylim = "free")
term.plot(r2, pages=1, ask = FALSE, ylim = "free")
term.plot(r3, pages=1, ask = FALSE, ylim = "free")
term.plot(r4, pages=1, ask = FALSE, ylim = "free")

# Comportamento das funções de suavização para sigma

term.plot(r1, what="sigma",pages = 1, ask = FALSE, ylim = "free")
term.plot(r2, what="sigma", pages = 1, ask = FALSE, ylim = "free")
term.plot(r3, what="sigma", pages = 1, ask = FALSE, ylim = "free")
term.plot(r4, what="sigma", pages = 1, ask = FALSE, ylim = "free")


# Comportamento das funções de suavização para v

term.plot(r1, what='nu',pages = 1, ask = FALSE, ylim = "free") 
term.plot(r2, what='nu',pages = 1, ask = FALSE, ylim = "free") 
term.plot(r3, what='nu',pages = 1, ask = FALSE, ylim = "free")


# Comportamento das funções de suavização para t

term.plot(r1, what='tau',pages = 1, ask = FALSE, ylim = "free")
term.plot(r2, what='tau',pages = 1, ask = FALSE, ylim = "free")



# Análise de Resíduos

plot(r1,ts = TRUE)
plot(r2,ts = TRUE)
plot(r3,ts = TRUE)
plot(r4,ts = TRUE)


# Análise de Resísuos - worm plot

wp(r1, ylim.all = 1.5) ; title("Worm plot - BCTo")
wp(r2, ylim.all = 1.5) ; title("Worm plot - BCPEo")
wp(r3, ylim.all = 1.5) ; title("Worm plot - BCCGo")
wp(r4, ylim.all = 1.5) ; title("Worm plot - WEI3")



##############################################################################
###################### SERIES TEMPORAIS - GAMLSS #############################
##############################################################################



## 5 Ad-hoc estimation: The spreads again

r1$mu.df
r1$sigma.df
r1$nu.df
r1$tau.df

r2$mu.df
r2$sigma.df
r2$nu.df
r2$tau.df

r3$mu.df
r3$sigma.df
r3$nu.df
r3$tau.df

r4$mu.df
r4$sigma.df
r4$nu.df
r4$tau.df

## Obtendo resíduos de mu 

mures_r1 <-residuals(r1, what="mu")
mures_r2 <-residuals(r2, what="mu")
mures_r3 <-residuals(r3, what="mu")
mures_r4 <-residuals(r4, what="mu")

## Ajustando um modelo ARIMA para mu

a1 <- auto.arima(mures_r1)
a1

plot(ts(residuals(r1, what="mu")))
lines(fitted(a1), col="red")

a2 <- auto.arima(mures_r2)
a2

plot(ts(residuals(r2, what="mu")))
lines(fitted(a2), col="red")

a3 <- auto.arima(mures_r3)
a3

plot(ts(residuals(r3, what="mu")))
lines(fitted(a3), col="red")

a4 <- auto.arima(mures_r4)
a4

plot(ts(residuals(r4, what="mu")))
lines(fitted(a4), col="red")

## pegue os valores ajustados deste modelo e desloque-os no modelo μ

r1_1 <-  gamlss(TEMP_MEDIA ~ offset(fitted(a1)) + pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
                  pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
                  pb(PRESSAO) + pb(VENT_MED), nu.formula = ~pb(TEMP_ORV) +  
                  pb(PRESSAO), tau.formula = ~pb(TEMP_ORV) + pb(PRECIPITACAO),  
                family = BCTo, data = training, method = mixed(10, 100)) # familia BCTo

plot(r1_1,ts = TRUE)

r22_1 <-  gamlss(TEMP_MEDIA ~ offset(fitted(a2)) + pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
                   pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
                   pb(PRESSAO) + pb(UMIDADE) + pb(VENT_MED) + pb(PRECIPITACAO),  
                 nu.formula = ~pb(VENT_MED), tau.formula = ~pb(VENT_MED) +  
                   pb(PRESSAO) + pb(PRECIPITACAO), family = BCPEo, data = training,  
                 method = mixed(20, 100)) # familia BCPEo

plot(r22_1,ts = TRUE)

r33_1 <-  gamlss(TEMP_MEDIA ~ offset(fitted(a3)) + TEMP_ORV + pb(PRECIPITACAO) +  
                   pb(VENT_MED) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
                   pb(PRESSAO) + pb(VENT_MED) + pb(PRECIPITACAO), nu.formula = ~pb(VENT_MED),  
                 family = BCCGo, data = training, method = mixed(10, 200)) # familia BCCGo

plot(r33_1,ts = TRUE)


r44_1 <-  gamlss(TEMP_MEDIA ~ offset(fitted(a4)) + TEMP_ORV + UMIDADE + pb(PRESSAO) +  
                   pb(VENT_MED) + pb(PRECIPITACAO), sigma.formula = ~pb(UMIDADE) +  
                   TEMP_ORV + pb(PRESSAO) + pb(VENT_MED), family = WEI3,  
                 data = training, method = mixed(20, 100)) # familia WEI3

  
plot(r44_1,ts = TRUE)

# Função para calcular métricas dos modelos

calculate_metrics <- function(predictions, actual_values) {
  rmse <- sqrt(mean((predictions - actual_values)^2))
  mae <- mean(abs(predictions - actual_values))
  mase <- mean(abs(predictions - actual_values) / mean(abs(diff(training$TEMP_MEDIA))))
  smape <- 2 * mean(abs(predictions - actual_values) / (abs(predictions) + abs(actual_values)))
  srq <- 1 - sum((predictions - actual_values)^2) / sum((actual_values - mean(actual_values))^2)
  mape <- mean(abs((actual_values - predictions) / actual_values)) * 100
  
  return(c(RMSE = rmse, MAE = mae, MASE = mase, SMAPE = smape, SRQ = srq, MAPE = mape))
}

# Modelos
models <- list(r1_1, r22_1, r33_1, r44_1)

# Loop através dos modelos
for (i in 1:length(models)) {
  model <- models[[i]]
  
  # Fazer previsões no conjunto de treinamento
  train_predictions <- predict(model, type = "response")
  
  # Valores reais no conjunto de treinamento
  train_actual_values <- training$TEMP_MEDIA
  
  # Calcular e imprimir as métricas
  metrics <- calculate_metrics(train_predictions, train_actual_values)
  cat(paste("Métricas para o Modelo ", i, ":\n"))
  print(metrics)
  cat("\n")
}


## Aplicando o modelo que obteve as melhores métricas no conjunto de treinamento
## no conjunto teste (BCTo)

R1_teste <-  gamlss(TEMP_MEDIA ~  pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
                  pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
                  pb(PRESSAO) + pb(VENT_MED), nu.formula = ~pb(TEMP_ORV) +  
                  pb(PRESSAO), tau.formula = ~pb(TEMP_ORV) + pb(PRECIPITACAO),  
                family = BCTo, data = test) # familia BCTo

summary(R1_teste)
plot(R1_teste ,ts = TRUE)

## Obtendo resíduos de mu 
mures_R1_teste <-residuals(R1_teste, what="mu")

a1_teste <- auto.arima(mures_R1_teste)
a1_teste

plot(ts(residuals(R1_teste, what="mu")))
lines(fitted(a1_teste), col="red")


R1_teste_1 <-  gamlss(TEMP_MEDIA ~ offset(fitted(a1_teste)) + pb(TEMP_ORV) + pb(UMIDADE) + pb(VENT_MED) +  
                      pb(PRECIPITACAO) + pb(PRESSAO), sigma.formula = ~pb(TEMP_ORV) +  
                      pb(PRESSAO) + pb(VENT_MED), nu.formula = ~pb(TEMP_ORV) +  
                      pb(PRESSAO), tau.formula = ~pb(TEMP_ORV) + pb(PRECIPITACAO),  
                    family = BCTo, data = test) # familia BCTo

summary(R1_teste_1)
plot(R1_teste_1 ,ts = TRUE)

# Prevendo valores com o modelo R1_teste
predictions_R1_teste_1 <- fitted(R1_teste_1)

# Calculando RMSE, MAE, MAPE, MSE, R², MASE e SMAPE
rmse_R1_teste_1 <- sqrt(mean((test$TEMP_MEDIA - predictions_R1_teste_1)^2))
mae_R1_teste_1 <- mean(abs(test$TEMP_MEDIA - predictions_R1_teste_1))
mape_R1_teste_1 <- mean(abs((test$TEMP_MEDIA - predictions_R1_teste_1) / test$TEMP_MEDIA)) * 100
mse_R1_teste_1 <- mean((test$TEMP_MEDIA - predictions_R1_teste_1)^2)

# Calculando MASE
mae_mean <- mean(abs(test$TEMP_MEDIA - mean(test$TEMP_MEDIA)))
mase_R1_teste_1 <- mae_R1_teste_1 / mae_mean

# Calculando SMAPE
smape_R1_teste_1 <- mean(2 * abs(predictions_R1_teste_1 - test$TEMP_MEDIA) / (abs(predictions_R1_teste_1) + abs(test$TEMP_MEDIA))) * 100

# Calculando R²
SSE_R1_teste_1 <- sum((test$TEMP_MEDIA - predictions_R1_teste_1)^2)
SST_R1_teste_1 <- sum((test$TEMP_MEDIA - mean(test$TEMP_MEDIA))^2)
r_squared_R1_teste_1 <- 1 - SSE_R1_teste_1 / SST_R1_teste_1

# Criando um data frame com as métricas
metrics_R1_teste_1 <- data.frame(
  Model = "R1_teste",
  RMSE = rmse_R1_teste_1,
  MAE = mae_R1_teste_1,
  MAPE = mape_R1_teste_1,
  MSE = mse_R1_teste_1,
  MASE = mase_R1_teste_1,
  SMAPE = smape_R1_teste_1,
  R_squared = r_squared_R1_teste_1
)

metrics_R1_teste_1


# Comportamento das funções de suavização  para mu

term.plot(R1_teste_1, what="mu",pages = 1, ask = FALSE, ylim = "free")
term.plot(R1_teste_1,what="mu", terms =1, ask = FALSE, ylim = "free", ylabs = expression(log(mu)), xlabs = "Temperatura de Orvalho (ºC)")
term.plot(R1_teste_1,what="mu", terms =2, ask = FALSE, ylim = "free", ylabs = expression(log(mu)), xlabs = "Umidade (%)")
term.plot(R1_teste_1,what="mu", terms =3, ask = FALSE, ylim = "free", ylabs = expression(log(mu)), xlabs = expression("Velocidade do vento (m.s"^{-1} * ")"))
term.plot(R1_teste_1,what="mu", terms =4, ask = FALSE, ylim = "free", ylabs = expression(log(mu)), xlabs = "Precipitação (mm)")
term.plot(R1_teste_1,what="mu", terms =5, ask = FALSE, ylim = "free", ylabs = expression(log(mu)), xlabs = "Pressão Atmosférica (mB)")

# Comportamento das funções de suavização para sigma

term.plot(R1_teste_1, what="sigma",pages = 1, ask = FALSE, ylim = "free")
term.plot(R1_teste_1,what="sigma", terms =1, ask = FALSE, ylim = "free", ylabs = expression(log(sigma)), xlabs = "Temperatura de Orvalho (ºC)")
term.plot(R1_teste_1,what="sigma", terms =2, ask = FALSE, ylim = "free", ylabs = expression(log(sigma)), xlabs = "Pressão Atmosférica (mB)")
term.plot(R1_teste_1,what="sigma", terms =3, ask = FALSE, ylim = "free", ylabs = expression(log(sigma)), xlabs = expression("Velocidade do vento (m.s"^{-1} * ")"))


# Comportamento das funções de suavização para v

term.plot(R1_teste_1, what='nu',pages = 1, ask = FALSE, ylim = "free") 
term.plot(R1_teste_1,what="nu", terms =1, ask = FALSE, ylim = "free", ylabs = expression(nu), xlabs = "Temperatura de Orvalho (ºC)")
term.plot(R1_teste_1,what="nu", terms =2, ask = FALSE, ylim = "free", ylabs = expression(nu), xlabs = "Pressão Atmosférica (mB)")

# Comportamento das funções de suavização para t

term.plot(R1_teste_1, what='tau',pages = 1, ask = FALSE, ylim = "free")
term.plot(R1_teste_1,what="tau", terms =1, ask = FALSE, ylim = "free", ylabs = expression(log(tau)), xlabs = "Temperatura de Orvalho (ºC)")
term.plot(R1_teste_1,what="tau", terms =2, ask = FALSE, ylim = "free", ylabs = expression(log(tau)), xlabs = "Precipitação (mm)")


# Análise de Residuo 

plot(R1_teste_1,ts = TRUE)

# Análise de resíduos - worm plot

wp(R1_teste_1, ylim.all = 1.5) ; title("Worm plot - BCTo")






