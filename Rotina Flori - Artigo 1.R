## Pacotes necessários
library(gamlss)
library(e1071)

## Diretório do R
setwd("<PATH>")

##  Banco de Dados
dados <- read.csv("Dados - Flori.csv", sep = ";" , dec = "," ,header = TRUE)
names(dados)
head(dados)

## Descritiva dos dados
summary(dados)
skewness(Temperatura)
kurtosis(Temperatura)
var(Temperatura)
sd(Temperatura)

## Gráficos descritivos

DDD <- par(mfrow=c(2,2))
plot(Temperatura~Data.Medicao, data = dados, col= 4,pch = 17, cex = 0.5)
plot(Temperatura~Precipitacao.total, 
     data = dados, col= 4,pch = 17, cex = 0.5)
plot(Temperatura~Pressao.Atmosferica, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Temperatura.Orvalho, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Umidade, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Velocidade.Vento, data = dados, col=4,pch = 17, cex = 0.5)
par(DDD)
par(mfrow=c(1,2))
hist(dados$Temperatura,main="Temperatura Florianópolis - 06/2021 - 06/2022",
xlab="Temperatura",prob=TRUE,ylim=c(0,0.12), col = 4 );
lines(density(dados$Temperatura,na.rm=TRUE),col=1)
boxplot(dados$Temperatura,horizontal=FALSE, col = 4, ylab = "Temperatura")
plot(Temperatura, col = 4 )


## ____________________________________________

## Análise do modelo Gamlss

# ______________
# Com suavização 
# ______________

Model_BCPEo <- gamlss(Temperatura~1,data = dados,
method = mixed(20,100), family = BCPEo) # modelo nulo
Model_BCPEo <- stepGAICAll.A(Model_BCPEo, scope = list(lower=~1,
upper = ~ pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica) + pb(Temperatura.Orvalho) + pb(Umidade) + 
pb(Velocidade.Vento)))
summary(Model_BCPEo)
term.plot(Model_BCPEo,pages = 1, ask = FALSE, ylim = "free")



r2 <- gamlss( Temperatura ~ pb(Temperatura.Orvalho) +  
                pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica),  
              sigma.formula = ~pb(Temperatura.Orvalho) + pb(Velocidade.Vento),  
              nu.formula = ~pb(Precipitacao.total) + pb(Data.Medicao),  
              family = BCPEo, data = dados, method = mixed(20,  
                                                           100), trace = FALSE, tau.formula = ~1) # familia BCPEo
summary(r2)

# Comportamento da função de suavização para mu
term.plot(r2, pages=1, ask = FALSE, ylim = "free")

# Comportamento da função de suavização para sigma
term.plot(r2, what="sigma", pages = 1, ask = FALSE, ylim = "free")

# Comportamento da função de suavização para nu
term.plot(r2, what='nu',pages = 1, ask = FALSE, ylim = "free") 

# Comportamento da função de suavização para tau
term.plot(r2, what='tau',pages = 1, ask = FALSE, ylim = "free")
 
# Análise de Residuo 
plot(r2)


# Análise de residuo - worm plot
wp(r2, ylim.all = 1.5) ; title("Worm plot - BCPEo")

#____

# Retirando a suavização das variáveis que apresentaram comportamento linear


r22 <- gamlss(Temperatura ~ Temperatura.Orvalho +  
                pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica),  
              sigma.formula = ~Temperatura.Orvalho + Velocidade.Vento,  
              nu.formula = ~Precipitacao.total + Data.Medicao,  
              family = BCPEo, data = dados, method = mixed(20,  
                                                           100), trace = FALSE, tau.formula = ~1) # familia BCPEo
summary(r22)

# Comportamento da função de suavização para mu
term.plot(r22, pages=1, ask = FALSE, ylim = "free")

# Comportamento da função de suavização para sigma
term.plot(r22, what="sigma", pages = 1, ask = FALSE, ylim = "free")

# Comportamento da função de suavização para nu
term.plot(r22, what='nu',pages = 1, ask = FALSE, ylim = "free")

# Comportamento da função de suavização para tau
term.plot(r22, what='tau',pages = 1, ask = FALSE, ylim = "free")


# Analise de Residuo 
plot(r22)

# Análise de resíduos - worm plot
wp(r22, ylim.all = 1.5) ; title("Worm plot - BCPEo")
