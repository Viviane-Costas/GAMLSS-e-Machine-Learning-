library(gamlss)


setwd("D:\\UNIDADE D\\Vivi\\Mestrado\\Dissertação\\Banco de dados\\INMET\\Florianopolis")
dados <- read.csv("Dados - Flori.csv", sep = ";" , dec = "," ,header = TRUE)
names(dados)
head(dados)
attach(dados)
summary(dados)

## Descritiva 

DDD <- par(mfrow=c(2,2))
plot(Temperatura~Data.Medicao, data = dados, col= 4,pch = 17, cex = 0.5)
plot(Temperatura~Precipitacao.total, data = dados, col= 4,pch = 17, cex = 0.5)
plot(Temperatura~Pressao.Atmosferica, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Temperatura.Orvalho, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Umidade, data = dados, col=4,pch = 17, cex = 0.5)
plot(Temperatura~Velocidade.Vento, data = dados, col=4,pch = 17, cex = 0.5)
par(DDD)

par(mfrow=c(1,2))
hist(dados$Temperatura,main="Temperatura Florianópolis - 06/2021 - 06/2022",xlab="Temperatura",prob=TRUE,ylim=c(0,0.12), col = 4 );
lines(density(dados$Temperatura,na.rm=TRUE),col=1)
boxplot(dados$Temperatura,horizontal=FALSE, col = 4, ylab = "Temperatura")
plot(Temperatura, col = 4 )
library(e1071)
skewness(Temperatura)
kurtosis(Temperatura)
var(Temperatura)
sd(Temperatura)
## ________________________________________________________________________

## An?lise dos modelos Gamlss

# ______________
# Com suaviza??o 
# ______________

Model_BCTo <- gamlss(Temperatura~1,data = dados, method = mixed(20,100), family = BCTo) # modelo nulo
Model_BCTo <- stepGAICAll.A(Model_BCTo, scope = list(lower=~1,
                                                     upper = ~ pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica) +
                                                       pb(Temperatura.Orvalho) +  pb(Umidade) +  pb(Velocidade.Vento)))
summary(Model_BCTo)
term.plot(Model_BCTo,pages =1, ask=FALSE, ylim = "free")


Model_BCPEo <- gamlss(Temperatura~1,data = dados, method = mixed(20,100), family = BCPEo) # modelo nulo
Model_BCPEo <- stepGAICAll.A(Model_BCPEo, scope = list(lower=~1,
                                                       upper = ~ pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica) +
                                                         pb(Temperatura.Orvalho) +  pb(Umidade) +  pb(Velocidade.Vento)))
summary(Model_BCPEo)
term.plot(Model_BCPEo,pages = 1, ask = FALSE, ylim = "free")

Model_BCCGo <- gamlss(Temperatura~1,data =dados, method = mixed(20,100), family = BCCGo) # modelo nulo
Model_BCCGo <- stepGAICAll.A(Model_BCCGo, scope = list(lower=~1,
                                                 upper = ~ pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica) +
                                                   pb(Temperatura.Orvalho) +  pb(Umidade) +  pb(Velocidade.Vento)))
summary(Model_BCCGo)
term.plot(Model_BCCGo,pages = 1,ask = FALSE, ylim = "free")



r1 <- gamlss(Temperatura ~ pb(Temperatura.Orvalho) +  
               pb(Umidade) + pb(Data.Medicao) + pb(Pressao.Atmosferica) +  
               pb(Velocidade.Vento) + pb(Precipitacao.total),  
             sigma.formula = ~pb(Temperatura.Orvalho) + pb(Data.Medicao) +  
               pb(Precipitacao.total), family = BCTo, data = dados,  
             method = mixed(20, 100), trace = FALSE, nu.formula = ~1,  
             tau.formula = ~1) # familia BCTo
summary(r1)

r2 <- gamlss( Temperatura ~ pb(Temperatura.Orvalho) +  
                pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica),  
              sigma.formula = ~pb(Temperatura.Orvalho) + pb(Velocidade.Vento),  
              nu.formula = ~pb(Precipitacao.total) + pb(Data.Medicao),  
              family = BCPEo, data = dados, method = mixed(20,  
                                                           100), trace = FALSE, tau.formula = ~1) # familia BCPEo
summary(r2)

r3 <- gamlss(Temperatura ~ pb(Temperatura.Orvalho) + pb(Umidade) +  
               pb(Data.Medicao) + pb(Precipitacao.total) + pb(Velocidade.Vento) +  
               pb(Pressao.Atmosferica), sigma.formula = ~pb(Temperatura.Orvalho) +  
               pb(Pressao.Atmosferica) + pb(Data.Medicao), nu.formula = ~pb(Pressao.Atmosferica) +  
               pb(Precipitacao.total), family = BCCGo, data = dados, method = mixed(20,  
                                                                                    100), trace = FALSE) # familia BCCGo

summary(r3)

# Compara??o entre os modelos ajustados

AIC(r1,r2,r3)


# Comportamento das fun??es de suaviza??o para ?

term.plot(r1, pages =1, ask = FALSE, ylim = "free")
term.plot(r2, pages=1, ask = FALSE, ylim = "free")
term.plot(r3, pages=1, ask = FALSE, ylim = "free")
edfAll(r3)

term.plot(r1, terms = 1, ask = FALSE) 

# Comportamento das fun??es de suaviza??o para sigma

op <- par(mfrow= c(1,2))

term.plot(r1, what="sigma",pages = 1, ask = FALSE, ylim = "free")
term.plot(r2, what="sigma", pages = 1, ask = FALSE, ylim = "free")
term.plot(r3, what="sigma", pages = 1, ask = FALSE, ylim = "free")

par(op)

# Comportamento das fun??es de suaviza??o para v

term.plot(r1, what='nu',pages = 1, ask = FALSE, ylim = "free") 
term.plot(r2, what='nu',pages = 1, ask = FALSE, ylim = "free") 
term.plot(r3, what='nu',pages = 1, ask = FALSE, ylim = "free") 

# Comportamento das fun??es de suaviza??o para t

term.plot(r1, what='tau',pages = 1, ask = FALSE, ylim = "free")
term.plot(r2, what='tau',pages = 1, ask = FALSE, ylim = "free")
term.plot(r3, what='tau',pages = 1, ask = FALSE, ylim = "free") 


# An?lise de Residuo 

plot(r1,ts = TRUE)
plot(r2)
plot(r3)

# An?lise de res?duos - worm plot

op <- par(mfrow = c(1,2))
wp(r1, ylim.all = 1.5) ; title("Worm plot - BCTo")
wp(r2, ylim.all = 1.5) ; title("Worm plot - BCPEo")
wp(r3, ylim.all = 1.5) ; title("Worm plot - BCCGo")
par(op)


# Compara??o entre os modelos ajustados

AIC(r1,r2,r3)
AIC(r2)

# stepGAICAll.A(m1, lower=~1, upper=~;;;;;;;, parallel = 'snow', ncpus=2L) pra rodar mais rapido

# ________________________________________________________________________________

# Retirando a suaviza??o dos par?metros que apresentaram comportamento linear


#r11 <- gamlss(Temperatura ~ Temperatura.Orvalho +  
#                Umidade + pb(Data.Medicao) + pb(Press?o.Atmosferica) +  
#                Velocidade.Vento + pb(Precipita??o.total),  
#              sigma.formula = ~pb(Temperatura.Orvalho) + pb(Data.Medicao) +  
#                pb(Precipita??o.total), family = BCTo, data = dados,  
#              method = mixed(20, 100), trace = FALSE, nu.formula = ~1,  
#              tau.formula = ~1) # familia BCTo
#summary(r11)

r22 <- gamlss(Temperatura ~ Temperatura.Orvalho +  
                pb(Data.Medicao) + pb(Precipitacao.total) + pb(Pressao.Atmosferica),  
              sigma.formula = ~Temperatura.Orvalho + Velocidade.Vento,  
              nu.formula = ~Precipitacao.total + Data.Medicao,  
              family = BCPEo, data = dados, method = mixed(20,  
                                                           100), trace = FALSE, tau.formula = ~1) # familia BCPEo
summary(r22)

#r33 <- gamlss(Temperatura ~ Temperatura.Orvalho + Umidade +  
#                pb(Data.Medicao) + pb(Precipita??o.total) + pb(Velocidade.Vento) +  
#                pb(Press?o.Atmosferica), sigma.formula = ~pb(Temperatura.Orvalho) +  
#                Press?o.Atmosferica + pb(Data.Medicao), nu.formula = ~Press?o.Atmosferica +  
#                Precipita??o.total, family = BCCGo, data = dados, method = mixed(200,100)) # familia BCCGo

#summary(r33)


# Compara??o entre os modelos ajustados

#AIC(r11,r22,r33)
AIC(r22)

# Comportamento das fun??es de suaviza??o para ?

#term.plot(r11, pages =1, ask = FALSE, ylim = "free")
term.plot(r22, pages=1, ask = FALSE, ylim = "free")
#term.plot(r33, pages=1, ask = FALSE, ylim = "free")

# Comportamento das fun??es de suaviza??o para sigma

#term.plot(r11, what="sigma",pages = 1, ask = FALSE, ylim = "free")
term.plot(r22, what="sigma", pages = 1, ask = FALSE, ylim = "free")
#term.plot(r33, what="sigma", pages = 1, ask = FALSE, ylim = "free")



# Comportamento das fun??es de suaviza??o para v

#term.plot(r11, what='nu',pages = 1, ask = FALSE, ylim = "free") # modelo nulo no modelo
term.plot(r22, what='nu',pages = 1, ask = FALSE, ylim = "free") # modelo nulo no modelo
#term.plot(r33, what='nu',pages = 1, ask = FALSE, ylim = "free") # modelo nulo no modelo

# Comportamento das fun??es de suaviza??o para t

#term.plot(r11, what='tau',pages = 1, ask = FALSE, ylim = "free")
term.plot(r22, what='tau',pages = 1, ask = FALSE, ylim = "free")




# An?lise de Residuo 

#plot(r11)
plot(r22)
#plot(r33)

# An?lise de res?duos - worm plot

#op <- par(mfrow = c(1,2))
#wp(r11, ylim.all = 1.5) ; title("Worm plot - BCTo")
wp(r22, ylim.all = 1.5) ; title("Worm plot - BCPEo")
#wp(r33, ylim.all = 1.5) ; title("Worm plot - BCCGo")
#par(op)


# Compara??o entre os modelos ajustados

AIC(r11,r22,r33)



