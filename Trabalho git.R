##################### TRABALHO SOBREVIVENCIA #####################

#### PACOTES ####

library(tidyverse)
library(survival)
library(AdequacyModel)
library(flexsurv)

#### DADOS ####

tree <- read.csv("tree_data.csv")

# O banco de dados e composto por 3024 mudas de quatro especies arboreas submetidas 
# a diferentes condicoes de luminosidade e origem do solo. O objetivo e avaliar fatores
# associados a sobrevivencia das mudas ao longo do tempo. Para cada individuo registrou-se
# o tempo de acompanhamento (Time) e a ocorrencia do evento de interesse (Event), 
# definido como a morte da muda. Individuos que permaneceram vivos ate o final do 
# estudo ou foram removidos para medicoes laboratoriais foram tratados como observacoes
# censuradas.

#### ANALISE DESCRITIVA E EXPLORATORIA ####

### FUNCOES E GRAFICOS DE SOBREVIVENCIA E RISCO DOS DADOS SEM CONSIDERAR COVARIAVEIS ###

## KAPLER MEIER ##

km <- survfit(Surv(Time, Event) ~ 1, data = tree, conf.int = F); summary(km)
plot(km, conf.int = F)

## FUNCAO DE RISCO ACUMULADA ##

sobrevivencia_km <- km$surv
H_km <- -log(sobrevivencia_km)
plot(H_km, type = "s")

## GRAFICO TTT ##

TTT(tree$Time, col = "red", lwd = 2.5, grid = TRUE, lty = 2)

### FUNCOES E GRAFICOS DE SOBREVIVENCIA E RISCO DAS COVARIAVEIS SEPARADAMENTE ###

## PLOT ##

km_plot <- survfit(Surv(Time, Event) ~ Plot, data = tree, conf.int = F)
summary(km_plot)
teste_plot <- survdiff(Surv(Time, Event) ~ Plot, data = tree);teste_plot
# Aqui o grafico e dificil de ver, ja que sao 18 categorias. Porem, pelo teste de
# comparacao de curvas, existe diferenca entre as categorias.

## SUBPLOT ##

km_subplot <- survfit(Surv(Time, Event) ~ Subplot, data = tree, conf.int = F)
summary(km_subplot)
teste_subplot <- survdiff(Surv(Time, Event) ~ Subplot, rho = 1, data = tree);teste_subplot
plot(km_subplot, conf.int = F, col = rainbow(5), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Subplot)),
  col = rainbow(5),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor de 0.8) e do  (pelo teste de wilcoxom p-valor=0.6)
# grafico de sobrevivencia, as diferentes categorias desta variavel nao diferem,
# portanto nao devem ser consideradas para analises futuras

## ESPECIE ##

km_especie <- survfit(Surv(Time, Event) ~ Species, data = tree, conf.int = F)
summary(km_especie)
teste_especie <- survdiff(Surv(Time, Event) ~ Species,rho = 1, data = tree); teste_especie
plot(km_especie, conf.int = F, col = rainbow(4), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Species)),
  col = rainbow(4),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.
# Uma coisa interessante e que existe "duplas" no grafico, mostrando que duas duplas
# de categorias tem comportamentos muito parecidos

## LUZ ##

km_luz <- survfit(Surv(Time, Event) ~ Light_Cat, data = tree, conf.int = F)
summary(km_luz)
teste_luz <- survdiff(Surv(Time, Event) ~ Light_Cat,rho = 1, data = tree); teste_luz
plot(km_luz, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Light_Cat)),
  col = rainbow(3),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor = 0.1) e do (pelo teste de wilcoxom p-valor=0.2)
# grafico de sobrevivencia, existe evidencia, apesar de pouca, de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## CORE ##

km_core <- survfit(Surv(Time, Event) ~ Core, data = tree, conf.int = F)
summary(km_core)
teste_core <- survdiff(Surv(Time, Event) ~ Core, rho = 1, data = tree); teste_core
plot(km_core, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Core)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do (pelo teste de wilcoxom p-valor=0.06)
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## SOLO ##

km_solo <- survfit(Surv(Time, Event) ~ Soil, data = tree, conf.int = F)
summary(km_solo)
teste_solo <- survdiff(Surv(Time, Event) ~ Soil, rho = 1, data = tree); teste_solo
plot(km_solo, conf.int = F, col = rainbow(7), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Soil)),
  col = rainbow(7),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## ADULT ??? ##

## ESTERELIZACAO ##

km_este <- survfit(Surv(Time, Event) ~ Sterile, data = tree, conf.int = F)
summary(km_este)
teste_este <- survdiff(Surv(Time, Event) ~ Sterile, rho = 1, data = tree); teste_este
plot(km_este, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Sterile)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## CONSPECIFIC ##

km_conspecific <- survfit(Surv(Time, Event) ~ Conspecific, data = tree, conf.int = F)
summary(km_conspecific)
teste_conspecific <- survdiff(Surv(Time, Event) ~ Conspecific, rho = 1, data = tree); teste_conspecific
plot(km_conspecific, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Conspecific)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

## MYCO ##

km_myco <- survfit(Surv(Time, Event) ~ Myco, data = tree, conf.int = F)
summary(km_myco)
teste_myco <- survdiff(Surv(Time, Event) ~ Myco , rho = 0, data = tree); teste_myco
plot(km_myco, conf.int = F, col = rainbow(2), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$Myco)),
  col = rainbow(2),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que as categorias desta
# variavel diferem, portanto deve ser considerada para analises futuras.

## SOILMYCO ##

km_soilmyco <- survfit(Surv(Time, Event) ~ SoilMyco, data = tree, conf.int = F)
summary(km_soilmyco)
teste_soilmyco <- survdiff(Surv(Time, Event) ~ SoilMyco, rho = 1, data = tree); teste_soilmyco
plot(km_soilmyco, conf.int = F, col = rainbow(3), lty = 2)
legend(
  "bottomleft",
  legend = levels(as.factor(tree$SoilMyco)),
  col = rainbow(3),
  lty = 2,
  bty = "n",
  cex = 0.8
)
# Pode-se perceber que, pelo teste de comparacao de curvas (p-valor < 0.05) e do 
# grafico de sobrevivencia, existe evidencia de que ao menos uma categoria desta
# variavel difere das demais, portanto deve ser considerada para analises futuras.

### CONCLUSAO DA ANALISE DESCRITIVA E EXPLORATORIA ###

# A análise descritiva e exploratória dos dados permitiu caracterizar o comportamento
# inicial da sobrevivência das mudas e identificar possíveis fatores associados ao 
# tempo até a ocorrência do evento de interesse. A inspeção dos gráficos TTT e da 
# função de risco acumulado indicou um comportamento compatível com uma taxa de falha
# crescente ao longo do tempo, sugerindo que distribuições paramétricas capazes de 
# acomodar esse padrão constituem alternativas adequadas para a modelagem dos dados. 
# Dessa forma, nas etapas seguintes serão avaliados modelos assumindo a distribuição
# de Gama Generalizada, visando identificar aquela que melhor descreve o processo de 
# sobrevivência observado. Além disso, a variável Subplot não apresentou evidências
# de associação com a sobrevivência durante a análise exploratória, sendo, portanto,
# desconsiderada nas etapas subsequentes de modelagem, de modo a favorecer um modelo
# mais parcimonioso sem perda significativa de informação.

#### MODELAGEM ####

tree$Plot <- as.factor(tree$Plot)
tree$Species <- as.factor(tree$Species)
tree$Light_Cat <- as.factor(tree$Light_Cat)
tree$Core <- as.factor(tree$Core)
tree$Soil <- as.factor(tree$Soil)
tree$Sterile <- as.factor(tree$Sterile)
tree$Conspecific <- as.factor(tree$Conspecific)
tree$Myco <- as.factor(tree$Myco)
tree$SoilMyco <- as.factor(tree$SoilMyco)

## SURVREG ##

modelo_weibull <- survreg(Surv(Time, Event) ~ 1,
                          data = tree,
                          dist = "weibull")

modelo_logweibull <- survreg(Surv(log(Time), Event) ~ 1,
                             data = tree,
                             dist = "extreme")

modelo_exp <- survreg(Surv(Time, Event) ~ 1,
                      data = tree,
                      dist = "exponential")

modelo_lognormal <- survreg(Surv(Time, Event) ~ 1,
                            data = tree,
                            dist = "lognormal")

modelo_loglogi <- survreg(Surv(Time, Event) ~ 1,
                          data = tree,
                          dist = "loglogistic")

## FLEXSURV ##

modelo_gamma <- flexsurvreg(Surv(Time, Event) ~ 1,
                            data = tree,
                            dist = "gamma")

modelo_gengamma <- flexsurvreg(Surv(Time, Event) ~ 1,
                               data = tree,
                               dist = "gengamma")

## METODO GRAFICO ##

# TEMPO #

tempo <- seq(0, max(tree$Time), by = 0.1)

# EXPONENCIAL #

alphaE <- exp(coef(modelo_exp))

sE <- exp(-tempo/alphaE)

# WEIBULL #

gammaW <- 1/modelo_weibull$scale
alphaW <- exp(coef(modelo_weibull))

sW <- exp(-(tempo/alphaW)^gammaW)

# LOG NORMAL #

muLN <- coef(modelo_lognormal)
sigmaLN <- modelo_lognormal$scale

sLN <- 1 - pnorm((log(tempo) - muLN)/sigmaLN)
sLN[1] <- 1

# LOG LOGISTIC #

gammaLL <- 1/modelo_loglogi$scale
alphaLL <- exp(coef(modelo_loglogi))

sLL <- 1/(1 + (tempo/alphaLL)^gammaLL)

# GAMMA #

sGamma <- summary(modelo_gamma,
                  t = tempo,
                  type = "survival")[[1]]$est

# GAMA GENERALIZADA #

sGG <- summary(modelo_gengamma,
               t = tempo,
               type = "survival")[[1]]$est

# GRAFICO DE COMPARACAO #

plot(km,
     conf.int = FALSE,
     xlab = "Tempo",
     ylab = "S(t)",
     col = "black",
     lwd = 2)

lines(tempo, sE, col = "orange", lwd = 2)
lines(tempo, sW, col = "blue", lwd = 2)
lines(tempo, sLN, col = "darkgreen", lwd = 2)
lines(tempo, sLL, col = "red", lwd = 2)
lines(tempo, sGamma, col = "purple", lwd = 2)
lines(tempo, sGG, col = "brown", lwd = 2)

legend("bottomleft",
       legend = c("Kaplan-Meier",
                  "Exponencial",
                  "Weibull",
                  "Log-normal",
                  "Log-Logística",
                  "Gamma",
                  "Gamma Generalizada"),
       col = c("black",
               "orange",
               "blue",
               "darkgreen",
               "red",
               "purple",
               "brown"),
       lwd = 2,
       cex = 0.7)

## AIC, BIC, AICc ##

# AIC e BIC #

tab_criterios <- data.frame(
  Modelo = c("Exponencial",
             "Weibull",
             "Log-normal",
             "Log-logística",
             "Gamma",
             "Gamma Generalizada"),
  
  AIC = c(AIC(modelo_exp),
          AIC(modelo_weibull),
          AIC(modelo_lognormal),
          AIC(modelo_loglogi),
          modelo_gamma$AIC,
          modelo_gengamma$AIC),
  
  BIC = c(BIC(modelo_exp),
          BIC(modelo_weibull),
          BIC(modelo_lognormal),
          BIC(modelo_loglogi),
          modelo_gamma$BIC,
          modelo_gengamma$BIC)
)

tab_criterios

# AICc #

# definindo n e k

n <- nrow(tree)

k_exp <- length(coef(modelo_exp)) + 1

k_weibull <- length(coef(modelo_weibull)) + 1

k_lognormal <- length(coef(modelo_lognormal)) + 1

k_loglogi <- length(coef(modelo_loglogi)) + 1

k_gamma <- length(modelo_gamma$res.t)

k_gengamma <- length(modelo_gengamma$res.t)

AICc <- function(modelo, k, n){
  aic <- AIC(modelo)
  aic + (2*k*(k+1))/(n-k-1)
}

tab_criterios$AICc <- c(
  AICc(modelo_exp, k_exp, n),
  AICc(modelo_weibull, k_weibull, n),
  AICc(modelo_lognormal, k_lognormal, n),
  AICc(modelo_loglogi, k_loglogi, n),
  AICc(modelo_gamma, k_gamma, n),
  AICc(modelo_gengamma, k_gengamma, n)
)

tab_criterios

### SELECAO DE COVARIAVEIS ###

modelo_gengamma

## PARA CADA COVARIAVEL ##

teste_variavel <- function(data, variavel, dist = "gengamma") {
  
  # Modelo nulo
  form0 <- as.formula("Surv(Time, Event) ~ 1")
  
  # Modelo com a variavel
  form1 <- as.formula(paste("Surv(Time, Event) ~", variavel))
  
  modelo0 <- flexsurvreg(form0,
                         data = data,
                         dist = dist)
  
  modelo1 <- flexsurvreg(form1,
                         data = data,
                         dist = dist)
  
  LR <- 2 * (modelo1$loglik - modelo0$loglik)
  
  gl <- modelo1$npars - modelo0$npars
  
  p <- pchisq(LR, df = gl, lower.tail = FALSE)
  
  data.frame(
    Variavel = variavel,
    Distribuicao = dist,
    LogLik_Nulo = modelo0$loglik,
    LogLik_Modelo = modelo1$loglik,
    TRV = LR,
    gl = gl,
    p.valor = p,
    AIC_Nulo = modelo0$AIC,
    AIC_Modelo = modelo1$AIC
  )
}

# PLOT #

modelo_gengamma_plot <- flexsurvreg(Surv(Time, Event) ~ Plot, data = tree, dist = "gengamma")
modelo_gengamma_plot
teste_variavel(tree, "Plot", "gengamma")

# ESPECIE #

modelo_gengamma_especie <- flexsurvreg(Surv(Time, Event) ~ Species, data = tree, dist = "gengamma")
modelo_gengamma_especie
teste_variavel(tree, "Species", "gengamma")

# LUZ #

modelo_gengamma_luz <- flexsurvreg(Surv(Time, Event) ~ Light_Cat, data = tree, dist = "gengamma")
modelo_gengamma_luz
teste_variavel(tree, "Light_Cat", "gengamma")

# CORE #

modelo_gengamma_core <- flexsurvreg(Surv(Time, Event) ~ Core, data = tree, dist = "gengamma")
modelo_gengamma_core
teste_variavel(tree, "Core", "gengamma")

# SOLO #

modelo_gengamma_solo <- flexsurvreg(Surv(Time, Event) ~ Soil, data = tree, dist = "gengamma")
modelo_gengamma_solo
teste_variavel(tree, "Soil", "gengamma")

# ESTERELIZACAO #

modelo_gengamma_este <- flexsurvreg(Surv(Time, Event) ~ Sterile, data = tree, dist = "gengamma")
modelo_gengamma_este
teste_variavel(tree, "Sterile", "gengamma")

# MYCO #

modelo_gengamma_myco <- flexsurvreg(Surv(Time, Event) ~ Myco, data = tree, dist = "gengamma")
modelo_gengamma_myco
teste_variavel(tree, "Myco", "gengamma")

# SOILMYCO #

modelo_gengamma_soilmyco <- flexsurvreg(Surv(Time, Event) ~ SoilMyco, data = tree, dist = "gengamma")
modelo_gengamma_soilmyco
teste_variavel(tree, "SoilMyco", "gengamma")

## RETIRANDO UMA POR VEZ ##

# DETALHE IMPORTANTE #

table(tree$Species, tree$Myco)

table(tree$Soil, tree$SoilMyco)

table(tree$Sterile, tree$Conspecific)

xtabs(~ Species + Soil + Conspecific, data = tree)

# Ao fazer essa tabela acima fica claro que algumas variáveis são dependentes de
# outras, portanto elas não serão consideradas no modelo.

tree <- tree |>
  dplyr::filter(
    !is.na(Time),
    !is.na(Event),
    !is.na(Species),
    !is.na(Soil)
  )

modelo_completo <- survreg(
  Surv(Time, Event) ~ Species + Soil + Light_Cat,
  data = tree,
  dist = "loglogistic"
)

modelo_completo

modelo_sem_species <- flexsurvreg(
  Surv(Time, Event) ~ Soil,
  data = tree,
  dist = "gengamma"
)

modelo_sem_soil <- flexsurvreg(
  Surv(Time, Event) ~ Species,
  data = tree,
  dist = "gengamma"
)

# TESTE TRV #

teste_TRV <- function(modelo_reduzido, modelo_completo){
  
  LR <- 2 * (modelo_completo$loglik - modelo_reduzido$loglik)
  
  gl <- modelo_completo$npars - modelo_reduzido$npars
  
  p <- pchisq(LR, gl, lower.tail = FALSE)
  
  data.frame(
    TRV = LR,
    gl = gl,
    p.valor = p
  )
}

teste_TRV(modelo_sem_species, modelo_completo)

teste_TRV(modelo_sem_soil, modelo_completo)

# o modelo completo é o melhor.

## INTERPRETACAO DE COEFICIENTES ##

# O modelo paramétrico Gama Generalizada indicou que tanto a espécie da muda quanto
# a origem do solo influenciaram significativamente o tempo de sobrevivência. 
# Considerando Acer saccharum como categoria de referência, observou-se que as 
# espécies Quercus alba e Quercus rubra apresentaram maiores tempos de 
# sobrevivência, enquanto Prunus serotina não diferiu significativamente da 
# espécie de referência. Em relação à origem do solo, tomando Acer rubrum como 
# referência, verificou-se redução do tempo de sobrevivência para mudas cultivadas
# em solos provenientes de Prunus serotina e Quercus alba. Os demais tipos de solo
# não apresentaram diferenças estatisticamente significativas.

## COX SNELL ##

predlin <- modelo_completo$linear.predictors
sigma <- modelo_completo$scale

S_hat <- 1 / (1 + exp((log(tree$Time) - predlin)/sigma))

EI <- -log(S_hat)

kmEI <- survfit(Surv(EI, tree$Event) ~ 1, conf.int = FALSE)

tempoEI <- kmEI$time
sEI <- kmEI$surv
sExp <- exp(-tempoEI)

plot(kmEI,
     conf.int = FALSE,
     xlab = "Resíduos de Cox-Snell",
     ylab = "Sobrevivência")

lines(tempoEI, sExp,
      col = 2,
      lty = 2,
      lwd = 2)

AIC(modelo_completo)

