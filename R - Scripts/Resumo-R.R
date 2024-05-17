##############################################################################
########################## Resumo dos Comandos no R ##########################
##############################################################################

##############################################################################
########################## Resumo R:  Segundo Teste ##########################
##############################################################################

# Criar um array / vetor com dados
dadosTeste <- c("dado1", "dado2")

# Criar repetições de um dado
dadosTeste <- c(rep("dado1-15x", 15), "dado2-1x", rep("dado3-5x", 5))

# Array de Testes
dados <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Média
(mean(dados))

# Desvio Padrão
(sd(dados))

# Variância
(var(dados))

# Dimensão do Array
(length(dados))

##############################################################################
##############################################################################

# TESTES DE HIPÓTESES:

## Médias (Uma Média ou Diferença de Médias):
### z.test() (library(BSDA))
### t.test()

## Variâncias:
### Uma Variância: varTest() (library(EnvStats))
### Quociente de Variâncias: var.test()

## Proporções (Uma Proporção ou Diferença de Proporções):
### z.test() (library(BSDA))

##############################################################################
##############################################################################

dmvi1 <- c(36, 34, 34, 36, 35, 34, 33, 35, 33, 35)
dmvi2 <- c(26, 37, 28, 26, 34, 28, 32, 27, 23, 28, 26, 36, 33, 30, 31, 27)

##############################################################################
##############################################################################

# Função z.test => Distribuição Normal (0, 1)
z.test(
  x = dados,         # Vetor com a amostra
  sigma.x = 0.08,    # Desvio Padrão da População
  conf.level = 0.90  # Grau de Confiança para o teste/intervalo
)

##############################################################################
##############################################################################

# Intervalo de Confiança: Diferença de Médias - Variáveis Independentes - Com:

# Populações Normais;
# desvios-Padrões Desconhecidos mas Iguais;
# Amostras Independentes;
# n1 e n2 < 30

# Função t.test
t.test(
  x = dmvi1,          # Primeira Amostra
  y = dmvi2,          # Segunda Amostra
  paired = FALSE,     # As Amostras são Dependentes?
  var.equal = TRUE,   # As Variâncias são Iguais?
  conf.level = 0.95   # Grau de Confiança
)

##############################################################################
##############################################################################

# Teste Para o Quociente de Duas Variâncias
var.test(
  x = dmvi1,          # Amostra 1
  y = dmvi2,          # Amostra 2
  conf.level = 0.95   # Grau de COnfiança
)

##############################################################################
##############################################################################

# Populações Normais;
# desvios-Padrões Desconhecidos e Diferentes;
# Amostras Independentes;
# n1 e n2 < 30

# Função t.test
t.test(
  x = dmvi1,          # Primeira Amostra
  y = dmvi2,          # Segunda Amostra
  paired = FALSE,     # As Amostras são Dependentes?
  var.equal = FALSE,  # As Variâncias são Iguais?
  conf.level = 0.95   # Grau de Confiança
)

##############################################################################
##############################################################################

# Teste para a Variância - Intervalo de Confiança para a Variância
library(EnvStats)
varTest(
  x = dmvi1,          # Amostra
  conf.level = 0.90   # Grau de Confiança
)

##############################################################################
##############################################################################

# Teste Para a Diferença de Médias Com:

# Populações Quaisquer;
# σ1 e σ2 Desconhecidos;
# Amostras Independentes;
# n1 = 50 ≥ 30 e n2 = 50 ≥ 30.
library(BSDA)
z.test(
  x = dmvi1,
  sigma.x = sd(dmvi1),
  y = dmvi2,
  sigma.y = sd(dmvi2),
  conf.level = 0.90
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico
library(BSDA)
z.test(
  x = dmvi1,            # Amostra
  sigma.x = 20,         # Desvio Padrão Populacional
  alternative = "less", # Tipo de Teste (Unilateral Esquerdo) (H1: p < ...)
  mu = 60               # Média Populacional
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico
library(BSDA)
z.test(
  x = dmvi1,                     # Amostra
  sigma.x = sqrt(0.01*(1-0.01)), # Desvio Padrão Populacional
  alternative = "greater",       # Tipo de Teste (Unilateral Direito) (H1:
  mu = 0.01                      # Média Populacional                  p > ...)
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico
library(BSDA)
z.test(
  x = dmvi1,                     # Amostra
  sigma.x = sqrt(0.01*(1-0.01)), # Desvio Padrão Populacional
  alternative = "two.sided",     # Tipo de Teste (Bilateral) (H1: p != ...)
  mu = 0.01                      # Média Populacional
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico
# Diferença de Médias;
# Amostras Aleatórias Independentes.

# H0: µ1 - µ2 = 0 => Não há diferenças.
# vs.
# H1: µ1 - µ2 != 0 Há Diferenças.

library(BSDA)
z.test(
  x = dmvi1,                  # Amostra 1
  sigma.x = sd(dmvi1),        # Desvio Padrão da Amostra 1
  y = dmvi2,                  # Amostra 2
  sigma.y = sd(dmvi2),        # Desvio Padrão da Amostra 2
  alternative = "two.sided",  # Tipo de Teste (Bilateral)
  mu = 0                      # Média Resultante
)

##############################################################################
##############################################################################

# Teste de Ajustamento à Normalidade
shapiro.test(
  dmvi1  # Amostra
)

##############################################################################
##############################################################################

# Verificar se as Variâncias Podem Ser Consideradas Iguais:
## Intervalo de Confiança
var.test(
  x = dmvi1,
  y = dmvi2,
  conf.level = 0.95
)

## Teste de Hipóteses
var.test(
  x = dmvi1,
  y = dmvi2,
  alternative = "two.sided",
  ratio = 1
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico para a Diferença de Médias
t.test(
  x = dmvi1,
  y = dmvi2,
  alternative = "greater",
  mu = 0,
  paired = FALSE,
  var.equal = TRUE
)

##############################################################################
##############################################################################

# Teste de Hipóteses Paramétrico para a Variância
library(EnvStats)
varTest(
  x = dmvi1,
  alternative = "less",
  sigma.squared = 16
)

##############################################################################
##############################################################################

# Testes de Ajustamento do Qui-Quadrado:

# Função -> chisq.test()
# Argumentos da Função: Oi e pi

# Os Resultados Só Estão Todos Certos Se r = 0

#######################################################
#######################################################
#######################################################

## Exemplo - Distribuição Uniforme Discreta

# H0:X Segue uma distribuição Uniforme discreta
# vs.
# H1:X Não segue uma distribuição Uniforme discreta

# Domínio
(xi<- c(1,2,3,4,5,6))

# Nº de linhas da tabela de frequências
(k<-length(xi))

# Frequências Observadas
(Oi=c(46,35,25,19,40,45))

# Dimensão da Amostra
(n <- sum(Oi))

# Probabilidades Necessárias para as Frequências Esperadas
(pi =rep(1/6,k))

r <- 0 # Não foi necessário estimar parâmetros

(gl <- k-1-r)  # Graus de liberdade da distribuião Qui-Quadrado

# Teste de Ajustamento do Qui-Quadrado
chisq.test(x = Oi, p = pi)

chisq1 <- chisq.test(x = Oi, p = pi)
chisq1$statistic # Qobs
chisq1$parameter # graus de liberdade
chisq1$p.value   # valor-p
chisq1$observed  # Oi
chisq1$expected  # Ei=npi

# Verificação das Regras Recomendadas

# Dimensão da Amostra
if(n > 30){
  print("Respeita a Regra")
}else{
  print("A Amostra é Pequena")
}

# Todas as Frequências Esperadas >= 1
if(length(which(chisq1$expected < 1)) > 0){
  print("Juntar Linhas da Tabela de Frequências")
}else{
  print("Respeita a Regra")
}

# Não Há Mais de 20% das Frequências Esperadas < 5
if(length(which(exemplo.1$expected < 5)) > (k * 0.2)){
  print("Juntar Linhas da Tabela de Frequências")
}else{
  print("Respeita a Regra")
}

# Cálculo do Quantil de Probabilidade da Região critíca
(qchisq(0.95, gl))

# Cálculo do Valor-P
(1 - pchisq(exemplo.1$statistic, gl))

#######################################################
#######################################################
#######################################################

## Exemplo - Distribuição de Poisson, com Média 2.1

# H0: X Segue uma distribuição de Poisson de média 2.1
# vs.
# H1: X Não segue uma distribuição de Poisson  de média 2.1


# Domínio -> atenção que o 5 representa maior ou igual 5
# (devido ao domínio da Poisson)
(xi<- c(0,1,2,3,4,5))

# Número de linhas da tabela de frequências
(k<-length(xi))

# frequências observadas
(Oi=c(14,22,18,15,10,9))

# dimensão da amostra
(n <- sum(Oi))

r <- 0 # não foi necessário estimar parâmetros
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =dpois(xi,2.1))
pi[k] <- 1-ppois(xi[k-1],2.1)  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
chisq2 <- chisq.test(x=Oi,p=pi)

chisq2$statistic # Qobs
chisq2$parameter # graus de liberdade -> errado
chisq2$p.value   # valor-p -> errado
chisq2$observed  # Oi
chisq2$expected  # Ei=npi

# verificar as regras recomendadas

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.2a$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.2a$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# cálculo do quantil de probabilidade da Região critíca
qchisq(0.99, gl)

# cálculo do valor-p
1-pchisq(exemplo.2a$statistic, gl)

#######################################################
#######################################################
#######################################################

## Exemplo - Distribuição de Poisson

# H0: X Segue uma distribuição de Poisson
# vs.
# H1: X Não segue uma distribuição de Poisson


# Domínio -> atenção que o 5 representa maior ou igual 5
# (devido ao domínio da Poisson)
(xi<- c(0,1,2,3,4,5))

# número de linhas da tabela de frequências
(k<-length(xi))

# frequências observadas
(Oi=c(14,22,18,15,10,9))

# dimensão da amostra
(n <- sum(Oi))

# é necessário estimar o lambda -> média amostral
(elx <- sum(xi*Oi)/n)
#ou
amostra <- c(rep(0,14), rep(1,22), rep(2,18), rep(3,15), rep(4,10), rep(5,9))
(elx <- mean(amostra))


r <- 1  # estimou-se um parâmetro
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =dpois(xi,elx))
pi[k] <- 1-ppois(xi[k-1],elx)  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

# teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

# dar um nome para poder ver os diversos campos
chisq3 <- chisq.test(x=Oi,p=pi)

chisq3$statistic # Qobs
chisq3$parameter # graus de liberdade -> errado
chisq3$p.value   # valor-p -> errado
chisq3$observed  # Oi
chisq3$expected  # Ei=npi

# verificar as regras recomendadas

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.2b$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.2b$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp <- 1-pchisq(exemplo.2b$statistic, gl)
valorp

# cálculo do quantil de probabilidade da a Região critíca
qchisq(0.99, gl)

#######################################################
#######################################################
#######################################################

## Exemplo - Distribuição de Poisson - Com Junção de Linhas

# H0: X segue uma distribuição de Poisson
# vs.
# H1: X NÃO segue uma distribuição de Poisson


# Domínio -> atenção que o 5 representa maior ou igual 5
# (devido ao domínio da Poisson)
(xi<- c(0:5))

# número de linhas da tabela de frequências
(k<-length(xi))

# frequências observadas
(Oi=c(6,14,10,7,2,1))

# dimensão da amostra
(n <- sum(Oi))

# é necessário estimar o lambda -> média amostral
(elx <- sum(xi*Oi)/n)
#ou
amostra <- c(rep(0,6), rep(1,14), rep(2,10), rep(3,7), rep(4,2), rep(5,1))
(elx <- mean(amostra))


r <- 1  # estimou-se um parâmetro
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =dpois(xi,elx))
pi[k] <- 1-ppois(xi[k-1],elx)  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

# teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

# dar um nome para poder ver os diversos campos
exemplo.3 <- chisq.test(x=Oi,p=pi)

# verificar as regras recomendadas

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.3$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.3$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# juntar as duas últimas linhas
# (são as que têm as frequências que não respeitam uma das regras)
exemplo.3$expected

# perde 1 linha
k2 <- k-1

(gl2 <- k2-1-r)

# Domínio -> atenção que o 4 representa maior ou igual 4
# (devido ao domínio da Poisson)
(xi2<- xi[1:k2])

# frequências observadas
(Oi2=c(Oi[1:k2-1],Oi[k2]+Oi[k2+1]))

# probabilidades necessárias para as frequências esperadas
(pi2 =pi[1:k2])
pi2[k2] <- 1-ppois(xi2[k2-1],elx)  #P(X>=4)
sum(pi2)
round(pi2,4)

# teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi2,p=pi2)

exemplo.31 <- chisq.test(x=Oi2,p=pi2)

# verificar as regras recomendadas
# verificar só a que falhou anteriormente

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.31$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp2 <- 1-pchisq(exemplo.31$statistic, gl2)
valorp2

#cálculo do quantil de probabilidade da a Região critíca
qchisq(0.99, gl2)

#######################################################
#######################################################
#######################################################

## Exemplo - Distribuição Exponencial

# H0: X segue uma distribuição Exponencial
# vs.
# H1: X Não segue uma distribuição Exponencial

amostra <- c(
  1476,300,98,221,157,
  182,499,552,1563,36,
  246,442,20,796,31,
  47,438,400,279,247,
  210,284,553,767,1297,
  214,428,597,2025,185,
  467,401,210,289,1024
)

# Regra de Sturges

# dimensão da amostra
(n <- length(amostra))

# número de classes ou de linhas da tabela de frequências
(k<-trunc(1+log(n)/log(2)))

# amplitude de cada classe = h
(h<-(range(amostra)[2]-range(amostra)[1])/k)

# valor mínimo
(valor.min <- min(amostra))

# valor mínimo
(valor.max <- valor.min + h*k)

# definir as classes -> cut()
cortes <- seq(valor.min, valor.max, by=h)
(classes <- cut(
  amostra,
  breaks=cortes,
  right=TRUE,
  include.lowest=TRUE,
  dig.lab = 5
))
levels(classes)

# frequências absolutas = frequências Observadas
(Oi<-table(classes))

# é necessário estimar o tetha -> média amostral
(elx <- mean(amostra))

r <- 1  # estimou-se um parâmetro
(gl <- k-1-r)  # graus de liberdade da distribui??o Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =pexp(cortes[2:(k+1)],1/elx))
for(i in 2:k){
  pi[i] <- pexp(cortes[i+1],1/elx)-pexp(cortes[i],1/elx)
} 
pi[k] <- 1-pexp(cortes[k],1/elx)  # corrigir a última probabilidade
round(pi,4)
sum(pi)

# teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

# dar um nome para poder ver os diversos campos
exemplo.4 <- chisq.test(x=Oi,p=pi)

# verificar as regras recomendadas

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.4$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.4$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# juntar as quatro últimas linhas 
# (são as que têm as frequências que não respeitam uma das regras)
exemplo.4$expected

# perde 3 linhas
k2 <- k-3

(gl2 <- k2-1-r)

# limites das classes
(cortes2<- c(cortes[1:k2],cortes[k+1]))

# definir as classes -> cut()
(classes2 <- cut(
  amostra,
  breaks=cortes2,
  right=TRUE, 
  include.lowest=TRUE,
  dig.lab = 5
))

# frequências absolutas = frequências Observadas
(Oi2<-table(classes2))

# probabilidades necessárias para as frequências esperadas
(pi2 =pexp(cortes2[2:(k2+1)],1/elx))
for(i in 2:k2){
  pi2[i] <- pexp(cortes2[i+1],1/elx)-pexp(cortes2[i],1/elx)
} 
pi2[k2] <- 1-pexp(cortes2[k2],1/elx)
round(pi2,4)
sum(pi2)

# teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi2,p=pi2)

exemplo.41 <- chisq.test(x=Oi2,p=pi2)

# verificar as regras recomendadas (só para a que falhou anteriormente)

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.41$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp2 <- 1-pchisq(exemplo.41$statistic, gl2)
valorp2

#cálculo do quantil de probabilidade da a Região critíca
qchisq(0.90, gl2)

##############################################################################
##############################################################################

# TesteS de Ajustamento de Kolmogorov-Smirnov:

# Função -> ks.test()

#######################################################
#######################################################
#######################################################

## Exemplo: Distribuição Exponencial, de Média 730

# H0: X segue uma distribuição Exponencial de média 730 horas
# vs.
# H1: X Não segue uma distribuição Exponencial  de média 730 horas

# amostra
amostra5 <- c(1476,182,246,300,499,442,98,552,20,221,1563,796,157,36,31)
length(amostra5)

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(
  amostra5,    # Amostra
  "pexp",      # Distribuição a Testar
  rate=1/730   # Média
)

#######################################################
#######################################################
#######################################################

# Exemplo: Distribuição Normal, Média 80 e Desvio Padrão 6.95

# H0: X segue uma distribuição Normal de média 80 e desvio padrão 6.95
# vs.
# H1: X Não segue uma distribuição Normal de média 80 e desvio padrão 6.95

# amostra
amostra6 <- c(
  75, 92, 80, 80, 84, 72, 84, 77, 81,
  77, 75, 81, 80, 92, 72, 77, 78, 76,
  77, 86, 77, 92, 80, 78, 68, 78, 92,
  68, 80, 81, 87, 76, 80, 87, 77, 86,
  74, 93, 79, 81, 83, 71, 83, 78, 80,
  76, 76, 80, 82, 91, 72, 76, 79, 75
)

# Teste de Ajustamento de Kolmogorv-Smirnov
ks.test(
  amostra6,  # Amostra
  "pnorm",   # Distribuição a Testar
  mean=80,   # Média
  sd=6.95    # Desvio Padrão
)

##############################################################################
##############################################################################

# TesteS de Ajustamento de Lilliefors:

# Função -> library(nortest) lillie.test()

#######################################################
#######################################################
#######################################################

# Exemplo: Distribuição Normal

# H0: X segue uma distribuição Normal
# vs.
# H1: X Não segue uma distribuição Normal

# amostra
amostra6 <- c(
  75, 92, 80, 80, 84, 72, 84, 77, 81,
  77, 75, 81, 80, 92, 72, 77, 78, 76,
  77, 86, 77, 92, 80, 78, 68, 78, 92,
  68, 80, 81, 87, 76, 80, 87, 77, 86,
  74, 93, 79, 81, 83, 71, 83, 78, 80,
  76, 76, 80, 82, 91, 72, 76, 79, 75
)

# Teste de Ajustamento de Lilliefors
library(nortest)
lillie.test(
  amostra6  # Amostra
)

##############################################################################
##############################################################################

# Testes de Ajustamento de Shapiro-Wilk:

# Função -> shapiro.test()

#######################################################
#######################################################
#######################################################

# Exemplo: Distribuição Normal

# H0: X segue uma distribuição Normal
# vs.
# H1: X Não segue uma distribuição Normal

amostra7 <- c(
  75, 92, 80, 80, 84, 72, 84, 77, 81,
  77, 75, 81, 80, 92, 72, 77, 78, 76,
  77, 86, 77, 92, 80, 78, 68, 78, 92,
  68, 80, 81, 87, 76, 80, 87, 77, 86
)

# Teste de Ajustamento de Shapiro-Wilk
shapiro.test(
  amostra7   # Amostra
)

##############################################################################
##############################################################################

# Testes de Wilcoxon e Testes de Mann-Whitney:

# Função -> wilcox.test()
# Argumentos da Função: Amostra X e Amostra Y

# Testa a Igualdade entre Duas Distribuições

#######################################################
#######################################################
#######################################################

# Exemplo: Wilcoxon

# D=Y-X

# H0: Média de D = 0
# vs.
# H1: Média de D > 0

# Amostras
amostra1.x <- c(3.5,3.6,4.1,2.9,3.4,4.2,3.9,4.1)
amostra1.y <- c(3.4,3.9,4.5,3.1,3.9,4.4,3.8,4.1)

# Teste de Wilcoxon
wilcox.test(
  x = amostra1.y,           # Amostra 1
  y = amostra1.x,           # Amostra 2
  alternative = "greater",  # Teste Unilateral Direito (H1 > ...)
  mu = 0,                   # Média
  paired = TRUE             # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.1 <- wilcox.test(
  x = amostra1.y,           # Amostra 1
  y = amostra1.x,           # Amostra 2
  alternative = "greater",  # Teste Unilateral Direito (H1 > ...)
  mu = 0,                   # Média
  paired = TRUE             # São Emparelhadas?
)

exemplo.1$statistic   # T+
exemplo.1$p.value     # valor-p
exemplo.1$null.value  # H0: MD = 0
exemplo.1$alternative # H1: MD > 0

#######################################################
#######################################################
#######################################################

# Exemplo: Wilcoxon

# D=Y-X

# H0: Média de D = 0
# vs.
# H1: Média de D < 0

# amostras
amostra2.x <- c(82.7,73.2,84.1,84.1,81.6,78.9,85.6,80.2,84.5,73.8)
amostra2.y <- c(74.5,73.2,79.1,85.6,81.6,79.6,81.5,80.2,86.9,73.8)

# Teste de Wilcoxon
wilcox.test(
  x = amostra2.y,           # Amostra 1
  y = amostra2.x,           # Amostra 2
  alternative = "less",     # Teste Unilateral Esquerdo (H1 < ...)
  mu = 0,                   # Média
  paired = TRUE             # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.2 <- wilcox.test(
  x = amostra2.y,           # Amostra 1
  y = amostra2.x,           # Amostra 2
  alternative = "less",     # Teste Unilateral Esquerdo (H1 < ...)
  mu = 0,                   # Média
  paired = TRUE             # São Emparelhadas?
)
exemplo.2$statistic   # T+
exemplo.2$p.value     # valor-p
exemplo.2$null.value  # H0: MD = 0
exemplo.2$alternative # H1: MD < 0

#######################################################
#######################################################
#######################################################

# Exemplo: Wilcoxon

# D=Y-X

# H0: Média de D = 0
# vs.
# H1: Média de D != 0

# amostras
amostra3.x <- c(10,12,13,14,11,12.4,15,9.8,12.9,12.9)
amostra3.y <- c(9.8,11.6,12,14,11,13,16,12,13,13.4)

# Teste de Wilcoxon
wilcox.test(
  x = amostra3.y,             # Amostra 1
  y = amostra3.x,             # Amostra 2
  alternative = "two.sided",  # Teste Bilateral (H1 != ...)
  mu = 0,                     # Média
  paired = TRUE               # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.3 <- wilcox.test(
  x = amostra3.y,             # Amostra 1
  y = amostra3.x,             # Amostra 2
  alternative = "two.sided",  # Teste Bilateral (H1 != ...)
  mu = 0,                     # Média
  paired = TRUE               # São Emparelhadas?
)
exemplo.3$statistic   # T+
exemplo.3$p.value     # valor-p
exemplo.3$null.value  # H0: MD = 0
exemplo.3$alternative # H1: MD != 0

#######################################################
#######################################################
#######################################################

# Exemplo: Mann-Whitney

# H0: Média de X = Média de Y
# vs.
# H1: Média de X < Média de Y

# amostras
amostra4.x <- c(
  14.4,14.2,13.8,16.5,14.1,16.6,15.9,
  15.6,14.1,15.3,15.7,16.7,13.7,15.3,14.0
)
amostra4.y <- c(
  17.4,16.2,17.1,17.5,15.0,
  16.0,16.9,15.0,16.3,16.8
)

# MX - MY

# Teste de Mann-Whitney
wilcox.test(
  x = amostra4.x,        # Amostra 1
  y = amostra4.y,        # Amostra 2
  alternative = "less",  # Teste Unilateral Esquerdo (H1 < ...)
  mu = 0,                # Média
  paired = FALSE         # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.4 <- wilcox.test(
  x = amostra4.x,        # Amostra 1
  y = amostra4.y,        # Amostra 2
  alternative = "less",  # Teste Unilateral Esquerdo (H1 < ...)
  mu = 0,                # Média
  paired = FALSE         # São Emparelhadas?
)
exemplo.4$statistic   # Uobs
exemplo.4$p.value     # valor-p
exemplo.4$null.value  # H0: MX-MY = 0
exemplo.4$alternative # H1: MX-MY < 0

#######################################################
#######################################################
#######################################################

# Exemplo: Mann-Whitney

# H0: Média de X = Média de Y
# vs.
# H1: Média de X > Média de Y

# amostras
amostra5.x <- c(1.62,0.51,1.29,0.71,0.52,2.10,0.88,0.99,0.51,1.59)
amostra5.y <- c(0.92,1.29,2.81,0.82,4.48,0.71,1.10,0.41)

# MX - MY

# Teste de Mann-Whitney
wilcox.test(
  x = amostra4.x,           # Amostra 1
  y = amostra4.y,           # Amostra 2
  alternative = "greater",  # Teste Unilateral Direito (H1 > ...)
  mu = 0,                   # Média
  paired = FALSE            # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.5 <- wilcox.test(
  x = amostra4.x,           # Amostra 1
  y = amostra4.y,           # Amostra 2
  alternative = "greater",  # Teste Unilateral Direito (H1 > ...)
  mu = 0,                   # Média
  paired = FALSE            # São Emparelhadas?
)
exemplo.5$statistic   # Uobs
exemplo.5$p.value     # valor-p
exemplo.5$null.value  # H0: MX-MY = 0
exemplo.5$alternative # H1: MX-MY > 0

#######################################################
#######################################################
#######################################################

# Exemplo: Mann-Whitney

# H0: Média de X = Média de Y
# vs.
# H1: Média de X != Média de Y

# amostras
amostra6.x <- c(607.4,809.1,488.8,481.1,592.8,345.4,620.0,407.7,513.3,527.4)
amostra6.y <- c(694.5,629.6,676.9,430.3,727.2)

# MX - MY

# Teste de Mann-Whitney
wilcox.test(
  x = amostra6.x,             # Amostra 1
  y = amostra6.x,             # Amostra 2
  alternative = "two.sided",  # Teste Bilateral (H1 != ...)
  mu = 0,                     # Média
  paired = FALSE              # São Emparelhadas?
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.6 <- wilcox.test(
  x = amostra6.x,             # Amostra 1
  y = amostra6.x,             # Amostra 2
  alternative = "two.sided",  # Teste Bilateral (H1 != ...)
  mu = 0,                     # Média
  paired = FALSE              # São Emparelhadas?
)
exemplo.6$statistic   # Uobs
exemplo.6$p.value     # valor-p
exemplo.6$null.value  # H0: MX-MY = 0
exemplo.6$alternative # H1: MX-MY != 0

##############################################################################
##############################################################################

# Testes de Independência do Qui-Quadrado:

# Função -> chisq.test()
# Argumentos da Função: Tabela de Contingência

#######################################################
#######################################################
#######################################################

# Exemplo 1:

# Tabela de Contingência
# | 24 | 41 |
# |  6 | 11 |

# Como no enunciado está a tabela de contingência,
# há duas possibilidades para aceder a essa tabela:

## Escrever a tabela de contingência como uma data.frame
## (introduzir os dados por coluna)
(tabela.contingencia <- data.frame(
  coluna1 = c(24,6),
  coluna2 = c(41,11)
))

# ou 

## ler os dados -> dados1.txt
dados1
## construir a tabela de contingência
(tabela.contingencia <- table(dados1$pai, dados1$filho))

# Teste de Independência do Qui-Quadrado
chisq.test(
  tabela.contingencia,  # Tabela de Contingência
  correct = FALSE
)

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.1 <- chisq.test(
  tabela.contingencia,  # Tabela de Contingência
  correct = FALSE
)
exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value   # valor-p
exemplo.1$observed  # Oi = frequências Observadas
exemplo.1$expected  # Ei = frequências Esperadas

# Graus de liberdade da distribuição Qui-Quadrado
exemplo.1$parameter

# ou
(2-1)*(2-1)

# Região Critíca
# cálculo do quantil de probabilidade da região crítica
qchisq(0.95,exemplo.1$parameter)

# valor-p
exemplo.1$p.value

# ou
1-pchisq(exemplo.1$statistic, exemplo.1$parameter)

#######################################################
#######################################################
#######################################################

# Exemplo 2:

# Tabela de Contingência
# | 132 | 57 | 13 |
# |  28 | 15 | 15 |
# |  20 | 17 | 17 |

# Data-Frame - Construir Por Coluna:
(tabela.contingencia2 <- data.frame(
  coluna1 = c(132, 28, 20),
  coluna2 = c(57, 15, 17),
  coluna3 = c(13, 15, 17)
))

# ou 
# ler os dados -> dados2.txt
dados2
# como as variáveis são qualitativas ordinais -> definir a ordem dos níveis
dados2$Rep <- factor(dados2$Rep, levels=c("Nenhuma","Uma","Duas ou mais"))
dados2$Faltas <- factor(dados2$Faltas, levels=c("Nenhuma","Algumas","Muitas"))
# construir a tabela de contingência
(tabela.contingencia2 <- table(dados2$Rep, dados2$Faltas))

# Reste de Independência do Qui-Quadrado
chisq.test(tabela.contingencia2, correct = FALSE)
chisq.test(tabela.contingencia2)
# correct = FALSE não é necessário para tabelas bidimensionais

# Atribuir um "nome" ao teste para aceder aos campos
exemplo.2 <- chisq.test(tabela.contingencia2)
exemplo.2$statistic # Qobs
exemplo.2$parameter # graus de liberdade
exemplo.2$p.value   # valor-p
exemplo.2$observed  # Oi = frequências Observadas
exemplo.2$expected  # Ei = frequências Esperadas

# Graus de liberdade da distribuição Qui-Quadrado
exemplo.2$parameter

# ou
(3-1)*(3-1)

# Região Critíca
# cálculo do quantil de probabilidade da região crítica
qchisq(0.99,exemplo.2$parameter)

# Valor-p
exemplo.2$p.value

# ou
1-pchisq(exemplo.2$statistic, exemplo.2$parameter)

##############################################################################
##############################################################################

# Medidas de Associação:

library(DescTools)

# Tabela de Contingência de Exemplo
# | 24 | 41 |
# |  6 | 11 |

(tabcont <- data.frame(
  coluna1 = c(24,6),
  coluna2 = c(41,11)
))

# Coeficiente de Contingência
ContCoef(tabcont)

# Coeficiente V de Crámer
CramerV(tabcont)

# Coeficiente Tb de Kendall
KendallTauB(tabcont)
# ou
KendallTauB(as.matrix(tabcont))

##############################################################################
##############################################################################

# Exemplos da Aula Laboratorial Extra (07-06-2023):

#######################################################
#######################################################
#######################################################

# Teste de Ajustamento do QUi-Quadrado

#################
### Ex. 6 - 2 ###
#################

# X - Nº de rapazes por família com 5 filhos.

# DOMINIO DE x = { 0, 1, 2, 3, 4, 5 }

# Nivel de significancia = 1%

# X ~ Binomial(n, p)

# N -> 5
# P -> 0.5 (50%)

# H0 - X é binomial.
# vs.
# H1: X não é binomial.

# Teste de Ajustamento: Qui-Quadrado
# A distribuição binomial é discreta.

# Informação Necessária:
# Oi -> Valoers Observados
# Pi -> Probabilidades Esperadas, supondo H0 verdadeiro.

# Tabela:
# ---------------------------------------------
# | Xi | ni = Oi |         Pi                 |
# ---------------------------------------------
# | 0  |    8    | P(X=0) = dbinom(0, 5, 0.5) |
# ---------------------------------------------
# | 1  |   40    | P(X=1) = dbinom(1, 5, 0.5) |
# ---------------------------------------------
# | 2  |   88    | P(X=2) = dbinom(2, 5, 0.5) |
# ---------------------------------------------
# | 3  |  110    | P(X=3) = dbinom(3, 5, 0.5) |
# ---------------------------------------------
# | 4  |   56    | P(X=4) = dbinom(4, 5, 0.5) |
# ---------------------------------------------
# | 5  |   18    | P(X=5) = dbinom(5, 5, 0.5) |
# ---------------------------------------------

# Oi
oi <- c(8, 40, 88, 110, 56, 18)

# Pi
pi <- dbinom(0:5, 5, 0.5)

# Teste:
teste <- chisq.test(
  x = oi,
  p = pi
)

# Estatística de Teste: Q ~ X^2(K-1-R)
# K = 6 (Nº de valores no domínio)
# R = 0 (Probabilidade

# Q ~ X^2(5)

# Valores Esperados:
teste$expected

# Tomada de Decisão:

## Segundo o P-Value:
teste$p.value
# Como p-value = 0.03534 > alpha = 0.01: Não se rejeita H0.

## Segundo a Região Crítica:
# X^2(5, 1-alpha) = X^2(5, 0.99)
# Quantil:
qchisq(0.99, 5)
# RC = [ 15.08627, +inf [
# Como Qobs = 11.96 !E RC: Não se rejeita H0.

# Conclusão:
# Com base na amostra e para um nível de significância de 1%,
# o Nº de rapazes por família com 5 filhos segue uma distribuição binomial.

#######################################################
#######################################################
#######################################################

#################
### Ex. 6 - 6 ###
#################

# média = 290 gramas
# Desvio padrão = 56 gramas
# Distribuição normal

# n = 10
# alpha = 5% = 0.05

amostra62 <- c(198, 254, 262, 272, 275, 278, 285, 287, 287, 292)

# X - Quantidade de Carne

# -> O cozinheiro afirmou que X ~ N(290, 56)

# H0: X ~ N(290, 56)
# vs.
# H1: X !~ N(290, 56)

# Teste de Ajustamento: Kolmogorov-Smirnov
# ks.test
# Precisa: Amostra; distribuição
(teste <- ks.test(
  amostra62,
  "pnorm",
  290,
  56
))

# Tomada de Decisão:
## Valor-P: 0.01785 <= alpha = 0.05: Logo, Rejeita-se H0.

# Conclusão:
# Com base na amostra, a afirmação dita pelo cozinheiro é falsa.

## EXTRA ##

# B) Diga a partir de que nível de significância a quantidade de carne
# servida não segue uma distribuição normal.

# H1: X ~ N
# vs.
# H1: X !~ N

# Teste: Shapiro-Wilk, pois n < 50
shapiro.test(amostra62)

# Valor-P <= alpha -> Rejeita-se H0.

# Como p-value = 0.003693, rejeita-se H0 par alpha = 0.003693.

## EXTRA ##

# C) Suponha que a quantidade de carne servida segue uma distribuição normal.
# Para um nível de significância de 5%, teste se os parâmetros indicados pelo
# cozinheiro são admissíveis.

# 1º

# H0: sigma = 56 <=> o^2 = 56^2
# vs.
# H1: sigma != 56 <=> o^2 = 56^2

# Teste Bilateral

# TH Para a Variância:
library(EnvStats)
varTest(
  amostra62,
  alternative = "two.sided",
  sigma.squared = 56^2
)

# Decisão:

## P-Value:
# Como P-Value = 0.02389 <= alpha = 0.05 => Rejeita-se H0.

## Região Crítica:
# X^2(n - 1) = X^2(9)

# Quantil de Prob. para RC
qchisq(0.05/2, 9)
qchisq(1-0.05/2, 9)

# RC = [ 0, 2.7 ] U [ 19.02, +inf [

# Como Xobs = 2.19 E RC => Rejeita-se H0.

# Conclusão:
# Com base na amostra e com um nível de significância de 5%, conclui-se
# que o desvio padrão não é 56.

# 2º

# H0: /u = 290
# vs.
# H1: /u != 290

# População normal, média 290 e desvio padrão desconhecido

# T ~ t(n - 1) = t(9)

# TH para a Média
(t.test(
  amostra62,
  alternative = c("two.sided"),
  mu = 290
))

# Decisão:

## P-Value:
# Como P-Value = 0.03971 <= alpha = 0.05 => Rejeita-se H0.

## Região Crítica:
# T(n - 1) = T(9)

# Quantil de Prob. para RC
qt(1-0.05/2, 9)

# RC = [ -inf, -2.26 ] U [ 2.26, +inf [

# Como Tobs = -2.4 E RC => Rejeita-se H0.

# Conclusão:
# Com base na amostra e com um nível de significância de 5%, conclui-se
# que a média não é 290.

##############################################################################
##############################################################################

##############################################################################
########################## Resumo R: Primeiro Teste ##########################
##############################################################################

# Os dados estão no ficheiro da obesidade
# Tabela em uso:
View(obesidade)

##############################################################################
##############################################################################

# PAra ordenar antes de Realizar a Tabela de Frequências:
(obesidade$FAVC <- factor(     # Variável para guardar a ordenação
  obesidade$FAVC,              # Variável a ordenar
  levels=c("N", "S", "F", "A") # Categorias para ordenar
))
# O comando acima não está correto, é só um exemplo!

##############################################################################
##############################################################################

# Fazer uma consulta a uma tabela existente:
obesidade["CONDIÇÃO",]
# Exemplo - Quantas pessoas inquiridas têm mais de 40 anos?
(obesidade[obesidade$Idade>44,])

##############################################################################
##############################################################################

# Como Construir uma Tabela de Frequências Sem Classes:
# Exemplo: Sobre a Variável FAVC

(ni.FAVC <- table(obesidade$FAVC))          # Frequências Absolutas
(fi.FAVC <- round(prop.table(ni.FAVC), 4))  # frequências Relativas
(Ni.FAVC <- cumsum(ni.FAVC))                # frequências Absolutas Acumuladas
(Fi.FAVC <- round(cumsum(fi.FAVC), 4))      # frequências Relativas Acumuladas

# Tabela de Frequências
(tabela.frequencias.FAVC <- data.frame(
  i = 1:nrow(ni.FAVC),
  xi = names(ni.FAVC),
  ni = as.integer(ni.FAVC),
  fi = as.numeric(fi.FAVC),
  Ni = as.integer(Ni.FAVC),
  Fi = as.numeric(Fi.FAVC)
))
# OU
DescTools::Freq(obesidade$FAVC)

##############################################################################
##############################################################################

# Como Construir uma Tabela de Frequências Com Classes:
# Exemplo: Sobre a Variável Idade

# Mínimo e Máximo dos Dados
min(obesidade$Idade)
max(obesidade$Idade)

# Caso se soubesse o nº de classes e a amplitude pelo enunciado:
  # 8 Classes
(k <- 8)
  # Amplitude = 6
(h <- 6)
# Caso não se soubesse o nº de classes e a amplitude pelo enunciado:
(n <- nrow(obesidade))
  # Nº de Classes:
(k <- trunc(1 + log(n)/log(2)))
  # Amplitude das Classes:
(h <- (max(obesidade$Peso) - min(obesidade$Peso)) / k)

# Mínimo e Máximo das Classes
(idade.min <- min(obesidade$Idade))
(idade.max <- idade.min + h * k)

# Extremos das Classes
(idade.cortes <- seq(idade.min, idade.max, by = h))

# Intervalos: Abertos à Esquerda e Fechados à Direita
(idade.classes <- cut(
  obesidade$Idade,
  breaks = idade.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Tabela de Frequências com Classes
(ni.idade <- table(idade.classes))           # Frequências Absolutas
(fi.idade <- round(prop.table(ni.idade), 4)) # Frequências Relativas
(Ni.idade <- cumsum(ni.idade))               # Frequências Absolutas Acumuladas
(Fi.idade <- round(cumsum(fi.idade), 4))     # Frequências Relativas Acumuladas

(tabela.frequencias.Idade <- data.frame(
  i=1:nrow(ni.idade),
  xi=names(ni.idade),
  ni=as.integer(ni.idade),
  fi=as.numeric(fi.idade),
  Ni=as.integer(Ni.idade),
  Fi=as.numeric(Fi.idade)
))

##############################################################################  
##############################################################################

# Para verificar se existem dados omissos:
any(is.na(obesidade))
# OU
anyNA(obesidade)

##############################################################################
##############################################################################

# Legenda Para os Gráficos (Funciona para Todos):
legend(
  "topright",                                   # Posição da Legenda
  legend = c("Nunca", "Às vezes", "Sempre"),    # Valores da Legenda (Nomes)
  fill = c(2:4),                                # Cores dos Valores
  cex = 1                                       # Tamanho do Objeto (Escala)
)
### O Comando da Legenda Ser Executado Depois de Criar o Gráfico!

##############################################################################
##############################################################################

# Gráfico de Barras de Frequências Absolutas - Variável FCVC:

# Frequências Absolutas
(ni.FCVC <- table(obesidade$FCVC))

# Gráfico de Barras
barplot(
  ni.FCVC,                                               # Variável no Gráfico
  main = "Come Vegeitais? (Frequências Absolutas)",      # Título do Gráfico
  xlab = "FCVC",                                         # Título Eixo XX
  ylab = "Frequências absolutas",                        # Título Eixo YY
  col = 2:4,                                             # Cores das Barras 
  ylim = c(0, 1200),                                      # Limites do Eixo YY
  names.arg = c("Nunca", "Às vezes", "Sempre")           # Nomes das Barras
)

##############################################################################
##############################################################################

# Gráfico de Barras de Frequências Relativas - Variável FAF:

# Frequências Absolutas
(ni.FAF <- table(obesidade$FAF))
# Frequências Relativas
(fi.FAF <- round(prop.table(ni.FAF), 4))

# Gráfico de Barras
barplot(
  fi.FAF,                                                # Variável no Gráfico
  main = "Física P/ Semana (Frequências Relativas)",     # Título do Gráfico
  xlab = "FAF",                                          # Título Eixo XX
  ylab = "Frequências Relativas",                        # Título Eixo YY
  col = 3:6,                                             # Cores das Barras 
  ylim = c(0, 0.4),                                      # Limites do Eixo YY
  names.arg = c(                                         # Nomes das Barras
    "Não Pratica",
    "1 - 2 Dias",
    "3 - 4 Dias",
    "+ de 4 Dias"
  )
)

##############################################################################
##############################################################################

# Gráfico de Barras de Frequências Relativas Em Percentagem - Variável CALC:

# Colocar as Categorias da Variável por Ordem
obesidade$CALC <- factor(
  obesidade$CALC,
  levels=c("N", "S", "F", "A")
)

# Frequências Absolutas
(ni.CALC <- table(obesidade$CALC))
# Frequências Relativas
(fi.CALC <- round(prop.table(ni.CALC), 4))

# Gráfico de Barras
barplot(
  fi.CALC * 100,                                          # Variável (%)
  main = "Bebidas Alcoólicas (Frequências Relativas %)",  # Título do Gráfico
  xlab = "CALC",                                          # Título Eixo XX
  ylab = "Frequências Relativas em %",                    # Título Eixo YY
  col = 5:2,                                              # Cores das Barras 
  ylim = c(0, 80)                                         # Limites do Eixo YY
)
legend(
  "topright",
  legend = c("N = Nunca", "S = Às vezes", "F = Frequentemente", "A = Sempre"),
  fill = 5:2
)

##############################################################################
##############################################################################

# Gráfico Circular - Variável Genero:

# Frequências Absolutas
(ni.G <- table(obesidade$Genero))
# Frequências Relativas
(fi.G <- round(prop.table(ni.G), 4))

# Gráfico
pie(
  ni.G,
  labels=paste(fi.G * 100, "%"),
  col=c("red", "blue"),
  main="Género"
)

# Legenda
legend(
  "topright",
  legend=names(ni.G),
  fill=c("red", "blue"),
  cex = 1
)

##############################################################################
##############################################################################

# Gráfico de Circular - Variável MTRANS:

# Colocar as Categorias da Variável por Ordem
obesidade$MTRANS <- factor(
  obesidade$MTRANS,
  levels=c(
    "Automovel",
    "Mota",
    "Transportes_Publicos",
    "Bicicleta",
    "A_pe"
  )
)

# Frequências Absolutas
(ni.MTRANS <- table(obesidade$MTRANS))
# Frequências Relativas
(fi.MTRANS <- round(prop.table(ni.MTRANS), 4))

pie(
  ni.MTRANS,
  labels = paste(fi.MTRANS*100, "%"),
  cex = 0.8,
  col = 2:6, 
  main = "Tipo de Transporte Habitual"
)

legend(
  "topleft",
  legend=c(
    "Automóvel",
    "Mota",
    "Transportes Públicos",
    "Bicicleta",
    "Anda a Pé"
  ), 
  fill=2:6,
  cex = 0.8
)

##############################################################################
##############################################################################

# Valores Necessários Para Desenhar Os Histogramas: Exemplos Seguintes - Idade:

# Mínimo e Máximo dos Dados
min(obesidade$Idade)
max(obesidade$Idade)

## Os valores abaixo também podem já ter sido calculados
## para realizar a tabela de frequências com classes.

# Nº de Classes
k.idade <- 8

# Amplitude das Classes
h.idade <- 6

# Mínimo e Máximo das Classes
(valor.min.idade <- min(obesidade$Idade))
(valor.max.idade <- valor.min.idade + h.idade * k.idade)

# Extremos das Classes
(cortes.idade <- seq(valor.min.idade, valor.max.idade, by = h.idade))

##############################################################################
##############################################################################

# Histograma - Eixo YY - Freq. Absolutas:

hist(
  obesidade$Idade,
  breaks = cortes.idade,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "Idade",
  ylab = "Frequências Absolutas",
  col = 2,
  xlim = c(0,70),
  ylim = c(0,1200),
  xaxt = "n"          # Para Poder Definir o Eixo XX de Seguida
)

axis(
  side = 1,
  at = c(0, cortes.idade, 70)
)  # Define os Valores para o Eixo XX Igual Às Classes

##############################################################################
##############################################################################

# Histograma - Eixo YY - fi/h - Comparação entre Qualquer Tipo de Classes:
hist(
  obesidade$Idade,
  breaks = cortes.idade,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE,
  main = "Histograma",
  xlab = "Idade",
  ylab = "Frequências Relativas / Amplitude das Classes",
  col = 2,
  xlim = c(0,70),
  ylim = c(0,0.1),
  xaxt = "n"
)                  # para poder definir o eixo dos xx

axis(
  side = 1,
  at = c(0, cortes.idade, 70)
)  # Define os Valores para o Eixo XX Igual Às Classes

##############################################################################
##############################################################################

# Histograma - Eixo YY - fi (Exemplo) - Comparação Mesmo Tipo de Classes:

# Tabela de Frequências
(classes.idade <- cut(
  obesidade$Idade,
  breaks = cortes.idade,
  right = TRUE,
  include.lowest = TRUE
))

# Frequências Absolutas
(ni.di <- table(classes.idade))

# Frequências Relativas
(fi.di <- round(prop.table(ni.di),4))

# Atribuir um nome ao histograma para poder aceder aos seus campos
graf <- hist(
  obesidade$Idade,
  breaks = cortes.idade,
  right = TRUE,
  include.lowest = TRUE
)

graf$density <- fi.di

plot(
  graf,
  freq = FALSE,
  main = "Histograma",
  xlab = "Idade",
  ylab = "Frequências Relativas",
  col = 2,
  xlim = c(0, 70),
  ylim = c(0, 0.5),
  xaxt = "n"
)

axis(
  side = 1,
  at = c(0, cortes.idade, 70)
)

##############################################################################
##############################################################################

# VARIÁVEIS QUALITATIVAS:

# Só se faz a Medida de Localização Central - Moda!

############################################

# VARIÁVEIS QUANTITATIVAS:

# Variável: Idade
(ni.Idade <- table(obesidade$Idade))

# Medidas de Localização Central:

## Moda:
if( min(ni.Idade)==max(ni.Idade)){
  print("Amodal")
} else{
  print(paste("Moda: ", names(ni.Idade)[ni.Idade==max(ni.Idade)]))
}
# OU
if( min(ni.Idade)==max(ni.Idade) ){
  print("Amodal")
} else{
  print(paste("Moda: ", DescTools::Mode(ni.Idade)))
}

## Média:
mean(obesidade$Idade)

## Mediana:
median(obesidade$Idade, type = 2)
# OU
quantile(obesidade$Idade, prob = 0.50, type = 2)

# Medidas de Localização Não Central:

## 1º e 3 Quantis:
quantile(
  obesidade$Idade,
  prob = c(0.25, 0.50, 0.75),
  type = 2
)

# Medidas de Dispersão:

## Dispersão Absoluta:

### Amplitude Total:
(max(obesidade$Idade)-min(obesidade$Idade))

### Amplitude Interquaril:
IQR(obesidade$Idade, type=2)

### Variância:
var(obesidade$Idade)

### Desvio Padrão:
sd(obesidade$Idade)

## Dispersão Relativa:

### Coeficiente de Variância = CV:
((sd(obesidade$Idade)/mean(obesidade$Idade))*100)

##############################################################################
##############################################################################

# Extremos e Quartis


# Diagrama de Extremos e Quartis - Caixa com Bigodes;
boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis",
  xlab="Sunspots",
  type = 2
)

# Sem Indicaçãoo de Outliers:
boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Sem Outliers",
  xlab = "sunspots",
  type = 2,
  range = 0
)

# Outliers A Partir dos Moderados:
boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Moderados",
  xlab = "sunspots",
  type = 2,
  range = 1.5
)

# Ver Quem São os Outliers:
ver.outliers.moderados <- boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  xlab = "sunspots",
  main = "Extremos e Quartis - Outliers Moderados",
  type = 2,
  range = 1.5
)
ver.outliers.moderados$out
length(ver.outliers.moderados$out)

# Outliers a Partir dos Severos:
boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Severos",
  xlab = "sunspots",
  type = 2,
  range = 3
)

# Ver Quem São os Outliers:
ver.outliers.severos <- boxplot(
  sunspots,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers Severos",
  xlab = "sunspots",
  type = 2,
  range = 3
)
ver.outliers.severos$out
length(ver.outliers.severos$out)

# Nº de Outliers Total:
length(ver.outliers.moderados$out)

# Nº de Outliers Moderados:
length(ver.outliers.moderados$out)-length(ver.outliers.severos$out)

# Nº de Outliers Severos:
length(ver.outliers.severos$out)

##############################################################################
##############################################################################

# Exemplo Para Calcular Integrais:

f1 <- function(y){y^2*(y-1)}
f2 <- function(y){y^2*(3-y)}

#E[X^2]
(
  0 +
  integrate(f1, lower=1, upper=2)$value +
  integrate(f2, lower=2, upper=3)$value +
  0
)

##############################################################################
##############################################################################

# Exemplo - Boxplot entre 2 Variáveis: Idade e FCVC
boxplot(
  Idade ~ FCVC,
  data = obesidade,
  col = 3:5,
  xlab = "Come vegetais à refeição?", 
  ylab = "Idade",
  type = 2
)

legend(
  "topleft",
  legend=c("1=Nunca", "2 = Às vezes", "3=Sempre"), 
  fill=3:5,
  cex = 0.7
)

##############################################################################
##############################################################################