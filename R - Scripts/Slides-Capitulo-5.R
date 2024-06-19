##############
# CAPÍTULO 5 #
##############


###############################
###############################
###############################

#          Exemplo 1          #

###############################
###############################
###############################

#amostra
amostra1 = c(25.8,76.0,59.6,61.4,51.3,66.2,30.4,37.5,57.2)

#teste de hipóteses paramétrico
library(BSDA)
z.test(x=amostra1, sigma.x=20, alternative="less", mu=60)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo1 = z.test(x=amostra1, sigma.x=20, alternative="less", mu=60))

# valor observado da estatística de teste sob H0
exemplo1$statistic

# Região crítica
# quantil de probabilidade da região crítica
qnorm(0.05)

# valor-p
exemplo1$p.value
#ou
pnorm(exemplo1$statistic)


###############################
###############################
###############################

#          Exemplo 2          #

###############################
###############################
###############################

#amostra: sucesso = 1 e insucesso = 0
amostra2 = c(rep(1,14),rep(0,1000-14))

#estimativa da proporção
14/1000
#ou
mean(amostra2)


###############
#Exemplo 2.1

#teste de hipóteses paramétrico
library(BSDA)
z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="greater", mu=0.01)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo21 = z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="greater", mu=0.01))

# valor observado da estatística de teste sob H0
exemplo21$statistic

# Região crítica
# quantil de probabilidade da região crítica
qnorm(1-0.05)

# valor-p
exemplo21$p.value
#ou
1-pnorm(exemplo21$statistic)


###############
#Exemplo 2.2

#teste de hipóteses paramétrico
library(BSDA)
z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="less", mu=0.01)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo22 = z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="less", mu=0.01))

# valor observado da estatística de teste sob H0
exemplo22$statistic

# Região crítica
# quantil de probabilidade da região crítica
qnorm(0.10)

# valor-p
exemplo22$p.value
#ou
pnorm(exemplo22$statistic)


###############
#Exemplo 2.3

#teste de hipóteses paramétrico
library(BSDA)
z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="two.sided", mu=0.01)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo23 = z.test(x=amostra2, sigma.x=sqrt(0.01*(1-0.01)), alternative="two.sided", mu=0.01))

# valor observado da estatística de teste sob H0
exemplo23$statistic

# Região crítica
# quantil de probabilidade da região crítica
qnorm(1-0.05/2)

# valor-p
exemplo23$p.value
#ou
2*(1-pnorm(abs(exemplo23$statistic)))


###################################
#observação: intervalo de confiança
IC.exemplo23 = z.test(x=amostra2, sigma.x=sqrt(0.014*(1-0.014)), conf.level=0.95)
IC.exemplo23$conf.int


###############################
###############################
###############################

#          Exemplo 3          #

###############################
###############################
###############################

#amostra dos meninos
amostra31 = c(rep(64,9),rep(72,16),rep(74,2),rep(90,5))
#dimensão da amostra
length(amostra31)
#estimativa da média
mean(amostra31)
#estimativa do desvio padrão
sd(amostra31)

#amostra das meninas
amostra32 = c(rep(70,8),rep(74,22),rep(76,2),rep(90,4))
#dimensão da amostra
length(amostra32)
#estimativa da média
mean(amostra32)
#estimativa do desvio padrão
sd(amostra32)


#teste de hipóteses paramétrico
library(BSDA)
z.test(x=amostra31, sigma.x=sd(amostra31), 
       y=amostra32, sigma.y=sd(amostra32),
       alternative="two.sided", mu=0)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo3 = z.test(x=amostra31, sigma.x=sd(amostra31), 
                   y=amostra32, sigma.y=sd(amostra32),
                   alternative="two.sided", mu=0))

# valor observado da estatística de teste sob H0
exemplo3$statistic

# Região crítica
# quantil de probabilidade da região critíca
qnorm(1-0.01/2)

# valor-p
exemplo3$p.value
#ou
2*(1-pnorm(abs(exemplo3$statistic)))


###############################
###############################
###############################

#          Exemplo 4          #

###############################
###############################
###############################

# amostras
antes = c(13, 18, 14, 16, 19, 12, 22)
depois = c(16, 24, 18, 14, 26, 17, 29)

#amostra D = Depois-Antes
depois-antes
#dimensão da amostra D
length(depois-antes)
#estimativa da média
mean(depois-antes)
#estimativa do desvio padrão
sd(depois-antes)

#teste de hipóteses paramétrico com D = Depois - Antes
t.test(x=depois, y=antes, alternative="greater", mu=0, paired=TRUE)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo4 = t.test(x=depois, y=antes, alternative="greater", mu=0, paired=TRUE))

# valor observado da estatística de teste sob H0
exemplo4$statistic

# graus de liberdade da distribuição t-Student
exemplo4$parameter
# ou
7-1

# Região crítica
# quantil de probabilidade da região critíca
qt(1-0.05,exemplo4$parameter)

# valor-p
exemplo4$p.value
#ou
1-pt(exemplo4$statistic,exemplo4$parameter)


###############
# outra possibilidade de resolução: só com "uma" amostra
d = depois -antes
t.test(x=d, alternative="greater", mu=0)


###############################
###############################
###############################

#          Exemplo 5          #

###############################
###############################
###############################

# dados
mtcars
?mtcars

###############
#Exemplo 5.1

#filtrar os dados
manual = mtcars[mtcars$am==1,]
automatica = mtcars[mtcars$am==0,]

#Amostras
#amostra 1
manual$mpg
length(manual$mpg)
mean(manual$mpg)
sd(manual$mpg)
#amostra 2
automatica$mpg
length(automatica$mpg)
mean(automatica$mpg)
sd(automatica$mpg)

#teste de ajustamento à normalidade
shapiro.test(manual$mpg)
shapiro.test(automatica$mpg)

#verificar se as variâncias podem ser consideradas iguais
#intervalo de confiança
var.test(x=manual$mpg, y=automatica$mpg, conf.level=0.95)
# OU
# teste de hipóteses
var.test(x=manual$mpg, y=automatica$mpg, alternative="two.sided", ratio=1)


#teste de hipóteses paramétrico para a diferença de médias
t.test(x=manual$mpg, y=automatica$mpg, alternative="greater", mu = 0,
       paired=FALSE, var.equal=TRUE)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo51 = t.test(x=manual$mpg, y=automatica$mpg, alternative="greater", mu = 0,
                    paired=FALSE, var.equal=TRUE))

# valor observado da estatística de teste sob H0
exemplo51$statistic

# graus de liberdade da distribuição t-Student
exemplo51$parameter
# ou
13+19-2

# Região crítica
# quantil de probabilidade da região crítica
qt(1-0.05,exemplo51$parameter)

# valor-p
exemplo51$p.value
#ou
1-pt(exemplo51$statistic,exemplo51$parameter)


###############
#Exemplo 5.2

#filtrar os dados
motorV = mtcars[mtcars$vs==0,]
motorL = mtcars[mtcars$vs==1,]

#amostras
#amostra 1
motorV$mpg
length(motorV$mpg)
mean(motorV$mpg)
sd(motorV$mpg)
var(motorV$mpg)
#amostra 2
motorL$mpg
length(motorL$mpg)
mean(motorL$mpg)
sd(motorL$mpg)
var(motorL$mpg)

#teste de ajustamento à normalidade
shapiro.test(motorV$mpg)
shapiro.test(motorL$mpg)

#teste de hipóteses paramétrico para o quociente de variâncias
var.test(x=motorV$mpg, y=motorL$mpg, alternative="two.side", ratio=2)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo52 = var.test(x=motorV$mpg, y=motorL$mpg, alternative="two.side", ratio=2))

# valor observado da estatística de teste sob H0
exemplo52$statistic

# graus de liberdade da distribuição F-Snedecor
exemplo52$parameter
# ou
18-1
14-1

# Região crítica
# quantil de probabilidade da região critíca
qf(0.05/2,exemplo52$parameter[1],exemplo52$parameter[2])
qf(1-0.05/2,exemplo52$parameter[1],exemplo52$parameter[2])

# valor-p
exemplo52$p.value
#ou
2*min(pf(0.26,exemplo52$parameter[1],exemplo52$parameter[2]), 
      1-pf(0.26,exemplo52$parameter[1],exemplo52$parameter[2]))


###############
#Exemplo 5.3

#amostra
motorV$mpg
length(motorV$mpg)
mean(motorV$mpg)
sd(motorV$mpg)
var(motorV$mpg)


#teste de hipóteses paramétrico para a variância
library(EnvStats)
varTest(x=motorV$mpg, alternative="less", sigma.squared=16)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo53 = varTest(x=motorV$mpg, alternative="less", sigma.squared=16))

# valor observado da estatística de teste sob H0
exemplo53$statistic

# graus de liberdade da distribuição Qui-Quadrado
exemplo53$parameter
# ou
18-1

# Região crítica
# quantil de probabilidade da região critíca
qchisq(0.05,exemplo53$parameter)

# valor-p
exemplo53$p.value
#ou
pchisq(exemplo53$statistic,exemplo53$parameter)

###############################
###############################
###############################

#          Exemplo 6          #

###############################
###############################
###############################

#amostras: sucesso = 1 e insucesso = 0
telefone = c(rep(1,320),rep(0,1600-320))
internet = c(rep(1,475),rep(0,2500-475))

#estimativas das proporções
320/1600
#ou
mean(telefone)

475/2500
#ou
mean(internet)

#teste de hipóteses paramétrico
library(BSDA)
z.test(x=telefone, sigma.x=sqrt(0.20*(1-0.20)),
       y=internet, sigma.y=sqrt(0.19*(1-0.19)),
       alternative="greater", mu=0)

#atribuir um "nome" ao teste para aceder aos campos
(exemplo6 = z.test(x=telefone, sigma.x=sqrt(0.20*(1-0.20)),
                   y=internet, sigma.y=sqrt(0.19*(1-0.19)),
                   alternative="greater", mu=0))

# valor observado da estatística de teste sob H0
exemplo6$statistic

# Região crítica
# quantil de probabilidade da região critíca
qnorm(1-0.01)

# valor-p
exemplo6$p.value
#ou
1-pnorm(exemplo6$statistic)

