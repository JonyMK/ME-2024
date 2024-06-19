##############
# CAPÍTULO 4 #
##############


###############################
###############################
###############################

#          Exemplo 1          #

###############################
###############################
###############################

amostra1 = c(11,15,13)


###############
#Exemplo 1.2

mean(amostra1)

###############################
###############################
###############################

#          Exemplo 2          #

###############################
###############################
###############################

amostra2 = c(5,7,4,2)


###############
#Exemplo 2.2

var(amostra2)


###############################
###############################
###############################

#          Exemplo 3          #

###############################
###############################
###############################

amostra3 = c(8.18, 8.16, 8.17, 8.22, 8.19)


###############
#Exemplo 3.1

mean(amostra3)


###############
#Exemplo 3.2

# recorrendo à função z.test
library(BSDA)
z.test(x=amostra3, sigma.x=0.08, conf.level=0.90)

#ou
exemplo32 = z.test(x=amostra3, sigma.x=0.08, conf.level=0.90)
round(exemplo32$conf.int,3)

###############
#Exemplo 3.4

# quantil de probabilidade da N(0,1)
# z0.95
qnorm(0.95)


###############
#Exemplo 3.5

pnorm(0.84)


###############################
###############################
###############################

#          Exemplo 4          #

###############################
###############################
###############################

###############
#Exemplo 4.1

# quantil de probabilidade da t(19)
# t0.975;19
qt(0.975,19)


###############
#Exemplo 4.2

# quantil de probabilidade da N(0,1)
# z0.975
qnorm(0.975)


###############################
###############################
###############################

#          Exemplo 5          #

###############################
###############################
###############################

amostra5A = c(36, 34, 34, 36, 35, 34, 33, 35, 33, 35)
(nA = length(amostra5A))

amostra5B = c(26, 37, 28, 26, 34, 28, 32, 27, 23, 28, 26, 36, 33, 30, 31, 27)
(nB = length(amostra5B))


###############
#Exemplo 5.1

# recorrendo à função t.test
t.test(x=amostra5A, y=amostra5B, paired=FALSE, var.equal=TRUE, conf.level=0.95)


###############
#Exemplo 5.2

# recorrendo à função var.test
var.test(x=amostra5A, y=amostra5B, conf.level=0.95)
#ou
exemplo52 = var.test(x=amostra5A, y=amostra5B, conf.level=0.95)
round(exemplo52$conf.int,4)


# ver a dispersão das amostras com recurso ao diagrama de extremos e quartis
golfe = data.frame(pontos=c(amostra5A,amostra5B),
                    jogadores=c(rep("golfA", nA), rep("golfB", nB)))

boxplot(pontos ~ jogadores, data=golfe, horizontal=TRUE)


###############
#Exemplo 5.3

# recorrendo à função t.test
t.test(x=amostra5A, y=amostra5B, paired=FALSE, var.equal=FALSE, conf.level=0.95)


###############################
###############################
###############################

#          Exemplo 6          #

###############################
###############################
###############################

amostra6.Sem = c(23, 35, 29, 33, 43, 32)
(nS = length(amostra6.Sem))

amostra6.Com = c(28, 38, 30, 37, 42, 30)
(nC = length(amostra6.Com))

# D = Com - Sem

# recorrendo à função t.test com as duas amostras
t.test(x=amostra6.Com, y=amostra6.Sem, paired=TRUE, conf.level=0.95)

#OU

# recorrendo à função t.test só com uma amostra (amostra das diferenças)
(amostra6.D = amostra6.Com-amostra6.Sem)
t.test(x=amostra6.D, conf.level=0.95)


###############################
###############################
###############################

#          Exemplo 7          #

###############################
###############################
###############################


###############
#Exemplo 7.1

# quantil de probabilidade do Qui-Quadadro(11)
qchisq(0.05,11)   
qchisq(0.95,11)   


###############
#Exemplo 7.2

amostra72 = c(14.06, 6.08, 9.76, 9.75, 15.45, 6.77,
               13.81, 6.28, 9.56, 10.75, 15.16, 5.57)
(n = length(amostra72))


# IC para a variância
# recorrendo à função varTest
library(EnvStats)
varTest(x=amostra72, conf.level=0.90)

# IC para o desvio padrão
exemplo72 = varTest(x=amostra72, conf.level=0.90)
round(sqrt(exemplo72$conf.int),4)


###############################
###############################
###############################

#          Exemplo 8          #

###############################
###############################
###############################

# quantil de probabilidade da F(6,5)
qf(0.005,6,5) 
qf(0.995,6,5)


###############################
###############################
###############################

#          Exemplo 9          #

###############################
###############################
###############################

# recorrendo à função z.test
# amostra: sucesso = 1 e insucesso = 0
(amostra9 = c(rep(1,24), rep(0,60-24)))
# proporção amostral
(estimativa.p = mean(amostra9))
# sigma.x para a função z.test
(s = sqrt(estimativa.p*(1-estimativa.p)))
# IC
library(BSDA)
z.test(x=amostra9, sigma.x=s, conf.level=0.96) 


###############################
###############################
###############################

#          Exemplo 10          #

###############################
###############################
###############################


# quantil de probabilidade da N(0,1)
# z(0.975)
qnorm(0.975)


###############################
###############################
###############################

#          Exemplo 11          #

###############################
###############################
###############################

# recorrendo à função z.test

# amostra 1: sucesso = 1 e insucesso = 0
(amostra.pop1 = c(rep(1,81), rep(0,270-81)))
# proporção amostral 1
(estimativa.p1 = mean(amostra.pop1))
# sigma.x para a função z.test
(s1 = sqrt(estimativa.p1*(1-estimativa.p1)))

# amostra 2: sucesso = 1 e insucesso = 0
(amostra.pop2 = c(rep(1,116), rep(0,290-116)))
# proporção amostral 2
(estimativa.p2 = mean(amostra.pop2))
# sigma.y para a função z.test
(s2 = sqrt(estimativa.p2*(1-estimativa.p2)))

# IC
library(BSDA)
z.test(x=amostra.pop1, sigma.x=s1, y=amostra.pop2, sigma.y=s2, conf.level=0.99) 


###############################
###############################
###############################

#          Exemplo 12          #

###############################
###############################
###############################

# dados
iris

# ver informação sobre os dados
?iris
str(iris)

###############
#Exemplo 12.1

# dimensão da amostra
length(iris$Sepal.Length)
# estimativa pontual para a média
mean(iris$Sepal.Length)
# estimativa pontual para o desvio padrão
sd(iris$Sepal.Length)

# calcular o intervalo de confiança
library(BSDA)
z.test(x=iris$Sepal.Length, sigma.x=sd(iris$Sepal.Length), conf.level=0.90)


###############
#Exemplo 12.2


# íris setosa
setosa = iris[iris$Species=="setosa",] 
# comprimentos da sépala das íris setosa
setosa$Sepal.Length
# dimensão da amostra dos comprimentos da sépala das íris setosa
length(setosa$Sepal.Length)
# estimativa pontual para a média dos comprimentos da sépala das íris setosa
mean(setosa$Sepal.Length)
# estimativa pontual para o desvio padrão dos comprimentos da sépala das íris setosa
sd(setosa$Sepal.Length)



# íris versicolor
versicolor = iris[iris$Species=="versicolor",] 
# comprimentos da sépala das íris versicolor
versicolor$Sepal.Length
# dimensão da amostra dos comprimentos da sépala das íris versicolor
length(versicolor$Sepal.Length)
# estimativa pontual para a média dos comprimentos da sépala das íris versicolor
mean(versicolor$Sepal.Length)
# estimativa pontual para o desvio padrão dos comprimentos da sépala das íris versicolor
sd(versicolor$Sepal.Length)


# calcular o intervalo de confiança
library(BSDA)
z.test(x=setosa$Sepal.Length, sigma.x=sd(setosa$Sepal.Length),
       y=versicolor$Sepal.Length, sigma.y=sd(versicolor$Sepal.Length),
       conf.level=0.90)

