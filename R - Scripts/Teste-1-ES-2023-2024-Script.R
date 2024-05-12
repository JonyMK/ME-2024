#########################
#  1.º TESTE 2023-2024  #
#########################

#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

distonia
str(distonia)


##############################################################
##############################################################

# 1 (a)

# dimensão da amostra

nrow(distonia)


##############################################################
##############################################################

# 1 (b)

# tabela de frequências da variável sex

# frequências absolutas
(niG <- table(distonia$sex))
# frequências relativas em percentagem
fiG <- prop.table(niG)

data.frame(i=1:length(niG),
           xi=c("1 = Feminino", "2 = Masculino"),
           ni=as.integer(niG),
           fi=as.numeric(round(fiG,4)))

# medidas: moda
names(niG)[niG==max(niG)]

##############################################################
##############################################################

# 1 (c)

# extremos dos dados idade
# mínimo
min(distonia$age)
# máximo
max(distonia$age)

# número de classes
k = 3

# Histograma -> eixo dos yy -> fi/h  -> as classes têm amplitudes diferentes
hist(distonia$age, breaks=c(min(distonia$age),45,60,max(distonia$age)), right=FALSE, include.lowest=TRUE, col=4,
     xlab="Idade", ylab="frequências relativas / amplitude das classes", main="Histograma", ylim=c(0,0.03),
     xaxt="n")                  
axis(side=1, at=c(0,min(distonia$age),45,60,max(distonia$age)))


##############################################################
##############################################################

# 1 (d)

# diagrama de extremos e quartis

# com indicação de outliers a partir dos moderados
boxplot(twstrs~treat, data=distonia, col=c("green", "pink", "red"), ylab="pontuação atribuída aos sintomas",
        names=c("placebo", "5000 botox B", "10000 botox B"), xlab="tratamento", range=1.5)

# com indicação de outliers a partir dos severos
boxplot(twstrs~treat, data=distonia, col=c("green", "pink", "red"), ylab="pontuação atribuída aos sintomas",
        names=c("placebo", "5000 botox B", "10000 botox B"), xlab="tratamento", range=3)

# outliers
verOut <- boxplot(twstrs~treat, data=distonia, range=1.5)
verOut$out
# número de "ouliers"
length(verOut$out)

# mínimos, máximos e quartis dos sintomas por tratamento
#placebo
min(distonia[distonia$treat==1,"twstrs"])
max(distonia[distonia$treat==1,"twstrs"])
quantile(distonia[distonia$treat==1,"twstrs"], probs=c(0.25,0.50,0.75))
IQR(distonia[distonia$treat==1,"twstrs"])
#5 000 unidades de toxina botulínica B
min(distonia[distonia$treat==2,"twstrs"])
max(distonia[distonia$treat==2,"twstrs"])
quantile(distonia[distonia$treat==2,"twstrs"], probs=c(0.25,0.50,0.75))
IQR(distonia[distonia$treat==2,"twstrs"])
#10 000 unidades de toxina botulínica B
min(distonia[distonia$treat==3,"twstrs"])
max(distonia[distonia$treat==3,"twstrs"])
quantile(distonia[distonia$treat==3,"twstrs"], probs=c(0.25,0.50,0.75))
IQR(distonia[distonia$treat==3,"twstrs"])


##############################################################
##############################################################

# 1 (e)

# analisar a simetria através das medidas de localização:
# moda
(niI = table(distonia$age))
names(niI)[niI==max(niI)]
# mediana
median(distonia$age)
# média
mean(distonia$age)


# medida b1 de simetria
e1071::skewness(distonia$age)

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (d)
# calcular os integrais

f <- function(y){(1/40)-(y/3200)}

# 1 hora = 60 minutos
#P(Y>60)
integrate(f, lower=60, upper=80)$value + 0


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (a)
#parâmetro da distribuição de Poisson
5*2
#P(X=3)
dpois(3,5*2)

##############################################################
##############################################################

# 3 (b)

# considerar a unidade de medida os meses: 1 ano = 12 meses
#parâmetro da distribuição Exponencial
12/2
#P(T>6|T>=4) -> recorrer à propriedade de falta de memória
1-pexp(6-4,1/(12/2))

# OU

# considerar a unidade de medida os anos
#parâmetro da distribuição Exponencial
1/2
#P(T>6/12|T>=4/12) -> recorrer à propriedade de falta de memória
1-pexp((6/12)-(4/12),1/(1/2))

##############################################################
##############################################################

# 3 (c) i.

n = 7
#probabilidade de sucesso
(p = pnorm(280,300,20)) 
# probabilidade pretendida: P(W>=1)
1-pbinom(0,n,p)

##############################################################
##############################################################

# 3 (c) ii.

# P(Y<m)=0.05
# quantil de probabilidade da distribuição N(300,20)
qnorm(0.05,300,20)

##############################################################
##############################################################

# 3 (c) iii.

#P(V>250)=0.80
# quantil de probabilidade da distribuição N(0,1)
qnorm(1-0.80)
#cálculo do desvio padrão
(250-290)/qnorm(1-0.80)

