#######################################
#  1.º TESTE (recuperação) 2023-2024  #
#######################################

#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

ProtesesMI_T1
str(ProtesesMI_T1)


##############################################################
##############################################################

# 1 (a)

# dimensão da amostra

(n = nrow(ProtesesMI_T1))


##############################################################
##############################################################

# 1 (b)

# tabela de frequências da variável satisfacao

# frequências absolutas
(niS <- table(ProtesesMI_T1$satisfacao))
# frequências relativas em percentagem
fiS <- prop.table(niS)
# frequências absolutas acumuladas
(NiS = cumsum(niS))
# frequências relativas acumuladas
(FiS = cumsum(fiS))

data.frame(i=1:length(niS),
           xi=names(niS),
           ni=as.integer(niS),
           fi=as.numeric(round(fiS,4)),
           Ni=as.integer(NiS),
           Fi=as.numeric(round(FiS,4)))


##############################################################
##############################################################

# 1 (c)

quantile(ProtesesMI_T1$satisfacao, 0.30)

##############################################################
##############################################################

# 1 (d)

# extremos dos dados tempo
# mínimo
(m = min(ProtesesMI_T1$tempo))
# máximo
(M = max(ProtesesMI_T1$tempo))

# Regra de Sturges
# número de classes
(k = round(1+log(n)/log(2),0))
# amplitude das classes
(h = (M-m)/k)
# menor valor da primeira classse
(mvalor = m)
# maior valor da última classse
(Mvalor = mvalor+h*k)

# Histograma
hist(ProtesesMI_T1$tempo, breaks=seq(mvalor, Mvalor, by=h), 
     right=FALSE, include.lowest=TRUE, col=5,
     xlab="tempo de uso da prótese (em meses)", 
     ylab="frequências absolutas", main="Histograma", xaxt="n")                  
axis(side=1, at=c(0,round(seq(mvalor, Mvalor, by=h),1)))


##############################################################
##############################################################

# 1 (e)

# diagrama de extremos e quartis

# com indicação de outliers a partir dos moderados
boxplot(idade~protese, data=ProtesesMI_T1, col=5:7, ylab="tipo de prótese",
        names=c("Robótica", "Mecânica", "Híbrida"), xlab="idade dos pacientes", 
        range=1.5, horizontal=TRUE)

# mínimos, máximos, quartis e simetria da idade dos pacientes por tipo de prótese
# Robótica = 1
min(ProtesesMI_T1[ProtesesMI_T1$protese==1,"idade"])
max(ProtesesMI_T1[ProtesesMI_T1$protese==1,"idade"])
quantile(ProtesesMI_T1[ProtesesMI_T1$protese==1,"idade"], probs=c(0.25,0.50,0.75))
IQR(ProtesesMI_T1[ProtesesMI_T1$protese==1,"idade"])
e1071::skewness(ProtesesMI_T1[ProtesesMI_T1$protese==1,"idade"])

# Mecânica = 2
min(ProtesesMI_T1[ProtesesMI_T1$protese==2,"idade"])
max(ProtesesMI_T1[ProtesesMI_T1$protese==2,"idade"])
quantile(ProtesesMI_T1[ProtesesMI_T1$protese==2,"idade"], probs=c(0.25,0.50,0.75))
IQR(ProtesesMI_T1[ProtesesMI_T1$protese==2,"idade"])
e1071::skewness(ProtesesMI_T1[ProtesesMI_T1$protese==2,"idade"])

# Híbrida = 3
min(ProtesesMI_T1[ProtesesMI_T1$protese==3,"idade"])
max(ProtesesMI_T1[ProtesesMI_T1$protese==3,"idade"])
quantile(ProtesesMI_T1[ProtesesMI_T1$protese==3,"idade"], probs=c(0.25,0.50,0.75))
IQR(ProtesesMI_T1[ProtesesMI_T1$protese==3,"idade"])
e1071::skewness(ProtesesMI_T1[ProtesesMI_T1$protese==3,"idade"])


##############################################################
##############################################################

# 1 (f)

# coeficiente de variação da idade
(sd(ProtesesMI_T1$idade)/mean(ProtesesMI_T1$idade))*100

# coeficiente de variação do tempo de uso da prótese
(sd(ProtesesMI_T1$tempo)/mean(ProtesesMI_T1$tempo))*100


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2


# 2 (b)

(EX2 = (1^2)*0.1+(3^2)*0.1+(4^2)*0.2+(5^2)*0.2+(7^2)*0.4)
((2/3)^2)*(EX2-5^2)

##############################################################
##############################################################

# 2 (c)

# calcular os integrais

f <- function(y){2*y^(-3)}

# 250 horas = 2.5 centenas de horas
#P(Y<=2.5)
0 + integrate(f, lower=1, upper=2.5)$value

# valor esperado
10000*(1-0.84)


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (a)
#1-P(X<=5)=1-F(5)
1-ppois(5,2)

##############################################################
##############################################################

# 3 (b)

qpois(0.99,2)

# com s=6 a probabilidade ruptura é inferior a 1%
1-ppois(6,2)
# com s=7 a probabilidade ruptura é inferior a 1%
1-ppois(7,2)
# com s=8 a probabilidade ruptura é inferior a 1%
1-ppois(8,2)
#...

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 4

# 4 (a) i.

# F(8)
pnorm(8,7,2)
# F(5)
pnorm(5,7,2)

# probabilidade condicional
(pnorm(8,7,2)-pnorm(5,7,2))/(1-pnorm(5,7,2))

##############################################################
##############################################################

# 4 (a) ii.

# probabilidade de sucesso 
(p = 1-(pnorm(8,7,2)-pnorm(5,7,2)))

# P(W<=9)
pbinom(9,25,p)

# probabilidade pretendida: P(W>=10)
1-pbinom(9,25,p)

##############################################################
##############################################################

# 4 (b)

#quantil de probabilidade
qnorm(0.45)

# valor esperado
6-1.1*qnorm(0.45)
