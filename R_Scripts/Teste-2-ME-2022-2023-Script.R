################################################
#  2.º TESTE - MÉTODOS ESTATÍSTICOS 2022-2023  #
################################################


#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Dataset -> From Text

profTI
str(profTI)

##############################################################
##############################################################

# 1 (a)
#Estimativa para p
# como os dados são zeros e uns basta calcular a média
(estimativa.p <- mean(profTI$Be))
#OU
#dimensão da amostra
(n <- length(profTI$Be))
#número de sucessos = número de profissionais de TI que recebe benefícios extra
(nsuc <- nrow(profTI[profTI$Be==1,]))
#Estimativa para p
(estimativa.p <- nsuc/n)

# percentagem
estimativa.p *100


# IC para p
(s <- sqrt(estimativa.p*(1-estimativa.p)))  #cálculo do desvio padrão
(IC.p <- BSDA::z.test(x=profTI$Be, sigma.x=s, conf.level=0.96))
round(IC.p$conf.int,4)


##############################################################
##############################################################

# 1 (b)
## amostra de profissionais TI juniores
juniores <- profTI[profTI$Exp == "J", ]
# dimensão da amostra
nrow(juniores)

# como n>30 poderiamos recorrer ao z.test
(IC.1b <- BSDA::z.test(x=juniores$In, sigma.x=sd(juniores$In), conf.level=0.99))
IC.1b$conf.int


### Alternativa: verificar Normalidade 
shapiro.test(juniores$In)
# como não se rejeita a H0: X~Normal, pode-se usar t.test
(IC.1b2 <- t.test(x=juniores$In, conf.level=0.99))
IC.1b2$conf.int

##############################################################
##############################################################

# 1 (c)
#dimensão da amostra
length(profTI$Salario)

#TH para mu>1439 - unilateral direito considerando n>=30 Pop Qualquer e desvio padrão desconhecido
(TH.1c <- BSDA::z.test(x=profTI$Salario, sigma.x=sd(profTI$Salario), alternative="greater", mu=1439))
# valor-p
TH.1c$p.value
#OU
# tomada de decisão pela região crítica
#valor observado da estatística de teste
TH.1c$statistic
# quantil de probabilidade para a RC
qnorm(0.99)


### Alternativa: verificar Normalidade 
nortest::lillie.test(profTI$Salario)

# como não se rejeita a H0: X~Normal, pode-se usar t.test
(TH.1c2 <- t.test(x=profTI$Salario, alternative="greater", mu=1439))
TH.1c2$p.value
# valor-p
TH.1c2$p.value
#OU
# tomada de decisão pela região crítica
#valor observado da estatística de teste
TH.1c2$statistic
# quantil de probabilidade para a RC
qt(0.99,113)

##############################################################
##############################################################

# 1 (d)
#selecionar dados: Seniores feminino
seniores.f <- profTI[profTI$Genero == 1 & profTI$Exp == "S",]
# dimensão da amostra
nrow(seniores.f)

#normalidade
shapiro.test(seniores.f$In)
shapiro.test(seniores.f$Salario)
#OU
#amostra D = salario atual-salario inicial
D<-seniores.f$Salario-seniores.f$In
shapiro.test(D)

#Teste de Wilcoxon
(TH.1d <- wilcox.test(x=seniores.f$Salario, y=seniores.f$In, alternative = "two.sided", mu=0, paired=TRUE))
TH.1d$p.value

##############################################################
##############################################################

# 1 (e)
#construir a tabela de contingência
(tabela.contingencia <- table(profTI$Genero, profTI$Be))
addmargins(tabela.contingencia)

(TH.1e <- chisq.test(tabela.contingencia, correct = FALSE))
# Ei = frequências Esperadas
TH.1e$expected  # respeita as condições do teste (todas as fequências são maiores que 5)

# tomada de decisão pelo valor-p
# valor-p
TH.1e$p.value
#OU
# tomada de decisão pela região crítica
#valor observado da estatística de teste
TH.1e$statistic
# quantil de probabilidade para a RC
qchisq(0.99,1)


# como existe independencia, não faz sentido medir a associação


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (a)

# amostra
(avariaMU <- c(rep(1,30), rep(0,200-30)))

# proporção amostral
30/200
#ou
mean(avariaMU)

# teste de hipóteses para a proporção
(testep <- BSDA::z.test(x=avariaMU, alternative="less", mu=0.16, sigma.x=sqrt(0.16*(1-0.16))))

# valor observado da estatística de teste
testep$statistic

# quantil de probabilidade da distribuição Normal para a região crítica
qnorm(0.02)


##############################################################
##############################################################

# 2 (b)

#nível de significância
(alfa <- 1-0.90)
#quantil de probabilidade da distribuição Normal reduzida
quantilp <- qnorm(1-alfa/2)
#cálculo do valor de n
((2*quantilp*0.4)/0.2)^2


##############################################################
##############################################################

# 2 (c)

#amostras

xx<-c(7.5,7.6,7.7,7.8,7.9,8.0,8.1,8.3)
(nx <- length(xx))

yy<-c(7.1,7.3,7.7,8,8.3,8.6)
(ny <- length(yy))


##############################################################
##############################################################

# 2 (c) (i)

# teste de ajustamento
shapiro.test(xx)
shapiro.test(yy)


##############################################################
##############################################################

# 2 (c) (ii)

#variâncias
(vx <- var(xx))
(vy <- var(yy))

# usando o limite superior do intervalo
# probabilidade da F-Snedecor
probf <- pf(1/((0.94058^2)/(vx/vy)),nx-1,ny-1)
#grau de confiança
1-2*probf

#ou

# usando o limite inferior do intervalo
# probabilidade da F-Snedecor
probf <- pf(1/((0.20346^2)/(vx/vy)),nx-1,ny-1)
#grau de confiança
1-2*(1-probf)


# confirmação do resultado obtido
IC <- var.test(x=xx, y=yy, conf.level=0.91)
round(sqrt(IC$conf.int),5)


##############################################################
##############################################################

# 2 (c) (iii)

# margem de erro
(0.94058-0.20346)/2


##############################################################
##############################################################

# 2 (c) (iv)

# Teste de hipóteses para a diferença médias
(testedif <- t.test(x=xx, y=yy, alternative="greater", mu=30/60, paired=FALSE, var.equal=FALSE))

# valor-p
testedif$p.value

# graus de liberdade da distribuição t-Student
testedif$parameter

# valor observado da estatística de teste
testedif$statistic

# quantil de probabilidade para a região crítica
qt(1-0.10,testedif$parameter)
