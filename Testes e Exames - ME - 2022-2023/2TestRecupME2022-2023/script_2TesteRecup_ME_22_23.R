##############################################################
#  2.º TESTE (recuperação) - MÉTODOS ESTATÍSTICOS 2022-2023  #
##############################################################


#EXERCÍCIO 1

#amostras
amostraLocalizacaoA <- c(55,61,41,42,43,28,37,40,42,48,25)
amostraLocalizacaoB <- c(69,23,41,47,38,21,65,42,39,43,36,29,26)

#dimensão das amostras
(nA <- length(amostraLocalizacaoA))
(nB <- length(amostraLocalizacaoB))


##############################################################
##############################################################

# 1 (a)
shapiro.test(amostraLocalizacaoA)
shapiro.test(amostraLocalizacaoB)


##############################################################
##############################################################

# 1 (b)
#estimativa pontual média
mean(amostraLocalizacaoA)
mean(amostraLocalizacaoB)
#estimativa pontual variância
var(amostraLocalizacaoA)
var(amostraLocalizacaoB)

##############################################################
##############################################################

# 1 (c)

(valor <- ((nA-1)*var(amostraLocalizacaoA))/(16.5710^2))

#quantil de probabilidade
pchisq(valor,nA-1)


##############################################################
##############################################################

# 1 (d)

t.test(x=amostraLocalizacaoA, alternative = "two.sided",mu=40)

#quantil para a RC
qt(0.005,nA-1)
qt(0.995,nA-1)

##############################################################
##############################################################

# 1 (e)
var.test(x=amostraLocalizacaoA, y=amostraLocalizacaoB, conf.level=0.90)


##############################################################
##############################################################

# 1 (f)
#teste de hipóteses paramétrico para a diferença de médias
t.test(x=amostraLocalizacaoA, y=amostraLocalizacaoB, alternative="less", mu = 0,
       paired=FALSE, var.equal=TRUE)


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

cereal2
str(cereal2)

##############################################################
##############################################################

# 2 (a)

(TH.2a <- ks.test(cereal2$sugars, "pexp", rate=1/8))
#valor-p
TH.2a$p.value


##############################################################
##############################################################

# 2 (b)

#consumidor Infantil
infantil <- cereal2[cereal2$client=="I",]
#quantidade de açúcar nos cereais de pequeno almoço destinados aos consumidores infantis
infantil$sugars

#consumidor Adulto
adulto <- cereal2[cereal2$client=="A",]
# quantidade de açúcar nos cereais de pequeno almoço destinados aos consumidores adultos
adulto$sugars

#Teste de Mann-Whitney
(TH.1b <- wilcox.test(x=infantil$sugars, y=adulto$sugars, alternative="greater", mu=0, paired=FALSE))
#valor-p
TH.1b$p.value


##############################################################
##############################################################

# 2 (c)

#dimensão da amostra
(n <- nrow(cereal2))

#cereais que estão na prateleira mais alta
prat3 <- cereal2[cereal2$shelf==3,]
#número de cereais na prateleira mais alta
(n3 <- nrow(prat3))

#estimativa pontual para a proporção de cereais que estão na prateleira mais alta
(est.prop <- n3/n)


#OU
#frequências relativas
prop.table(table(cereal2$shelf))
#estimativa pontual para a proporção de cereais que estão na prateleira mais alta
(est.prop <- prop.table(table(cereal2$shelf))[3])


##############################################################
##############################################################

# 2 (d)

#amostra do hipermercado em estudo
amostra1 <- c(rep(1,n3), rep(0, n-n3))
#proporção amostral
(p1 <- est.prop)

#amostra do outro hipermercado
amostra2 <- c(rep(1,34), rep(0, 96-34))
(p2 <- mean(amostra2))

#intervalo de confiança
(IC.2d <- BSDA::z.test(x=amostra1, y=amostra2, sigma.x=sqrt(p1*(1-p1)), sigma.y=sqrt(p2*(1-p2)),
             conf.level = 0.97))
IC.2d$conf.int


##############################################################
##############################################################

# 2 (e)

#cereais da prateleira mais alta
prat3
#destinados aos consumidores infantis
prat3.infantil <- prat3[prat3$client=="I",]

#número de cereais na prateleira mais alta destinados aos consumidores infantis
(n3I <- nrow(prat3.infantil))

#teste de hipóteses
amostraI <- c(rep(1,n3I), rep(0, n3-n3I))
(TH.2e <- BSDA::z.test(x=amostraI, alternative="less", mu=0.10, sigma.x=sqrt(0.10*(1-0.10))))

# tomada de decisão pelo valor-p
#valor-p
TH.2e$p.value

# tomada de decisão pela região crítica
#estatística de teste observada sob a hipótese H0
TH.2e$statistic
#quantil de probabilidade para a RC
qnorm(0.04)


##############################################################
##############################################################

# 2 (f)

#tabela de contingência
(tabela.contingencia <- table(cereal2$shelf, cereal2$client))
addmargins(tabela.contingencia)

#teste de independência do Qui-Quadrado
(TH.2f <- chisq.test(tabela.contingencia))
# Ei = frequências Esperadas
TH.2f$expected  # respeita as condições do teste (todas as fequências são maiores que 5)


# tomada de decisão pelo valor-p
# valor-p
TH.2f$p.value
#OU
# tomada de decisão pela região crítica
#valor observado da estatística de teste sob a hipótese H0
TH.2f$statistic
#graus de liberdade
(gl <- TH.2f$parameter)
# quantil de probabilidade para a RC
qchisq(1-0.06,gl)


#medidas de associação
library(DescTools)
ContCoef(tabela.contingencia)
CramerV(tabela.contingencia)

