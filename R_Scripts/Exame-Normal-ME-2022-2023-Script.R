#########################################################
#  Exame Época Normal - MÉTODOS ESTATÍSTICOS 2022-2023  #
#########################################################


#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

cereal
str(cereal)

##############################################################
##############################################################

# 1 (a)

# tabela de frequências da variável shelf
(ni.prat <- table(cereal$shelf))  # frequências absolutas
(fi.prat <- round(prop.table(ni.prat),5))  # frequências relativas
(Ni.prat <- cumsum(ni.prat))  # frequências absolutas acumuladas
(Fi.prat <- cumsum(fi.prat)) # frequências relativas acumuladas

(tabela.freq.prat <- data.frame(i=1:nrow(ni.prat),
                               prateleira=c("1 = mais baixa", "2 = do meio", "3 = mais alta"),
                               ni=as.integer(ni.prat),
                               fi=as.numeric(fi.prat),
                               Ni=as.integer(Ni.prat),
                               Fi=as.numeric(Fi.prat)))
#ou
DescTools::Freq(as.factor(cereal$shelf))


#medidas de localização: Moda
names(ni.prat)[ni.prat==max(ni.prat)]
#ou
DescTools::Mode(as.factor(cereal$shelf))


##############################################################
##############################################################

# 1 (b)

#número de classes
(k<-7)
#amplitude de cada classe
(h<-3.5)
#variaçao dos classes
(valor.min <- min(cereal$carbo))
(valor.max <- valor.min+h*k)
#variação dos dados
range(cereal$carbo)
#limites dos intervalos
(cortes <- seq(valor.min, valor.max, by=h))

#histogrma da variável carbo
hist(cereal$carbo, breaks=cortes, include.lowest=TRUE, right=TRUE, 
     col=4, xlab="hidratos de carbono por dose do cereal (em gramas)",
     ylab="frequências absolutas", main="", ylim=c(0,35), xaxt="n")
axis(side = 1, at=c(0,cortes))

#comprovar o que se vê no gráfico e que foi escrito na resposta à questão sobre simetria
e1071::skewness(cereal$carbo, type=3)
# o valor é negativo mas próximo de zero


##############################################################
##############################################################

# 1 (c)

#consumidor Infantil
infantil <- cereal[cereal$client=="I",]
#quantidade de açúcar nos cereais de pequeno almoço destinados aos consumidores infantis
infantil$sugars
#dimensão da amostra
(nI <- length(infantil$sugars))


#consumidor Adulto
adulto <- cereal[cereal$client=="A",]
# quantidade de açúcar nos cereais de pequeno almoço destinados aos consumidores adultos
adulto$sugars
#dimensão da amostra
(nA <- length(adulto$sugars))

#Teste de hipóteses
(TH.1c <- BSDA::z.test(x=infantil$sugars, y=adulto$sugars, alternative="greater", mu= 0,
  sigma.x=sd(infantil$sugars), sigma.y=sd(adulto$sugars)))

# tomada de decisão pela região crítica
#estatística de teste observada sob a hipótese H0
TH.1c$statistic
#quantil de probabilidade para a RC
qnorm(1-0.04)


##############################################################
##############################################################

# 1 (d)

#cereais que estão na prateleira mais alta
prat3 <- cereal[cereal$shelf==3,]
#número de cereais na prateleira mais alta
(n3 <- nrow(prat3))

#cereais na prateleira mais alta destinados aos consumidores infantis
prat3.infantil <- prat3[prat3$client=="I",]
#número de cereais na prateleira mais alta destinados aos consumidores infantis
(n3I <- nrow(prat3.infantil))

#Intervalo de confiança
amostraI <- c(rep(1,n3I), rep(0, n3-n3I))
(est.p <- n3I/n3)
(TH.1d <- BSDA::z.test(x=amostraI, sigma.x=sqrt(est.p*(1-est.p)), conf.level=0.96))
TH.1d$conf.int


##############################################################
##############################################################

# 1 (e)

#tabela de contingência
(tabela.contingencia <- table(cereal$shelf, cereal$client))
addmargins(tabela.contingencia)

#teste de independência do Qui-Quadrado
(TH.1e <- chisq.test(tabela.contingencia))
# Ei = frequências Esperadas
TH.1e$expected  # respeita as condições do teste (todas as fequências são maiores que 5)

# valor-p
TH.1e$p.value


##############################################################
##############################################################

# 1 (f)

#analisar a correlação linear
#diagrama de dispersão
plot(x=cereal$calories, y=cereal$rating,
     xlab="calorias por dose do cereal (em quilocalorias)",
     ylab="pontuação",
     pch=20, col=2)
#coeficiente de correlação linear de Pearson
cor(x=cereal$calories, y=cereal$rating)

#reta de regressão linear simples
(modelo <- lm(rating~calories, data=cereal))

#previsão
predict(modelo, newdata=data.frame(calories=c(125)))
#ou
modelo$coefficients[1]+modelo$coefficients[2]*125

#intervalo de variação da variável independente
range(cereal$calories)


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (b)
x <- c(0, 1, 2, 3)
f_x <- c(0.25, 0.125, 0.5, 0.125)

(E_X <- sum(x*f_x))
(E_Y <- -3*E_X+2)
#alternativa:
(E_Y <- sum((-3*x + 2) * f_x))

##############################################################
##############################################################

# 2 (c)
1-ppois(9,12)

##############################################################
##############################################################

# 2 (d) (i)
qnorm(0.95, 50, 5)

##############################################################
##############################################################

# 2 (d) (ii)

(p<-1-pnorm(60,50,5))
dbinom(3,10,p)

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (a)
amostra.vento <- c(55, 61, 41, 42, 43, 28, 37, 40, 42, 48, 25)

(var.vento <- var(amostra.vento))
(valor <- (10*var.vento)/(16.5710^2))
pchisq(valor,10)


##############################################################
##############################################################

# 3 (b)
t.test(x=amostra.vento, alternative="less", mu=40)

qt(0.01,10)


