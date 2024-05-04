################################################
#  1.º TESTE - MÉTODOS ESTATÍSTICOS 2022-2023  #
################################################


#EXERCÍCIO 1

# 1 (b)
# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

cafe
str(cafe)

##############################################################
##############################################################

# 1 (b) (i)

#dimensão da amostra
nrow(cafe)

##############################################################
##############################################################

# 1 (b) (ii)

# país que produziu mais
cafe[cafe$bags ==max(cafe$bags),1]
max(cafe$bags)*1000

# país melhor pago
cafe[cafe$price ==max(cafe$price),1]
max(cafe$price)


##############################################################
##############################################################

# 1 (b) (iii)

# tabela de frequências da variável month
# frequências absolutas
(ni.mes <- table(cafe$month))
# frequências relativas
(fi.mes <- round(prop.table(ni.mes),3))
# frequências absolutas acumuladas
(Ni.mes <- cumsum(ni.mes))
# frequências relativas acumuladas
(Fi.mes <- cumsum(fi.mes))

(tabela.freq.mes <- data.frame(i=1:nrow(ni.mes),
                                 xi=c("4 = abril", "6 = junho","10 = outubro"),
                                 ni=as.integer(ni.mes),
                                 fi=as.numeric(fi.mes),
                                 Ni=as.integer(Ni.mes),
                                 Fi=as.numeric(Fi.mes)))
#ou
DescTools::Freq(as.factor(cafe$month))

# representação gráfica
# gráfico de barras -> eixo dos yy fi
barplot(fi.mes, main="Mês da colheita", xlab="mês", ylab="frequências relativas",
     col=2:4, ylim=c(0,1))                  

# diagrama circular
pie(ni.mes, col=2:4, main="Mês da colheita",
    labels=paste(fi.mes*100, c("% = 4 = abril", "% = 6 = junho","% = 10 = outubro")))


##############################################################
##############################################################

# 1 (b) (iv)

boxplot(price~month, data=cafe, horizontal=TRUE, col=5:7, xlab="preço", ylab="mês")

#ou
abril <- cafe[cafe$month==4,]
junho <- cafe[cafe$month==6,]
outubro <- cafe[cafe$month==10,]

boxplot(abril$price, junho$price, outubro$price, horizontal=TRUE, col=5:7, names=c("abril","junho","outubro"),
        xlab="preço", ylab="mês")

#comprovar o que se vê no gráfico e que foi escrito na resposta à questão
# dispersão através da amplitude total
max(abril$price)-min(abril$price)
max(junho$price)-min(junho$price)
max(outubro$price)-min(outubro$price)  #maior

# dispersão através da amplitude interquartil
IQR(abril$price)
IQR(junho$price)  #maior
IQR(outubro$price)

#mediana
median(abril$price)
median(junho$price)
median(outubro$price) #menor

##############################################################
##############################################################

# 1 (b) (v)

# coeficiente de variação

# sacos de café de 60kg
(sd(cafe$bags)/mean(cafe$bags))*100

# preço pago aos produtores de café
(sd(cafe$price)/mean(cafe$price))*100

##############################################################
##############################################################

# 1 (b) (vi)

########
#classes

# Regra de Sturges
(n <- nrow(cafe))   # dimensão da amostra
(k <- trunc(1+log(n)/log(2)))  # número de classes
(h <- (max(cafe$price)-min(cafe$price))/k)  # amplitude de cada classe

#comece as classes com o mínimo dos dados e considere classes abertas à direita e fechadas à esquerda
(valor.min <- min(cafe$price))
(valor.max <- valor.min+k*h)
(cortes.preco <- seq(valor.min, valor.max, by=h))
(classes.preco <- cut(cafe$price, cortes.preco, right=FALSE, include.lowest=TRUE, dig.lab=4))

######################
#tabela de frequências

# frequências absolutas
(ni.preco <- table(classes.preco))
# frequências relativas
(fi.preco <- prop.table(ni.preco))
# frequências absolutas acumuladas
(Ni.preco <- cumsum(ni.preco))
# frequências relativas acumuladas
(Fi.preco <- cumsum(fi.preco))

(tabela.freq.preco <- data.frame(i=1:nrow(ni.preco),
                                 classes=names(ni.preco),
                                 ni=as.integer(ni.preco),
                                 fi=as.numeric(fi.preco),
                                 Ni=as.integer(Ni.preco),
                                 Fi=as.numeric(Fi.preco)))
#ou
DescTools::Freq(classes.preco)

######################
#histograma

hist(cafe$price, cortes.preco, right=FALSE, include.lowest=TRUE,
     xlab="preço",ylab="frequências absolutas",
     main="preço do café pago aos produtores", col="green", ylim=c(0,20),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,round(cortes.preco,2),200))  # definir os valores para o eixo dos xx igual às classes


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 d) (i)
dpois(20,12)

# 2 d) (ii)
1-pexp(6,1/(8/1.2))


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (a)
qnorm(0.05)

#3 b) (i)
(1-pnorm(2.5,2,sqrt(0.04)))/(1-pnorm(1,2,sqrt(0.04)))

#3 b) (ii)
#probabilidade de sucesso
pnorm(2,2,sqrt(0.04))
# probabilidade pretendida
1-pbinom(4,20,0.5)

