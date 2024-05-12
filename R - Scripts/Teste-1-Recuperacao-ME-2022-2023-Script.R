##############################################################
#  1.º TESTE (recuperação) - MÉTODOS ESTATÍSTICOS 2022-2023  #
##############################################################


#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

cereal1

##############################################################
##############################################################

# 1 (a)
str(cereal1)


##############################################################
##############################################################

# 1 (b)

#dimensão da população
(60*100)/30

#dimensão da amostra
nrow(cereal1)

##############################################################
##############################################################

# 1 (c)


# tabela de frequências da variável shelf
(ni.prat <- table(cereal1$shelf))  # frequências absolutas
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
DescTools::Freq(as.factor(cereal1$shelf))

########
#medidas de localização: Moda
names(ni.prat)[ni.prat==max(ni.prat)]
#ou
DescTools::Mode(as.factor(cereal1$shelf))

##############################################################
##############################################################

# 1 (d)

#Diagrama de extremos e quartis com indicação de outliers (moderados e severos)
(modsev <- boxplot(cereal1$calories, range=1.5, horizontal=TRUE, col=7, 
                   xlab="calorias por dose do cereal (em quilocalorias)"))

#outliers
modsev$out
# número de outliers
length(modsev$out)
# número de outliers pelo limite inferior
sum(modsev$out<modsev$stats[1])
# número de outliers pelo limite superior
sum(modsev$out>modsev$stats[4])


#Diagrama de extremos e quartis só com a indicação de outliers severos
sev <- boxplot(cereal1$calories, range=3, horizontal=TRUE, col=7, 
                xlab="calorias por dose do cereal (em quilocalorias)")
#outliers severos
sev$out
# número de outliers severos
length(sev$out)
# número de outliers severos pelo limite inferior
sum(sev$out<sev$stats[1])
# número de outliers severos pelo limite superior
sum(sev$out>sev$stats[4])

# número de outliers moderados
length(modsev$out)-length(sev$out)
# número de outliers moderados pelo limite inferior
sum(modsev$out<modsev$stats[1])-sum(sev$out<sev$stats[1])
# número de outliers severos pelo limite superior
sum(modsev$out>modsev$stats[4])-sum(sev$out>sev$stats[4])


##############################################################
##############################################################

# 1 (e)

#número de classes
(k<-7)
#amplitude de cada classe
(h<-3.5)
#variaçao dos classes
(valor.min <- min(cereal1$carbo))
(valor.max <- valor.min+h*k)
#variação dos dados
range(cereal1$carbo)
#limites dos intervalos
(cortes <- seq(valor.min, valor.max, by=h))

#histogrma da variável carbo
hist(cereal1$carbo, breaks=cortes, include.lowest=TRUE, right=TRUE, 
     col=4, xlab="hidratos de carbono por dose do cereal (em gramas)",
     ylab="frequências absolutas", main="", ylim=c(0,35), xaxt="n")
axis(side = 1, at=c(0,cortes))

#comprovar o que se vê no gráfico e que foi escrito na resposta à questão sobre simetria
e1071::skewness(cereal1$carbo, type=3)
# o valor é negativo mas próximo de zero


##############################################################
##############################################################

# 1 (f)

# coeficiente de variação

# calorias
(sd(cereal1$calories)/mean(cereal1$calories))*100

# hidratos de carbono
(sd(cereal1$carbo)/mean(cereal1$carbo))*100


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

(E_X <-sum(x*f_x))
(E_X_quad <-sum(x^2*f_x))

(Var_X<-E_X_quad-E_X^2)


##############################################################
##############################################################

# 2 (c)


##############################################################
##############################################################

# 2 (d) (i)
1-pexp(40,1/15)

##############################################################
##############################################################

# 2 (d) (ii)
1-ppois(9,12)

##############################################################
##############################################################

# 2 (e) (i)
qnorm(0.95, 50, 5)

##############################################################
##############################################################

# 2 (e) (ii)
(p<-1-pnorm(60,50,5))
dbinom(3,10,p)


