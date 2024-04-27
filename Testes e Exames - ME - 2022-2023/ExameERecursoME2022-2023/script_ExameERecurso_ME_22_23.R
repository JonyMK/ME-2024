#########################################################
#  Exame Época Recurso - MÉTODOS ESTATÍSTICOS 2022-2023 #
#########################################################


#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

lampadas
str(lampadas)

##############################################################
##############################################################

# 1 (a)

# consumo máximo
(M<-max(lampadas$Consumption))
lampadas[lampadas$Consumption == M, ]

# consumo mediano (fórmula dos slides: type=2)
(med<-median(lampadas$Consumption, type=2))
lampadas[lampadas$Consumption == med, ]


##############################################################
##############################################################

# 1 (b)

# dimensão da amostra
nrow(lampadas)

# mínimo e máximo dos dados
min(lampadas$Hour)
max(lampadas$Hour)

# 3 classes: [1,8[, [8,12[, [12,18[ e [18,24]
#extremos das classes
cortes <- c(1,8,12,18,24)

# intervalos fechados à esquerda e abertos à direita
# a última classe fechada dos dois lados
(classes <- cut(lampadas$Hour, breaks=cortes, right=FALSE, include.lowest=TRUE))

# tabela de frequências
(ni <- table(classes))               # frequências absolutas
(fi <- round(prop.table(ni),4))   # frequências relativas
(Ni <- cumsum(ni))                # frequências absolutas acumuladas
(Fi <- round(cumsum(fi),4))       # frequências relativas acumuladas

(tabela.frequencias.Hour <- data.frame(i=1:nrow(ni),
                                       classes=names(ni),
                                       ni=as.integer(ni),
                                       fi=as.numeric(fi),
                                       fi.ai = round(as.numeric(fi)/c(8-1,12-8,18-12,24-12),4),
                                       Ni=as.integer(Ni),
                                       Fi=as.numeric(Fi)))

# representação gráfica
# histograma -> eixo dos yy -> fi/h  -> as classes têm amplitudes diferentes
hist(lampadas$Hour, breaks=cortes, right=FALSE, include.lowest=TRUE,
     freq=FALSE,
     main="Histograma",
     xlab="tempo (em horas) que a lâmpada está acesa",
     ylab="frequências relativas / amplitudes das classes",
     col=7,
     xlim=c(0,30),
     ylim=c(0,0.07),
     xaxt="n")                  
axis(side=1, at=cortes)


##############################################################
##############################################################

# 1 (c)

boxplot(lampadas$Consumption ~ lampadas$Type, data=lampadas, col=c(4,"lightblue"), 
        main="Diagrama de extremos e quartis", horizontal=TRUE, xlab="Consumo de Energia(kWh)", 
        ylab="Tipo de lâmpada", type=2)
## OU 
Amostra.Led<-lampadas[lampadas$Type == "L", ]
Amostra.Fluorescente<-lampadas[lampadas$Type == "F", ]

# Diagrama de extremos e quartis = caixa com bigodes (fórmula dos slides: type=2)
boxplot(Amostra.Fluorescente$Consumption, Amostra.Led$Consumption, col=c(4,"lightblue"), 
        main="Diagrama de extremos e quartis", horizontal=TRUE, 
        xlab="Consumo Lampadas (em Kwh)", ylab="Tipo de Lãmpada", 
        names=c("Fluorescente","Led"), type=2)

#comprovar valores do gráfico e que são usados para os comentários

#Led
max(Amostra.Led$Consumption) # máximo
min(Amostra.Led$Consumption) # mínimo
max(Amostra.Led$Consumption)-min(Amostra.Led$Consumption)  # dispersão através da amplitude total
quantile(Amostra.Led$Consumption, prob=c(0.25,0.50, 0.75), type=2) #1º, 2º (mediana) e 3º quartis
IQR(Amostra.Led$Consumption, type=2) # amplitude dos 50% centrais ->  amplitude interquartil

# Fluorescente
max(Amostra.Fluorescente$Consumption) # máximo
min(Amostra.Fluorescente$Consumption) # mínimo
max(Amostra.Fluorescente$Consumption)-min(Amostra.Fluorescente$Consumption)  # dispersão através da amplitude total
quantile(Amostra.Fluorescente$Consumption, prob=c(0.25, 0.50, 0.75), type=2)  #1º, 2º (mediana) e 3º quartis
IQR(Amostra.Fluorescente$Consumption, type=2) # amplitude dos 50% centrais ->  amplitude interquartil


##############################################################
##############################################################

# 1 (d)

#Medida de Assimetria b1
e1071::skewness(lampadas$Consumption, type=3)
# como o valor de b1 é próximo de zero -> dados simétricos -> testar a distribuição Normal

# dimensão da amostra
nrow(lampadas)
# teste de ajustamento
nortest::lillie.test(lampadas$Consumption)


##############################################################
##############################################################

# 1 (e)

#dados
Amostra.Fluorescente$Illumination
Amostra.Led$Illumination
#dimensão das amostras
length(Amostra.Fluorescente$Illumination)
length(Amostra.Led$Illumination)

#teste de hipóteses
(TH.1e <- BSDA::z.test(x=Amostra.Fluorescente$Illumination, sigma.x=sd(Amostra.Fluorescente$Illumination), 
                      y=Amostra.Led$Illumination, sigma.y=sd(Amostra.Led$Illumination),
                      alternative="two.sided", mu=0))

#tomada de decisão pelo valor-p
TH.1e$p.value

#tomada de decisão pela região crítica
#quantil de probabilidade para a RC
qnorm(0.03/2)
qnorm(1-0.03/2)
#valor observado da estatística de teste sob a hipóteses H0
TH.1e$statistic


##############################################################
##############################################################

# 1 (f)

# diagrama de dispersão -> plot()
plot(x=lampadas$Illumination, y=lampadas$Consumption,
     pch=20, col="blue",
     xlab="Nível de Iluminação (em lux)", 
     ylab="Consumo de Energia (em kWh)",
     main="Diagrama de Dispersão")

# coeficiente de correlação linear de Pearson -> cor()
cor(x=lampadas$Illumination, y=lampadas$Consumption)


#modelo pretendido: y^=a+bx
# fórmula:  y~x
(modelo<-lm(Consumption~Illumination, data=lampadas))


#previsão
(prev <- predict(modelo, newdata=data.frame(Illumination=c(100))))
#ou
(prev <- modelo$coefficients[1]+modelo$coefficients[2]*100)

#intervalo de variação da variável independente
range(lampadas$Illumination)


# ver a reta de regressão e a previsão no diagrama de dispersão
plot(x=lampadas$Illumination, y=lampadas$Consumption,
     pch=20, col="blue",
     xlab="Nível de Iluminação (em lux)", 
     ylab="Consumo de Energia (em kWh)",
     main="Diagrama de Dispersão",
     xlim=c(90,1000),
     ylim=c(25,125))
abline(a= modelo$coefficients[1], b=modelo$coefficients[2], col="red")
points(x=100, y=prev, pch=19, col="green3") 
legend("topleft", 
       legend=c("pontos observados", "reta de regressão", "previsão"),
       col=c("blue", "red", "green3"), 
       pch=c(20,NA, 20), 
       lty=c(NA,1,NA), cex=0.8)


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (a)

# E[X]
f2 <- function(x){x*x}
f3 <- function(x){x*(2-x)}
(EX <- 0+integrate(f2, lower=0, upper=1)$value + integrate(f3, lower=1, upper=2)$value +0)

#V[1-2X]
((-2)^2)*((7/6)-EX^{2})


##############################################################
##############################################################

# 2 (b) (i) 

(p.sucesso <- 1-pnorm(26,25,3))
dbinom(4,8,p.sucesso)


##############################################################
##############################################################

# 2 (b) (ii)

pnorm((27-25)/(3/sqrt(15)))-pnorm((24-25)/(3/sqrt(15)))


##############################################################
##############################################################

# 2 (c)

1-pexp(5/6, rate=1/(1/3))


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 3

# 3 (a)

#Intervalo de confiança
amostraCB <- c(rep(1,26), rep(0, 40-26))
(est.p <- 26/40)
(TH.3a <- BSDA::z.test(x=amostraCB, sigma.x=sqrt(est.p*(1-est.p)), conf.level=0.90))
TH.3a$conf.int


##############################################################
##############################################################

# 3 (b)

# grau de confiança
1-2*(1-pnorm(0.1/(sqrt(est.p*(1-est.p))/sqrt(40))))


##############################################################
##############################################################

# 3 (c)

amostra.3c <- c(4.6, 4.3, 6.6, 4.7, 6.2, 4.2, 4.8, 3.9, 3.7, 4.6, 6.0, 5.8)
length(amostra.3c)  

##############################################################
##############################################################

# 3 (c) (i)

#estimativa pontual para a média
mean(amostra.3c)

#estimativa pontual para o desvio padrão
sd(amostra.3c)


##############################################################
##############################################################

# 3 (c) (ii)

#testar a distribuição Normal
shapiro.test(amostra.3c)

#teste de hipóteses para a variância
(TH.3cii <- EnvStats::varTest(x=amostra.3c, alternative="less", sigma.squared=1.5^{2}))
#valor observado da estatística de testes sob a hipóteses H0
TH.3cii$statistic

#quantil de probabilidade para a Região Crítica
qchisq(0.01,TH.3cii$parameters)

