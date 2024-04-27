#############################################
#                                           #
#    Capítulo 1 - Estatística Descritiva    #
#                                           #
#############################################

############
# página 18

# escrever um vector (feminino, masculino) com o nome genero
genero <- c("feminino", "masculino")


# escrever uma tabela com o vector "genero" e o vector idade (20,18)
tabela <- data.frame(genero=c("feminino", "masculino"), idade=c(20,18))


# ver informação sobre "data.frame"
?data.frame
help("data.frame")


################################################################
################################################################
################################################################

############
# página 21

dados <- data.frame(i=c(1:10),
                    dieta=c("Sim","Sim","Não", "Sim", "Sim", "Não", "Sim", "Não", "Não", "Sim"),
                    intensidade=c("Moderada", "Elevada", "Baixa", "Moderada", "Elevada", "Baixa", 
                                  "Baixa", "Baixa", "Elevada", "Baixa"),
                    suplementos=c(3, 2, 5, 6, 6, 3, 4, 4, 7, 3),
                    ferro=c(14.3, 7.8, 27.0, 11.0, 9.9, 14.5, 15.4, 20.8, 10.5, 15.9))


# escrever um ficheiro .txt
write.table(dados, file="dados.txt", quote=FALSE, row.names=FALSE)

# escrever um ficheiro .csv
write.csv(dados, file = "dados.csv")


################################################################
################################################################
################################################################

############
# página 22

View(dados)   # ver a tabela toda
head(dados)   # ver as primeiras linhas da tabela
tail(dados)   # ver as últimas linhas da tabela

names(dados)  # ver os nomes das variáveis
str(dados)    # ver a estrutura da tabela

dim(dados)    # número de linhas e colunas da tabela
nrow(dados)   # número de linhas da tabela
ncol(dados)   # número de colunas da tabela


dados[1,2]       # elemento da linha 1 e coluna 2 
dados[1,]        # linha 1 
dados[,2]        # coluna 2
dados[,c(2,4)]   # coluna 2 e coluna 4
dados[,2:4]      # coluna 2, coluna 3 e coluna 4
dados[c(2,4),]   # linha 2 e linha 4
dados[2:4,]      # linha 2, linha 3 e coluna 4


################################################################
################################################################
################################################################

############
# página 23

dados$dieta                  # ver a variável "dieta"
dados[,c("dieta", "ferro")]  # ver as variáveis "dieta" e "ferro"


dados[dados$dieta=="Sim",]          # tabela com as linhas onde a dieta é "Sim"
dados[dados$suplementos>3,]         # tabela com as linhas onde suplementos são >3
dados[dados$intensidade!="Baixa",]  # tabela com as linhas onde a intensidade não é "Baixa"


################################################################
################################################################
################################################################

############
# página 24


dados$dieta        # cada variável da tabela é um vector
v <- c(15,14,7,8)  # escrever um vector


length(dados$dieta)
length(v)


dados$dieta[3]        # dado na posição 3 
dados$dieta[c(3,7)]   # dados nas posições 3 e 7 
dados$dieta[3:7]      # dados nas posições 3 a 7


################################################################
################################################################
################################################################

############
# página 26


dados2 <- data.frame(var1=2:5, var2=c(1,NA,6,12))
dados2

# ver se tem NA
is.na(dados2)
anyNA(dados2)


# usar a função soma
sum(dados2$var2)
sum(dados2$var2, na.rm=TRUE)


# retirar as linhas que têm pelo menos 1 NA
na.omit(dados2)

dados3 <- na.omit(dados2)
dados3
anyNA(dados3)
sum(dados3$var2)


################################################################
################################################################
################################################################

#######################
# página 34 e página 35

# variável estatística: dieta equilibrada

(ni.1 = table(dados$dieta)) # frequências absolutas
(fi.1 = prop.table(ni.1))   # frequências relativas

# tabelas de frequências
(tabela.frequencias.1 = data.frame(i=1:nrow(ni.1),
                                   xi=names(ni.1),
                                   ni=as.integer(ni.1),
                                   fi=as.numeric(fi.1)))


# variável estatística: intensidade dos treinos

# definir a ordem dos níveis da variável
dados$intensidade = factor(x=dados$intensidade, levels=c("Baixa", "Moderada", "Elevada"))

(ni.2 = table(dados$intensidade))    # frequências absolutas
(fi.2 = prop.table(ni.2))            # frequências relativas
(Ni.2 = cumsum(ni.2))                # frequências absolutas acumuladas
(Fi.2 = cumsum(fi.2))                # frequências relativas acumuladas

# tabelas de frequências
(tabela.frequencias.2 = data.frame(i=1:nrow(ni.2),
                                   xi=names(ni.2),
                                   ni=as.integer(ni.2),
                                   fi=as.numeric(fi.2),
                                   Ni=as.integer(Ni.2),
                                   Fi=as.numeric(Fi.2)))


# variável estatística: suplementos alimentares

(ni.3 = table(dados$suplementos))    # frequências absolutas
(fi.3 = prop.table(ni.3))            # frequências relativas
(Ni.3 = cumsum(ni.3))                # frequências absolutas acumuladas
(Fi.3 <- cumsum(fi.3))                # frequências relativas acumuladas

# tabelas de frequências
(tabela.frequencias.3 = data.frame(i=1:nrow(ni.3),
                                   xi=names(ni.3),
                                   ni=as.integer(ni.3),
                                   fi=as.numeric(fi.3),
                                   Ni=as.integer(Ni.3),
                                   Fi=as.numeric(Fi.3)))

################################################################
################################################################
################################################################

###########
# página 43


# variável estatística: nível de ferro

k.4 = 3   # número de classes
cortes.4 = c(0,15,20,30) #limites das classes
#transformar os dados em classes
(classes.4 = cut(x=dados$ferro, breaks=cortes.4, include.lowest=TRUE, right=FALSE))
#colocar na tabela de dados as classes
dados$classes1 = classes.4
View(dados)

(ni.4 = table(classes.4))            # frequências absolutas
(fi.4 = prop.table(ni.4))            # frequências relativas
(Ni.4 = cumsum(ni.4))                # frequências absolutas acumuladas
(Fi.4 = cumsum(fi.4))                # frequências relativas acumuladas

# tabelas de frequências
(tabela.frequencias.4 = data.frame(i=1:nrow(ni.4),
                                   classei=names(ni.4),
                                   ni=as.integer(ni.4),
                                   fi=as.numeric(fi.4),
                                   Ni=as.integer(Ni.4),
                                   Fi=as.numeric(Fi.4)))

################################################################
################################################################
################################################################

###########
# página 45


# variável estatística: nível de ferro

min(dados$ferro)   # mínimo dos dados
max(dados$ferro)   # máximo dos dados

k.5 = 3                        # número de classes
h.5 = 10                       # amplitude de cada classe
minC.5 = 5                     # mínimo da primeira classe
(maxC.5 = minC.5 + h.5*k.5)    # máximo da última classe

(cortes.5 = seq(minC.5, maxC.5, by=h.5)) #limites das classes
#transformar os dados em classes
(classes.5 = cut(x=dados$ferro, breaks=cortes.5, include.lowest=FALSE, right=TRUE))
#colocar na tabela de dados as classes
dados$classes2 = classes.5
View(dados)

(ni.5 = table(classes.5))            # frequências absolutas
(fi.5 = prop.table(ni.5))            # frequências relativas
(Ni.5 = cumsum(ni.5))                # frequências absolutas acumuladas
(Fi.5 = cumsum(fi.5))                # frequências relativas acumuladas

# tabelas de frequências
(tabela.frequencias.5 = data.frame(i=1:nrow(ni.5),
                                   classei=names(ni.5),
                                   ni=as.integer(ni.5),
                                   fi=as.numeric(fi.5),
                                   Ni=as.integer(Ni.5),
                                   Fi=as.numeric(Fi.5)))

################################################################
################################################################
################################################################

###########
# página 47


# variável estatística: nível de ferro

min(dados$ferro)   # mínimo dos dados
max(dados$ferro)   # máximo dos dados

(n = nrow(dados))  # dimensão da amostra

(k.6 = trunc(1+log(n)/log(2))) # número de classes
(h.6 = (max(dados$ferro)-min(dados$ferro))/k.6)      # amplitude de cada classe
(minC.6 = min(dados$ferro))                  # mínimo da primeira classe
(maxC.6 = minC.6 + h.6*k.6)    # máximo da última classe

(cortes.6 = seq(minC.6, maxC.6, by=h.6)) #limites das classes
#transformar os dados em classes
(classes.6 = cut(x=dados$ferro, breaks=cortes.6, include.lowest=TRUE, right=TRUE))
#colocar na tabela de dados as classes
dados$classes3 = classes.6
View(dados)

(ni.6 = table(classes.6))            # frequências absolutas
(fi.6 = prop.table(ni.6))            # frequências relativas
(Ni.6 = cumsum(ni.6))                # frequências absolutas acumuladas
(Fi.6 = cumsum(fi.6))                # frequências relativas acumuladas

# tabelas de frequências
(tabela.frequencias.6 <- data.frame(i=1:nrow(ni.6),
                                    classei=names(ni.6),
                                    ni=as.integer(ni.6),
                                    fi=as.numeric(fi.6),
                                    Ni=as.integer(Ni.6),
                                    Fi=as.numeric(Fi.6)))


################################################################
################################################################
################################################################

###########
# página 52


# variável estatística: dieta equilibrada
# gráfico de barras
barplot(ni.1, col=2:3, xlab="Dieta equilibrada", ylab="Frequências absolutas")


# variável estatística: intensidade dos treinos
# gráfico de barras
barplot(fi.2, col=c("blue","pink","yellow"), xlab="Intensidade dos treinos", ylab="Frequências relativas")


# variável estatística: suplementos alimentares
# gráfico de barras
barplot(fi.3, col=rainbow(6), xlab="Frequências relativas", ylab="suplementos alimentares", horiz=TRUE)


################################################################
################################################################
################################################################

###########
# página 53

dados1 = c(2/30,6/30,1/30,9/30,5/30,7/30)
dados2 = c(5/50, 10/50, 9/50, 14/50, 5/50, 7/50)
nomes12 = c("Andebol", "Atletismo", "Basquetebol", "Futebol", "Ténis", "Voleibol")

barplot(rbind(dados1, dados2),
        beside = TRUE,
        names.arg = nomes12,
        legend.text = c("Lisboa", "Porto"),
        col = c("blue", "red"),
        ylim=c(0,0.35))


################################################################
################################################################
################################################################

# variável estatística: dieta equilibrada
# gráfico circular
pie(ni.1, labels=paste(ni.1, "= freq. Absoluta"),  col=2:3, main="Dieta equilibrada")
legend("bottomright", legend=names(ni.1), fill=2:3)


# variável estatística: intensidade dos treinos
# gráfico circular
pie(ni.2, labels=paste(fi.2*100, "%"),  col=c("blue","pink","yellow"), main="Intensidade dos treinos")
legend("topright", legend=names(ni.2), fill=c("blue","pink","yellow"))


# variável estatística: suplementos alimentares
pie(ni.3, labels=paste(fi.3*100, "%"),  col=rainbow(6), main="Número de suplementos alimentares")
legend("topright", legend=2:7, fill=rainbow(6))


################################################################
################################################################
################################################################

###########
# página 54

dados3 = c(19,16,5)
nomes3 = c("A", "B", "C")

pie(dados3, labels=paste(dados3/sum(dados3)*100, "%"),  col=c(rgb(1,0,0),rgb(1,0.8,0.4),rgb(1,0.6,0.4)))
legend("topright", legend=nomes3, fill=c(rgb(1,0,0),rgb(1,0.8,0.4),rgb(1,0.6,0.4)))

################################################################
################################################################
################################################################

###########
# página 55

dados4 = c(10,7,8)
nomes4 = c("vitória", "empates", "derrota")

pie(dados4, labels=paste(dados4/25*100, "%"),  col=c("blue","red","green"))
legend("topright", legend=nomes4, fill=c("blue","red","green"))


################################################################
################################################################
################################################################

###########
# página 58

# variável estatística: nível de ferro
# histograma

k.7 = 5     # número de classes
h.7 = 5     # amplitude de cada classe
minC.7 = 5  # mínimo da primeira classe
(maxC.7 = minC.7 + h.7*k.7)    # máximo da última classe
(cortes.7 = seq(minC.7, maxC.7, by=h.7)) #limites das classes

hist(dados$ferro, breaks=cortes.7, include.lowest=FALSE, right=TRUE, 
     freq=TRUE,
     main="",
     xlab="Nível de ferro",
     ylab="frequências absolutas",
     col=2,
     xlim=c(0,30))


################################################################
################################################################
################################################################

###########
# página 60


# variável estatística: nível de ferro
# histograma

k.4       # número de classes
cortes.4  # limites das classes
classes.4 # classes definidas

# classes com diferentes amplitudes -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes

hist(dados$ferro, breaks=cortes.4, include.lowest=TRUE, right=FALSE, 
     freq=FALSE,
     main="",
     xlab="Nível de ferro",
     ylab="frequências relativas / amplitude das classes = Freq. rel. / mg",
     col=2,
     xlim=c(0,30))

#ou

hist(dados$ferro, breaks=cortes.4, include.lowest=TRUE, right=FALSE, 
     freq=FALSE,
     main="",
     xlab="Nível de ferro",
     ylab="frequências relativas / amplitude das classes = Freq. rel. / mg",
     col=2,
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=cortes.4)  # definir os valores para o eixo dos xx igual às classes


################################################################
################################################################
################################################################

# Medidas de Localização

# número de suplementos alimentares
# Moda
names(ni.3)[ni.3==max(ni.3)]
# Média
mean(dados$suplementos)
# Mediana
median(dados$suplementos)
# Quartis
quantile(dados$suplementos, probs=c(0.25,0.50,0.75))


################################################################
################################################################
################################################################

# Diagrama de Extremos e Quartis

# nível de ferro
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=0) # sem indicação de outliers
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=1.5) # indicação de outliers a partir dos moderados
boxplot(dados$ferro, col=4, xlab="nível de ferro", 
        horizontal=TRUE, range=3) # indicação de outliers a partir dos severos
# só há outliers moderados

verOut <- boxplot(dados$ferro, col=4, xlab="nível de ferro", 
                  horizontal=TRUE, range=1.5)
verOut$out  #outlier moderado

################################################################
################################################################
################################################################

#comparar o nível de ferro de quem tem uma dieta equilibrada e quem não tem 

boxplot(ferro~dieta, data= dados, col=c("red","green"), xlab="dieta equilibrada", ylab="nível de ferro")


################################################################
################################################################
################################################################

# Medidas de dispersão absoluta

# número de suplementos alimentares
# amplitude total
max(dados$suplementos)-min(dados$suplementos)
# amplitude interquartil
IQR(dados$suplementos)
# variância
var(dados$suplementos)
# desvio padrão
sd(dados$suplementos)

################################################################
################################################################
################################################################

# Medida de dispersão relativa
# coeficiente de variação

# número de suplementos alimentares
(sd(dados$suplementos)/mean(dados$suplementos))*100


################################################################
################################################################
################################################################

# Medida de simetria

#########
# b1

library(e1071)

# número de suplementos alimentares
skewness(dados$suplementos)
