#######################
# FICHA DE TRABALHO 3 #
#######################


# EXERCÍCIO 1

# 1 (a)

(amostra = c(rep("engenheiro",32), 
              rep("professor",20), 
              rep("analistas de dados",16), 
              rep("aluno",12)))


##############################################################
# 1 (b)

#######################
# tabela de frequências

(ni = table(amostra))  # frequências absolutas
(fi = prop.table(ni))  # frequências relativas

(tab.freq = data.frame(i=1:nrow(ni),       # número da linha
                        xi =names(ni),      # níveis da variável
                        ni=as.integer(ni),  # frequências absolutas
                        fi=as.numeric(fi)))  # frequências relativas

#######################
# representação gráfica

# gráfico circular
pie(ni, labels=paste(fi*100, "%"), col=c("red", "yellow", "blue", "green"))
legend("topright", legend=names(ni), fill=c("red", "yellow", "blue", "green"))


#######################
# medidas

if( min(ni)==max(ni) ){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni)[ni==max(ni)]))
}



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# EXERCÍCIO 2

#dados
#ler os dados do ficheiro: File -> Import Datset -> From EXCEL
obesidade


##############################################################
# 2 (a)

##############################
# variável estatística: Genero
# qualitativa

######
# i. medidas de localização central
#moda
(ni.g <- table(obesidade$Genero))

if( min(ni.g)==max(ni.g)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.g)[ni.g==max(ni.g)]))
}


##############################
# variável estatística: Idade
# quantitativa

######
# i. medidas de localização central
#moda
(ni.Idade <- table(obesidade$Idade))

if( min(ni.Idade)==max(ni.Idade)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.Idade)[ni.Idade==max(ni.Idade)]))
}
#média
mean(obesidade$Idade)
#mediana
median(obesidade$Idade)

######
# ii. quartis
quantile(obesidade$Idade, prob=c(0.25,0.50, 0.75))

######
# iii. nono decil
quantile(obesidade$Idade, prob=0.90)

######
# iV. terceiro percentil
quantile(obesidade$Idade, prob=0.03)

######
# v. extremos
#mínimo
min(obesidade$Idade)
#máximo
max(obesidade$Idade)

######
# vi. medidas de dispersão absoluta
# amplitude total
max(obesidade$Idade)-min(obesidade$Idade)
# amplitude Interquartil
IQR(obesidade$Idade)
# variância
var(obesidade$Idade)
# desvio padrão
sd(obesidade$Idade)

######
# vii. medidas de dispersão relativa
# coeficiente de variação
(sd(obesidade$Idade)/mean(obesidade$Idade))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade$Idade)


##############################
# variável estatística: Altura
# quantitativa

######
# i. medidas de localização central
#moda
(ni.Altura <- table(obesidade$Altura))

if( min(ni.Altura)==max(ni.Altura)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.Altura)[ni.Altura==max(ni.Altura)]))
}

#média
mean(obesidade$Altura)
#mediana
median(obesidade$Altura)

######
# ii. quartis
quantile(obesidade$Altura, prob=c(0.25,0.50, 0.75))

######
# iii. nono decil
quantile(obesidade$Altura, prob=0.90)

######
# iV. terceiro percentil
quantile(obesidade$Altura, prob=0.03)

######
# v. extremos
#mínimo
min(obesidade$Altura)
#máximo
max(obesidade$Altura)

######
# vi. medidas de dispersão absoluta
# amplitude total
max(obesidade$Altura)-min(obesidade$Altura)
# amplitude Interquartil
IQR(obesidade$Altura)
# variância
var(obesidade$Altura)
# desvio padrão
sd(obesidade$Altura)

######
# vii. medidas de dispersão relativa
# coeficiente de variação
(sd(obesidade$Altura)/mean(obesidade$Altura))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade$Altura)


##############################
# variável estatística: Peso
# quantitativa

######
# i. medidas de localização central
#moda
(ni.Peso <- table(obesidade$Peso))

if( min(ni.Peso)==max(ni.Peso)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.Peso)[ni.Peso==max(ni.Peso)]))
}

#média
mean(obesidade$Peso)
#mediana
median(obesidade$Peso)

######
# ii. quartis
quantile(obesidade$Peso, prob=c(0.25,0.50, 0.75))

######
# iii. nono decil
quantile(obesidade$Peso, prob=0.90)

######
# iV. terceiro percentil
quantile(obesidade$Peso, prob=0.03)

######
# v. extremos
#mínimo
min(obesidade$Peso)
#máximo
max(obesidade$Peso)

######
# vi. medidas de dispersão absoluta
# amplitude total
max(obesidade$Peso)-min(obesidade$Peso)
# amplitude Interquartil
IQR(obesidade$Peso)
# variância
var(obesidade$Peso)
# desvio padrão
sd(obesidade$Peso)

######
# vii. medidas de dispersão relativa
# coeficiente de variação
(sd(obesidade$Peso)/mean(obesidade$Peso))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade$Peso)


##############################
# variável estatística: FAVC
# qualitativa

#moda
(ni.FAVC <- table(obesidade$FAVC))

if( min(ni.FAVC)==max(ni.FAVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.FAVC)[ni.FAVC==max(ni.FAVC)]))
}


##############################
# variável estatística: FCVC
# qualitativa

#moda
(ni.FCVC <- table(obesidade$FCVC))

if( min(ni.FCVC)==max(ni.FCVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.FCVC)[ni.FCVC==max(ni.FCVC)]))
}


##############################
# variável estatística: NCP
# quantitativa

######
# i. medidas de localização central
#moda
(ni.NCP <- table(obesidade$NCP))

if( min(ni.NCP)==max(ni.NCP)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.NCP)[ni.NCP==max(ni.NCP)]))
}

#média
mean(obesidade$NCP)
#mediana
median(obesidade$NCP)

######
# ii. quartis
quantile(obesidade$NCP, prob=c(0.25,0.50, 0.75))

######
# iii. nono decil
quantile(obesidade$NCP, prob=0.90)

######
# iV. terceiro percentil
quantile(obesidade$NCP, prob=0.03)

######
# v. extremos
#mínimo
min(obesidade$NCP)
#máximo
max(obesidade$NCP)

######
# vi. medidas de dispersão absoluta
# amplitude total
max(obesidade$NCP)-min(obesidade$NCP)
# amplitude Interquartil
IQR(obesidade$NCP)
# variância
var(obesidade$NCP)
# desvio padrão
sd(obesidade$NCP)

######
# vii. medidas de dispersão relativa
# coeficiente de variação
(sd(obesidade$NCP)/mean(obesidade$NCP))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade$NCP)


##############################
# variável estatística: CAEC
# qualitativa

#moda
(ni.CAEC <- table(obesidade$CAEC))

if( min(ni.CAEC)==max(ni.CAEC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.CAEC)[ni.CAEC==max(ni.CAEC)]))
}


##############################
# variável estatística: Fumar
# qualitativa

#moda
(ni.Fumar <- table(obesidade$Fumar))

if( min(ni.Fumar)==max(ni.Fumar)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.Fumar)[ni.Fumar==max(ni.Fumar)]))
}


##############################
# variável estatística: CH2O
# qualitativa

#moda
(ni.CH2O <- table(obesidade$CH2O))

if( min(ni.CH2O)==max(ni.CH2O)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.CH2O)[ni.CH2O==max(ni.CH2O)]))
}


##############################
# variável estatística: FAF
# qualitativa

#moda
(ni.FAF <- table(obesidade$FAF))

if( min(ni.FAF)==max(ni.FAF)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.FAF)[ni.FAF==max(ni.FAF)]))
}


##############################
# variável estatística: CALC
# qualitativa

#moda
(ni.CALC <- table(obesidade$CALC))

if( min(ni.CALC)==max(ni.CALC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.CALC)[ni.CALC==max(ni.CALC)]))
}


##############################
# variável estatística: MTRANS
# qualitativa

#moda
(ni.MTRANS <- table(obesidade$MTRANS))

if( min(ni.MTRANS)==max(ni.MTRANS)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.MTRANS)[ni.MTRANS==max(ni.MTRANS)]))
}


##############################################################
# 2 (b)

##############################
# variável estatística: Idade
# quantitativa

# i. sem indicação de outliers
boxplot(obesidade$Idade, col=2, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Idade", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(obesidade$Idade, col=2, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Idade", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(obesidade$Idade, col=2, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Idade", range=3)


##############################
# variável estatística: Altura
# quantitativa

# i. sem indicação de outliers
boxplot(obesidade$Altura, col=3, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Altura", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(obesidade$Altura, col=3, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Altura", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(obesidade$Altura, col=3, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Altura", range=3)


##############################
# variável estatística: Peso
# quantitativa

# i. sem indicação de outliers
boxplot(obesidade$Peso, col=4, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Peso", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(obesidade$Peso, col=4, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Peso", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(obesidade$Peso, col=4, main="Diagrama de extremos e quartis", 
        horizontal=TRUE, xlab="Peso", range=3)


##############################
# variável estatística: NCP
# quantitativa

# i. sem indicação de outliers
boxplot(obesidade$NCP, col=5, main="Diagrama de extremos e quartis", horizontal=TRUE, 
        xlab="Número de refeições principais que tem habitualmente", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(obesidade$NCP, col=5, main="Diagrama de extremos e quartis", horizontal=TRUE, 
        xlab="Número de refeições principais que tem habitualmente", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(obesidade$NCP, col=5, main="Diagrama de extremos e quartis", horizontal=TRUE, 
        xlab="Número de refeições principais que tem habitualmente", range=3)


##############################################################
# 2 (d)

# dados dos género feminino
obesidade.F <- obesidade[obesidade$Genero=="Feminino",] 
nrow(obesidade.F)

# dados dos género masculino
obesidade.M <- obesidade[obesidade$Genero=="Masculino",] 
nrow(obesidade.M)

##############################
# variável estatística: Idade
# quantitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.Idade <- table(obesidade.F$Idade))
if( min(ni.F.Idade)==max(ni.F.Idade)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.Idade)[ni.F.Idade==max(ni.F.Idade)]))
}

(ni.M.Idade <- table(obesidade.M$Idade))
if( min(ni.M.Idade)==max(ni.M.Idade)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.Idade)[ni.M.Idade==max(ni.M.Idade)]))
}

#média
mean(obesidade.F$Idade)
mean(obesidade.M$Idade)

#mediana
median(obesidade.F$Idade)
median(obesidade.M$Idade)


######
# ii. quartis

quantile(obesidade.F$Idade, prob=c(0.25,0.50,0.75))
quantile(obesidade.M$Idade, prob=c(0.25,0.50,0.75))


######
# iii. nono decil
quantile(obesidade.F$Idade, prob=0.90)
quantile(obesidade.M$Idade, prob=0.90)


######
# iV. terceiro percentil
quantile(obesidade.F$Idade, prob=0.03)
quantile(obesidade.M$Idade, prob=0.03)


######
# v. extremos
#mínimo
min(obesidade.F$Idade)
min(obesidade.M$Idade)
#máximo
max(obesidade.F$Idade)
max(obesidade.M$Idade)


######
# vi. medidas de dispersão absoluta

# amplitude total
max(obesidade.F$Idade)-min(obesidade.F$Idade)
max(obesidade.M$Idade)-min(obesidade.M$Idade)

# amplitude Interquartil
IQR(obesidade.F$Idade)
IQR(obesidade.M$Idade)

# variância
var(obesidade.F$Idade)
var(obesidade.M$Idade)

# desvio padrão
sd(obesidade.F$Idade)
sd(obesidade.M$Idade)

######
# vii. medidas de dispersão relativa

# coeficiente de variação
(sd(obesidade.F$Idade)/mean(obesidade.F$Idade))*100
(sd(obesidade.M$Idade)/mean(obesidade.M$Idade))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade.F$Idade)
e1071::skewness(obesidade.M$Idade)

#######
# (b) #
#######

# i. sem indicação de outliers
boxplot(Idade ~ Genero, data=obesidade, col=2:3, xlab="Género", ylab="Idade", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(Idade ~ Genero, data=obesidade, col=2:3, xlab="Género", ylab="Idade", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(Idade ~ Genero, data=obesidade, col=2:3, xlab="Género", ylab="Idade", range=3)


##############################
# variável estatística: Altura
# quantitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda
(ni.F.Altura <- table(obesidade.F$Altura))
if( min(ni.F.Altura)==max(ni.F.Altura)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.Altura)[ni.F.Altura==max(ni.F.Altura)]))
}

(ni.M.Altura <- table(obesidade.M$Altura))
if( min(ni.M.Altura)==max(ni.M.Altura)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.Altura)[ni.M.Altura==max(ni.M.Altura)]))
}

#média
mean(obesidade.F$Altura)
mean(obesidade.M$Altura)

#mediana (fórmula dos slides)
median(obesidade.F$Altura, type=2)
median(obesidade.M$Altura, type=2)

######
# ii. quartis
quantile(obesidade.F$Altura, prob=c(0.25,0.50,0.75))
quantile(obesidade.M$Altura, prob=c(0.25,0.50,0.75))


######
# iii. nono decil
quantile(obesidade.F$Altura, prob=0.90)
quantile(obesidade.M$Altura, prob=0.90)


######
# iV. terceiro percentil
quantile(obesidade.F$Altura, prob=0.03)
quantile(obesidade.M$Altura, prob=0.03)


######
# v. extremos
#mínimo
min(obesidade.F$Altura)
min(obesidade.M$Altura)
#máximo
max(obesidade.F$Altura)
max(obesidade.M$Altura)


######
# vi. medidas de dispersão absoluta

# amplitude total
max(obesidade.F$Altura)-min(obesidade.F$Altura)
max(obesidade.M$Altura)-min(obesidade.M$Altura)

# amplitude Interquartil
IQR(obesidade.F$Altura)
IQR(obesidade.M$Altura)

# variância
var(obesidade.F$Altura)
var(obesidade.M$Altura)

# desvio padrão
sd(obesidade.F$Altura)
sd(obesidade.M$Altura)

######
# vii. medidas de dispersão relativa

# coeficiente de variação
(sd(obesidade.F$Altura)/mean(obesidade.F$Altura))*100
(sd(obesidade.M$Altura)/mean(obesidade.M$Altura))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade.F$Altura)
e1071::skewness(obesidade.M$Altura)

#######
# (b) #
#######

# i. sem indicação de outliers
boxplot(Altura ~ Genero, data=obesidade, col=4:5, xlab="Género", ylab="Altura", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(Altura ~ Genero, data=obesidade, col=4:5, xlab="Género", ylab="Altura", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(Altura ~ Genero, data=obesidade, col=4:5, xlab="Género", ylab="Altura", range=3)



##############################
# variável estatística: Peso
# quantitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda
(ni.F.Peso <- table(obesidade.F$Peso))
if( min(ni.F.Peso)==max(ni.F.Peso)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.Peso)[ni.F.Peso==max(ni.F.Peso)]))
}

(ni.M.Peso <- table(obesidade.M$Peso))
if( min(ni.M.Peso)==max(ni.M.Peso)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.Peso)[ni.M.Peso==max(ni.M.Peso)]))
}


#média
mean(obesidade.F$Peso)
mean(obesidade.M$Peso)

#mediana
median(obesidade.F$Peso)
median(obesidade.M$Peso)


######
# ii. quartis
quantile(obesidade.F$Peso, prob=c(0.25,0.50,0.75))
quantile(obesidade.M$Peso, prob=c(0.25,0.50,0.75))


######
# iii. nono decil
quantile(obesidade.F$Peso, prob=0.90)
quantile(obesidade.M$Peso, prob=0.90)


######
# iV. terceiro percentil
quantile(obesidade.F$Peso, prob=0.03)
quantile(obesidade.M$Peso, prob=0.03)


######
# v. extremos
#mínimo
min(obesidade.F$Peso)
min(obesidade.M$Peso)
#máximo
max(obesidade.F$Peso)
max(obesidade.M$Peso)


######
# vi. medidas de dispersão absoluta

# amplitude total
max(obesidade.F$Peso)-min(obesidade.F$Peso)
max(obesidade.M$Peso)-min(obesidade.M$Peso)

# amplitude Interquartil
IQR(obesidade.F$Peso)
IQR(obesidade.M$Peso)

# variância
var(obesidade.F$Peso)
var(obesidade.M$Peso)

# desvio padrão
sd(obesidade.F$Peso)
sd(obesidade.M$Peso)

######
# vii. medidas de dispersão relativa

# coeficiente de variação
(sd(obesidade.F$Peso)/mean(obesidade.F$Peso))*100
(sd(obesidade.M$Peso)/mean(obesidade.M$Peso))*100

######
# viii. medidas de simetria
e1071::skewness(obesidade.F$Peso)
e1071::skewness(obesidade.M$Peso)

#######
# (b) #
#######

# i. sem indicação de outliers
boxplot(Peso ~ Genero, data=obesidade, col=6:7, xlab="Género", ylab="Peso", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(Peso ~ Genero, data=obesidade, col=6:7, xlab="Género", ylab="Peso", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(Peso ~ Genero, data=obesidade, col=6:7, xlab="Género", ylab="Peso", range=3)


##############################
# variável estatística: FAVC
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda
(ni.F.FAVC <- table(obesidade.F$FAVC))
if( min(ni.F.FAVC)==max(ni.F.FAVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.FAVC)[ni.F.FAVC==max(ni.F.FAVC)]))
}

(ni.M.FAVC <- table(obesidade.M$FAVC))
if( min(ni.M.FAVC)==max(ni.M.FAVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.FAVC)[ni.M.FAVC==max(ni.M.FAVC)]))
}

##############################
# variável estatística: FCVC
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda
(ni.F.FCVC <- table(obesidade.F$FCVC))
if( min(ni.F.FCVC)==max(ni.F.FCVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.FCVC)[ni.F.FCVC==max(ni.F.FCVC)]))
}

(ni.M.FCVC <- table(obesidade.M$FCVC))
if( min(ni.M.FCVC)==max(ni.M.FCVC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.FCVC)[ni.M.FCVC==max(ni.M.FCVC)]))
}

##############################
# variável estatística: NCP
# quantitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.NCP <- table(obesidade.F$NCP))
if( min(ni.F.NCP)==max(ni.F.NCP)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.NCP)[ni.F.NCP==max(ni.F.NCP)]))
}

(ni.M.NCP <- table(obesidade.M$NCP))
if( min(ni.M.NCP)==max(ni.M.NCP)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.NCP)[ni.M.NCP==max(ni.M.NCP)]))
}


#média
mean(obesidade.F$NCP)
mean(obesidade.M$NCP)

#mediana  (fórmula dos slides: type=2)
median(obesidade.F$NCP, type=2)
median(obesidade.M$NCP, type=2)

######
# ii. quartis
quantile(obesidade.F$NCP, prob=c(0.25,0.50,0.75))
quantile(obesidade.M$NCP, prob=c(0.25,0.50,0.75))


######
# iii. nono decil
quantile(obesidade.F$NCP, prob=0.90)
quantile(obesidade.M$NCP, prob=0.90)


######
# iV. terceiro percentil
quantile(obesidade.F$NCP, prob=0.03)
quantile(obesidade.M$NCP, prob=0.03)


######
# v. extremos
#mínimo
min(obesidade.F$NCP)
min(obesidade.M$NCP)
#máximo
max(obesidade.F$NCP)
max(obesidade.M$NCP)


######
# vi. medidas de dispersão absoluta

# amplitude total
max(obesidade.F$NCP)-min(obesidade.F$NCP)
max(obesidade.M$NCP)-min(obesidade.M$NCP)

# amplitude Interquartil
IQR(obesidade.F$NCP)
IQR(obesidade.M$NCP)

# variância
var(obesidade.F$NCP)
var(obesidade.M$NCP)

# desvio padrão
sd(obesidade.F$NCP)
sd(obesidade.M$NCP)


######
# vii. medidas de dispersão relativa

# coeficiente de variação
(sd(obesidade.F$NCP)/mean(obesidade.F$NCP))*100
(sd(obesidade.M$NCP)/mean(obesidade.M$NCP))*100


######
# viii. medidas de simetria
e1071::skewness(obesidade.F$NCP)
e1071::skewness(obesidade.M$NCP)

#######
# (b) #
#######

# i. sem indicação de outliers
boxplot(NCP ~ Genero, data=obesidade, col=8:9, xlab="Género", 
        ylab="Número de refeições principais que tem habitualmente", range=0)
# ii. com indicação de outliers a partir dos moderados
boxplot(NCP ~ Genero, data=obesidade, col=8:9, xlab="Género", 
        ylab="Número de refeições principais que tem habitualmente", range=1.5)
# iii. com indicação de outliers a partir dos severos
boxplot(NCP ~ Genero, data=obesidade, col=8:9, xlab="Género", 
        ylab="Número de refeições principais que tem habitualmente", range=3)


##############################
# variável estatística: CAEC
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.CAEC <- table(obesidade.F$CAEC))
if( min(ni.F.CAEC)==max(ni.F.CAEC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.CAEC)[ni.F.CAEC==max(ni.F.CAEC)]))
}

(ni.M.CAEC <- table(obesidade.M$CAEC))
if( min(ni.M.CAEC)==max(ni.M.CAEC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.CAEC)[ni.M.CAEC==max(ni.M.CAEC)]))
}

##############################
# variável estatística: Fumar
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.Fumar <- table(obesidade.F$Fumar))
if( min(ni.F.Fumar)==max(ni.F.Fumar)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.Fumar)[ni.F.Fumar==max(ni.F.Fumar)]))
}

(ni.M.Fumar <- table(obesidade.M$Fumar))
if( min(ni.M.Fumar)==max(ni.M.Fumar)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.Fumar)[ni.M.Fumar==max(ni.M.Fumar)]))
}

##############################
# variável estatística: CH2O
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.CH2O <- table(obesidade.F$CH2O))
if( min(ni.F.CH2O)==max(ni.F.CH2O)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.CH2O)[ni.F.CH2O==max(ni.F.CH2O)]))
}

(ni.M.CH2O <- table(obesidade.M$CH2O))
if( min(ni.M.CH2O)==max(ni.M.CH2O)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.CH2O)[ni.M.CH2O==max(ni.M.CH2O)]))
}

##############################
# variável estatística: FAF
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.FAF <- table(obesidade.F$FAF))
if( min(ni.F.FAF)==max(ni.F.FAF)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.FAF)[ni.F.FAF==max(ni.F.FAF)]))
}

(ni.M.FAF <- table(obesidade.M$FAF))
if( min(ni.M.FAF)==max(ni.M.FAF)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.FAF)[ni.M.FAF==max(ni.M.FAF)]))
}


##############################
# variável estatística: CALC
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.CALC <- table(obesidade.F$CALC))
if( min(ni.F.CALC)==max(ni.F.CALC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.CALC)[ni.F.CALC==max(ni.F.CALC)]))
}

(ni.M.CALC <- table(obesidade.M$CALC))
if( min(ni.M.CALC)==max(ni.M.CALC)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.CALC)[ni.M.CALC==max(ni.M.CALC)]))
}

##############################
# variável estatística: MTRANS
# qualitativa

#######
# (a) #
#######


######
# i. medidas de localização central

#moda

(ni.F.MTRANS <- table(obesidade.F$MTRANS))
if( min(ni.F.MTRANS)==max(ni.F.MTRANS)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.F.MTRANS)[ni.F.MTRANS==max(ni.F.MTRANS)]))
}

(ni.M.MTRANS <- table(obesidade.M$MTRANS))
if( min(ni.M.MTRANS)==max(ni.M.MTRANS)){
  print("amodal")
} else{
  print(paste("Moda: ", names(ni.M.MTRANS)[ni.M.MTRANS==max(ni.M.MTRANS)]))
}




##############################################################
# 2 (f)

#################################
# i) variável estatística: Idade

# mínimo e máximo dos dados
min(obesidade$Idade)
max(obesidade$Idade)

k1 <- 8   # 8 classes
h1 <- 6   # amplitude 6 anos

# mínimo e máximo das classes
valor.min1 <- 14   # primeira classe a começar nos 14 anos
(valor.max1 <- valor.min1 + h1*k1)

#extremos das classes
(cortes1 <- seq(valor.min1, valor.max1, by=h1))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados = ao primeiro valor da primeira classe
# a primeira classe tem de ser fechada dos dois lados
(classes1 <- cut(obesidade$Idade, breaks=cortes1, right=TRUE, include.lowest=TRUE))

# classe modal
(ni.ei <- table(classes1))
if( min(ni.ei)==max(ni.ei)){
  print("amodal")
} else{
  print(paste("Classe Modal: ", names(ni.ei)[ni.ei==max(ni.ei)]))
}


# classes dos quartis
(fi.ei <- prop.table(ni.ei))
round(cumsum(fi.ei),4)


#################################
# ii) variável estatística: Altura

# mínimo e máximo dos dados
min(obesidade$Altura)
max(obesidade$Altura)

# 3 classes: [1.45,1.60], ]1.60,1.80] e ]1.80,2.00]
#extremos das classes
cortes2 <- c(1.45,1.60,1.80,2.00)

# intervalos abertos à esquerda e fechados à direita
# a primeira classe fechada dos dois lados
(classes2 <- cut(obesidade$Altura, breaks=cortes2, right=TRUE, include.lowest=TRUE))

# classe modal
(ni.eii <- table(classes2))
if( min(ni.eii)==max(ni.eii)){
  print("amodal")
} else{
  print(paste("Classe Modal: ", names(ni.eii)[ni.eii==max(ni.eii)]))
}

# classes dos quartis
(fi.eii <- prop.table(ni.eii))
round(cumsum(fi.eii),4)


#################################
# ii) variável estatística: Peso

# mínimo e máximo dos dados
min(obesidade$Peso)
max(obesidade$Peso)

(k3<-trunc(1+log(n)/log(2)))   # número de classes
(h3 <-  (max(obesidade$Peso)-min(obesidade$Peso))/k3)   # amplitude das classes

# mínimo e máximo das classes
(valor.min3 <- min(obesidade$Peso))
(valor.max3 <- valor.min3 + h3*k3)

#extremos das classes
(cortes3 <- seq(valor.min3, valor.max3, by=h3))

# intervalos fechados à esquerda e abertos à direita
# como o maximo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados
(classes3 <- cut(obesidade$Peso, breaks=cortes3, right=FALSE, include.lowest=TRUE))

# classe modal
(ni.eiii <- table(classes3))
if( min(ni.eiii)==max(ni.eiii)){
  print("amodal")
} else{
  print(paste("Classe Modal: ", names(ni.eiii)[ni.eiii==max(ni.eiii)]))
}

# classes dos quartis
(fi.eiii <- prop.table(ni.eiii))
round(cumsum(fi.eiii),4)


