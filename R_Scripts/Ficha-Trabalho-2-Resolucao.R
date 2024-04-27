#######################
# FICHA DE TRABALHO 2 #
#######################


# EXERCÍCIO 1

#dados
#ler os dados do ficheiro: File -> Import Datset -> From Text (base)
stroke

##############################################################
# 1 (a)

#verificar se há casos omissos
anyNA(stroke)

#tabela sem os casos omissos
stroke2 = na.omit(stroke)
View(stroke2)

# dimensão da tabela inicial
dim(stroke)
# dimensão da tabela sem casos omissos
dim(stroke2)

#número de indivíduos com pelo menos um dado omisso
nrow(stroke)-nrow(stroke2)


##############################################################
# 1 (b)

# gráficos de barras -> barplot()

# variável estatística: sex
(ni.s = table(stroke2$sex))            # frequências absolutas

#gráfico de barras das frequências absolutas
barplot(ni.s, xlab="Género", ylab="Frequências absolutas", 
        col=c("red", "yellow"), ylim=c(0,200), names.arg=c("masculino", "feminino"))



##############################################################
# 1 (c)

# gráficos de barras -> barplot()

# variável estatística: coma
(ni.cm = table(stroke2$coma))            # frequências absolutas
(fi.cm = round(prop.table(ni.cm),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.cm, xlab="coma", ylab="Frequências relativas", 
        col=5:6, ylim=c(0,1), names.arg=c("Não", "Sim"),
        main="Se o paciente entrou em coma após o AVC")


##############################################################
# 1 (d)

# gráficos circulares -> pie()

#################
# variável sex

pie(ni.s, labels=paste(ni.s, " = frequências absolutas"),  col=c("red", "yellow"), main="Género")
legend("topleft", legend=c("masculino", "feminino"), fill=c("red", "yellow"))


#################
# variável coma

pie(ni.cm, labels=paste(fi.cm*100, "%"),  col=5:6, main="Se o paciente entrou em coma após o AVC")
legend("bottomright", legend=c("Não", "Sim"), fill=5:6)

#################
# variável diab

(ni.db = table(stroke2$diab))          # frequências absolutas
(fi.db = round(prop.table(ni.db),4))   # frequências relativas

pie(fi.db, labels=paste(fi.db*100, "%"),  col=c(2,4), main="Se o paciente tem histórico de diabetes")
legend("bottomleft", legend=c("Não", "Sim"), fill=c(2,4))

#################
# variável dead

(ni.dd = table(stroke2$dead))          # frequências absolutas
(fi.dd = round(prop.table(ni.dd),4))   # frequências relativas

pie(fi.dd, labels=paste(fi.dd*100, "%"),  col=c("green","red"), main="Se o paciente morreu durante o estudo")
legend("topright", legend=c("Não", "Sim"), fill=c("green","red"))


##############################################################
# 1 (e)

# só os indivíduos que entraram em coma
comaS = stroke2[stroke2$coma==2,]

(ni.cmS = table(comaS$dgn))              # frequências absolutas
(fi.cmS = round(prop.table(ni.cmS),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.cmS, xlab="diagnóstico", ylab="Frequências relativas", 
        col=2:4, ylim=c(0,1), main="Diagnóstico dos pacientes que entraram em coma")


##############################################################
# 1 (f)

# só os indivíduos que não entraram em coma
comaN = stroke2[stroke2$coma==1,]

(ni.cmN = table(comaN$dgn))              # frequências absolutas
(fi.cmN = round(prop.table(ni.cmN),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.cmN, xlab="diagnóstico", ylab="Frequências relativas", 
        col=2:5, ylim=c(0,1), main="Diagnóstico dos pacientes que não entraram em coma")


##############################################################
# 1 (h)

# variável estatística: age
# embora os dados tenham sido recolhidos como quantitativos discretos
# como têm muitos níveis, serão analisados como quantitativos contínuos

# histograma -> hist()

# mínimo e máximo dos dados
min(stroke2$age)
max(stroke2$age)

(n0 = length(stroke2$age))  # dimensão da amostra
(k0 = round(1+log(n0)/log(2),0))   # número de classes
(h0 = (max(stroke2$age)-min(stroke2$age))/k0)   # amplitude das classes

# mínimo e máximo das classes
(valor.min0 = min(stroke2$age))
(valor.max0 = valor.min0 + h0*k0)

#extremos das classes
(cortes0 = seq(valor.min0, valor.max0, by=h0))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados é igual ao primeiro valor da primeira classe
# a primeira classe tem de ser fechada nos dois lados

#####
# histograma -> eixo dos yy -> frequências absolutas
hist(stroke2$age, breaks=cortes0, right=TRUE, include.lowest=TRUE,
     freq=TRUE,
     main="histograma",
     xlab="Idade do paciente no momento do acidente vascular cerebral",
     ylab="frequências absolutas",
     col="yellow",
     ylim=c(0,100),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,round(cortes0,1)))  # definir os valores para o eixo dos xx igual às classes

##############################################################
# 1 (i)

# só os indivíduos que não morreram no mesmo dia do AVC
Nmorreu = stroke2[stroke2$obsmonths!=0.1,]

# variável estatística: obsmonths
# dados quantitativos contínuos

# histograma -> hist()

# mínimo e máximo dos dados
min(Nmorreu$obsmonths)
max(Nmorreu$obsmonths)

(n01 = length(Nmorreu$obsmonths))  # dimensão da amostra
(k01 = round(1+log(n01)/log(2),0))   # número de classes
(h01 = (max(Nmorreu$obsmonths)-min(Nmorreu$obsmonths))/k01)   # amplitude das classes

# mínimo e máximo das classes
(valor.min01 = min(Nmorreu$obsmonths))
(valor.max01 = valor.min01 + h01*k01)

#extremos das classes
(cortes01 = seq(valor.min01, valor.max01, by=h01))

# intervalos fechados à esquerda e abertos à direita
# como o máxiiimo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados

#####
# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
hist(Nmorreu$obsmonths, breaks=cortes01, right=FALSE, include.lowest=TRUE,
     freq=FALSE,
     main="histograma",
     xlab="tempo de observação do paciente em meses",
     ylab="frequências relativas / amplitude das classes",
     col=2,
     ylim=c(0,0.08),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,round(cortes01,2)))  # definir os valores para o eixo dos xx igual às classes


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

# gráficos de barras -> barplot()


#################
# variável FCVC

(ni.FCVC = table(obesidade$FCVC)) # frequências absolutas

#gráfico de barras das frequências absolutas
barplot(ni.FCVC, main="Se come habitualmente vegetais nas refeições", xlab="FCVC", ylab="Frequências absolutas", 
        col=2:4, ylim=c(0,1200), names.arg=c("Nunca", "Às vezes", "Sempre"))


#################
# variável CAEC

# colocar as categorias da variável por ordem
obesidade$CAEC = factor(obesidade$CAEC,levels=c("N", "S", "F", "A"))

(ni.CAEC = table(obesidade$CAEC)) # frequências absolutas

#gráfico de barras das frequências absolutas
barplot(ni.CAEC, main="Se come habitualmente vegetais nas refeições", xlab="CAEC", ylab="Frequências absolutas", 
        col=c(3,4,6,7), ylim=c(0,2000))
legend("topright", legend=c("N = Não", "S = Às vezes", "F = Frequentemente", "A = Sempre"), fill=c(3,4,6,7))


#################
# variável CH2O

(ni.CH2O = table(obesidade$CH2O))          # frequências absolutas
(fi.CH2O = round(prop.table(ni.CH2O),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.CH2O, main="Quantidade de água que bebe diariamente", xlab="CH2O", ylab="Frequências relativas", 
        col=13:15, ylim=c(0,0.6), names.arg=c("< 1 litro", "1 - 2 litros", "> 2 litros"))


#################
# variável FAF

(ni.FAF = table(obesidade$FAF))          # frequências absolutas
(fi.FAF = round(prop.table(ni.FAF),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.FAF, main="Com que frequência pratica atividade física por semana", xlab="FAF", ylab="Frequências relativas", 
        col=3:6, ylim=c(0,0.4), names.arg=c("não pratica", "1 - 2 dias", "3 - 4 dias","+ de 4 dias"))


#################
# variável CALC

# colocar as categorias da variável por ordem
obesidade$CALC = factor(obesidade$CALC, levels=c("N", "S", "F", "A"))

(ni.CALC = table(obesidade$CALC))       # frequências absolutas
(fi.CALC = round(prop.table(ni.CALC),4))   # frequências relativas

#gráfico de barras das frequências relativas em percentagem
barplot(fi.CALC*100, main="Com que frequência bebe bebidas alcoólicas", xlab="CALC", ylab="Frequências relativas em %", 
        col=5:2, ylim=c(0,80))
legend("topright", legend=c("N = Nunca", "S = Às vezes", "F = Frequentemente", "A = Sempre"), fill=5:2)


##############################################################
# Exercício 2 (b)

# gráficos circulares -> pie()


#################
# variável Género

(ni.G = table(obesidade$Genero))     # frequências absolutas
(fi.G = round(prop.table(ni.G),4))   # frequências relativas

pie(ni.G, labels=paste(fi.G*100, "%"),  col=c("red", "yellow"), main="Género")
legend("topright", legend=names(ni.G), fill=c("red", "yellow"))
 

#################
# variável FAVC

(ni.FAVC = table(obesidade$FAVC))          # frequências absolutas
(fi.FAVC = round(prop.table(ni.FAVC),4))   # frequências relativas

pie(ni.FAVC, labels=paste(fi.FAVC*100, "%"),  col=2:3, main="Se come alimentos altamente calóricos habitualmente")
legend("topright", legend=c("Não","Sim"), fill=2:3)


#################
# variável Fumar

(ni.F = table(obesidade$Fumar))          # frequências absolutas
(fi.F = round(prop.table(ni.F),4))   # frequências relativas

pie(ni.F, labels=paste(fi.F*100, "%"),  col=c("green","red"), main="Fumar")
legend("topright", legend=c("Não","Sim"), fill=c("green","red"))


#################
# variável MTRANS

# colocar as categorias por outra ordem para o gráfico ficar legível
obesidade$MTRANS = factor(obesidade$MTRANS, levels=c("Automovel", "Mota", "Transportes_Publicos", "Bicicleta", "A_pe"))

(ni.MTRANS = table(obesidade$MTRANS))          # frequências absolutas
(fi.MTRANS = round(prop.table(ni.MTRANS),4))   # frequências relativas

pie(ni.MTRANS, labels=paste(fi.MTRANS*100, "%"), cex = 0.7, col=2:6, 
    main="Que tipo de transporte utiliza habitualmente")
legend("topleft", legend=c("Automóvel", "Mota", "Transportes públicos", "Bicicleta", "Anda a pé"), 
       fill=2:6, cex = 0.7)

##############################################################
# Exercício 2 (c)

# gráficos de barras -> barplot()
# gráficos circulares -> pie()

#################
# variável NCP


(ni.NCP = table(obesidade$NCP))       # frequências absolutas
(fi.NCP = round(prop.table(ni.NCP),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(fi.NCP, main="Número de refeições principais que tem habitualmente", xlab="NCP", ylab="Frequências relativas", 
        col=2:5, ylim=c(0,1))


##############################################################
# Exercício 2 (d)

# histograma -> hist()

####################
# Exercício 2 (d) i.

# variável estatística: Idade

# mínimo e máximo dos dados
min(obesidade$Idade)
max(obesidade$Idade)

k1 = 8   # 8 classes
h1 = 6   # amplitude 6 anos

# mínimo e máximo das classes
valor.min1 = 14   # primeira classe a começar nos 14 anos
(valor.max1 = valor.min1 + h1*k1)

#extremos das classes
(cortes1 = seq(valor.min1, valor.max1, by=h1))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados = ao primeiro valor da primeira classe
# a primeira classe tem de ser fechada dos dois lados

#####
# histograma -> eixo dos yy -> frequências absolutas
hist(obesidade$Idade, breaks=cortes1, right=TRUE, include.lowest=TRUE,
     freq=TRUE,
     main="histograma",
     xlab="Idade",
     ylab="frequências absolutas",
     col=2,
     xlim=c(0,70),
     ylim=c(0,1200),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,cortes1,70))  # definir os valores para o eixo dos xx igual às classes


#####################
# Exercício 2 (d) ii.

# variável estatística: Altura

# mínimo e máximo dos dados
min(obesidade$Altura)
max(obesidade$Altura)

# 3 classes: [1.45,1.60], ]1.60,1.80] e ]1.80,2.00]
#extremos das classes
cortes2 = c(1.45,1.60,1.80,2.00)

# intervalos abertos à esquerda e fechados à direita
# a primeira classe fechada dos dois lados

#####
# histograma -> eixo dos yy -> frequências absolutas -> só faz se colocar freq=TRUE 
# avisa que está errado

#####
# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
# como as classes têm amplitudes diferentes, este é o histograma correto
hist(obesidade$Altura, breaks=cortes2, right=TRUE, include.lowest=TRUE,
     freq=FALSE,
     main="histograma",
     xlab="Altura",
     ylab="frequências relativas / amplitude das classes",
     col=3,
     xlim=c(0,2.5),
     ylim=c(0,4),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,cortes2,2.5), las=2)  # definir os valores para o eixo dos xx igual às classes


######################
# Exercício 2 (d) iii.

# variável estatística: Peso

# mínimo e máximo dos dados
min(obesidade$Peso)
max(obesidade$Peso)

(n = length(obesidade$Peso))  # dimensão da amostra
(k3 = round(1+log(n)/log(2),0))   # número de classes
(h3 = (max(obesidade$Peso)-min(obesidade$Peso))/k3)   # amplitude das classes

# mínimo e máximo das classes
(valor.min3 = min(obesidade$Peso))
(valor.max3 = valor.min3 + h3*k3)

#extremos das classes
(cortes3 = seq(valor.min3, valor.max3, by=h3))

# intervalos fechados à esquerda e abertos à direita
# como o máximo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados


#####
# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
hist(obesidade$Peso, breaks=cortes3, right=FALSE, include.lowest=TRUE,
     freq=FALSE,
     main="histograma",
     xlab="Peso",
     ylab="frequências relativas / amplitude das classes",
     col=4,
     xlim=c(0,180),
     ylim=c(0,0.02),
     xaxt="n")                  # para poder definir o eixo dos xx
axis(side=1, at=c(0,round(cortes3,1),180), las=2)  # definir os valores para o eixo dos xx igual às classes

