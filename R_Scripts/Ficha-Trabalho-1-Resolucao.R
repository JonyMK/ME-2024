#######################
# FICHA DE TRABALHO 1 #
#######################


# EXERCÍCIO 2

# 2 (b)

# definir os vetores = cada coluna da tabela

nome = c("Hotel Ronda", "Villad'Este", "Hotel Lisboa", "Hotel Prem", "Hotel d'Europa", 
         "Palace Luzern", "Hotel Palace", "Hotel Arts", "Hotel Sacher", "Duc de Bourgogne",
         "Villa Gallici", "Hotel Vila")

pais = c("Espanha", "Itália", "Portugal", "Alemanha", "França", "França", "Portugal", "Espanha",
         "Alemanha", "França", "França", "Portugal")
                     
preco = c("$$","$$$$","$","$","$$","$$","$$$$","$$$","$$$","$","$$","$$")

quartos=c(18, 166, 81, 54, 47, 326, 185, 45, 120, 10, 22, 233)

pontuacao = c(8.4, 8.6, 8.5, 7.7, 7.6, 8.1, 9.5, 7.3, 8.5, 7.6, 9.0, 9.1)

# construir a tabela

hoteis = data.frame(Nome=nome, Pais=pais, Preco=preco, Nquartos=quartos, Pontuacao=pontuacao) 

# ver a tabela
View(hoteis)

# guardar a tabela como num ficheiro txt

write.table(hoteis, file="hoteis.txt", row.names=FALSE)

##############################################################
# 2 (c)

(n2 = nrow(hoteis))

##############################################################
# 2 (d)

# variável estatística: país
(ni21 = table(hoteis$Pais))          # frequências absolutas
(fi21 = prop.table(ni21))            # frequências relativas

(tabela.frequencias.pais = data.frame(i=1:nrow(ni21),
                                      xi=names(ni21),
                                      ni=as.integer(ni21),
                                      fi=round(as.numeric(fi21),4)))

# variável estatística: preço do quarto
(ni22 = table(hoteis$Preco))          # frequências absolutas
(fi22 = prop.table(ni22))            # frequências relativas
(Ni22 = cumsum(ni22))                # frequências absolutas acumuladas
(Fi22 = cumsum(fi22))                # frequências relativas acumuladas

(tabela.frequencias.preco = data.frame(i=1:nrow(ni22),
                                       xi=names(ni22),
                                       ni=as.integer(ni22),
                                       fi=round(as.numeric(fi22),4),
                                       Ni=as.integer(Ni22),
                                       Fi=round(as.numeric(Fi22),4)))

##############################################################
# 2 (e)

tabela.frequencias.pais$ni[2]+tabela.frequencias.pais$ni[5]

#ou
sum(tabela.frequencias.pais$ni[c(2,5)])

#####
#ou 
#sem usar a tabela de frequências

# %in% pertencer ao conjunto
nrow(hoteis[hoteis$Pais %in% c("Espanha", "Portugal"),])

# | representa ou
nrow(hoteis[hoteis$Pais=="Espanha" | hoteis$Pais=="Portugal",])


##############################################################
# 2 (f)

tabela.frequencias.preco$fi[4]*100

#####
#ou 
#sem usar a tabela de frequências
round((nrow(hoteis[hoteis$Preco=="$$$$",])/n2)*100,2)


#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# EXERCÍCIO 3

#dados
#ler os dados do ficheiro: File -> Import Datset -> From Text (base)

stroke

##############################################################
# 3 (b)

#dimensão da amostra

(n3 = nrow(stroke))

##############################################################
# 3 (d)

# variável estatística: sex
(ni31 = table(stroke$sex))            # frequências absolutas
(fi31 = round(prop.table(ni31),4))   # frequências relativas

(tabela.frequencias.sex = data.frame(i=1:nrow(ni31),
                                       xi=c("0 = masculino","1 = feminino"),
                                       ni=as.integer(ni31),
                                       fi=as.numeric(fi31)))


# variável estatística: dgn
(ni32 = table(stroke$dgn))            # frequências absolutas
(fi32 = round(prop.table(ni32),4))   # frequências relativas

(tabela.frequencias.dgn = data.frame(i=1:nrow(ni32),
                                      xi=names(ni32),
                                      ni=as.integer(ni32),
                                      fi=as.numeric(fi32)))


# variável estatística: coma
(ni33 = table(stroke$coma))            # frequências absolutas
(fi33 = round(prop.table(ni33),4))   # frequências relativas

(tabela.frequencias.coma = data.frame(i=1:nrow(ni33),
                                      xi=names(ni33),
                                      ni=as.integer(ni33),
                                      fi=as.numeric(fi33)))


# variável estatística: diab
(ni34 <- table(stroke$diab))            # frequências absolutas
(fi34 = round(prop.table(ni34),4))    # frequências relativas

(tabela.frequencias.diab = data.frame(i=1:nrow(ni34),
                                      xi=names(ni34),
                                      ni=as.integer(ni34),
                                      fi=as.numeric(fi34)))


# variável estatística: dead
(ni35 = table(stroke$dead))            # frequências absolutas
(fi35 = round(prop.table(ni35),4))    # frequências relativas

(tabela.frequencias.dead = data.frame(i=1:nrow(ni35),
                                       xi=names(ni35),
                                       ni=as.integer(ni35),
                                       fi=as.numeric(fi35)))


##############################################################
# 3 (e)

tabela.frequencias.dgn$ni[2]

#####
#ou 
#sem usar a tabela de frequências

nrow(stroke[stroke$dgn=="ID",])


##############################################################
# 3 (f)

tabela.frequencias.dead$fi[2]*100

#####
#ou 
#sem usar a tabela de frequências

round((nrow(stroke[stroke$dead==TRUE,])/n3)*100,2)

##############################################################
# 3 (g)

# existem falta de dados na variável coma
sum(stroke$coma==2 & stroke$diab=="Yes", na.rm=TRUE)

#ou
stroke2 = na.omit(stroke)
nrow(stroke2[stroke2$coma==2 & stroke2$diab=="Yes",])


##############################################################
# 3 (h)

nrow(stroke[stroke$obsmonths==0.1,])


#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

# EXERCÍCIO 4

#dados
#ler os dados do ficheiro: File -> Import Datset -> From EXCEL

obesidade

##############################################################
# 4 (b)

(n4 = nrow(obesidade))


##############################################################
# 4 (c)

# variável estatística: FAVC
(ni41 = table(obesidade$FAVC))       # frequências absolutas
(fi41 = round(prop.table(ni41),4))   # frequências relativas

(tabela.frequencias.FAVC = data.frame(i=1:nrow(ni41),
                                        xi=names(ni41),
                                        ni=as.integer(ni41),
                                        fi=as.numeric(fi41)))

# percentagem de pessoas inquiridas que come alimentos altamente calóricos habitualmente
tabela.frequencias.FAVC$fi[2]*100

#####
#ou 
#sem usar a tabela de frequências
round((nrow(obesidade[obesidade$FAVC==1,])/n4)*100,2)


##############################################################
# 4 (d)

# variável estatística: CAEC

# colocar as categorias da variável por ordem
obesidade$CAEC = factor(obesidade$CAEC, levels=c("N", "S", "F", "A"))

(ni42 = table(obesidade$CAEC))       # frequências absolutas
(fi42 = round(prop.table(ni42),4))   # frequências relativas
(Ni42 = cumsum(ni42))                # frequências absolutas acumuladas
(Fi42 = round(cumsum(fi42),4))       # frequências relativas acumuladas

(tabela.frequencias.CAEC = data.frame(i=1:nrow(ni42),
                                       xi=names(ni42),
                                       ni=as.integer(ni42),
                                       fi=as.numeric(fi42),
                                       Ni=as.integer(Ni42),
                                       Fi=as.numeric(Fi42)))

# percentagem de pessoas inquiridas que come entre as refeições principais
(1-tabela.frequencias.CAEC$fi[1])*100

#####
#ou 
#sem usar a tabela de frequências

# != diferente
round((nrow(obesidade[obesidade$CAEC!="N",])/n4)*100,2)

# %in% pertencer ao conjunto
round((nrow(obesidade[obesidade$CAEC %in% c("S", "F", "A"),])/n4)*100,2)


##############################################################
# 4 (e)

# variável estatística: MTRANS

(ni43 = table(obesidade$MTRANS))     # frequências absolutas
(fi43 = round(prop.table(ni43),4))   # frequências relativas

(tabela.frequencias.MTRANS = data.frame(i=1:nrow(ni43),
                                         xi=names(ni43),
                                         ni=as.integer(ni43),
                                         fi=as.numeric(fi43)))

# percentagem de pessoas inquiridas que que utiliza habitualmente bicicleta ou transportes públicos
sum(tabela.frequencias.MTRANS$fi[c(3,5)])*100

#####
#ou 
#sem usar a tabela de frequências

# %in% pertencer ao conjunto
round((nrow(obesidade[obesidade$MTRANS %in% c("Transportes_Publicos", "Bicicleta"),])/n4)*100,2)

# | representa ou
round((nrow(obesidade[obesidade$MTRANS=="Transportes_Publicos" | obesidade$MTRANS=="Bicicleta",])/n4)*100,2)


##############################################################
# 4 (f)

# variável estatística: NCP

(ni44 = table(obesidade$NCP))        # frequências absolutas
(fi44 = round(prop.table(ni44),4))   # frequências relativas
(Ni44 = cumsum(ni44))                # frequências absolutas acumuladas
(Fi44 = round(cumsum(fi44),4))       # frequências relativas acumuladas

(tabela.frequencias.NCP = data.frame(i=1:nrow(ni44),
                                      xi=names(ni44),
                                      ni=as.integer(ni44),
                                      fi=as.numeric(fi44),
                                      Ni=as.integer(Ni44),
                                      Fi=as.numeric(Fi44)))


##############################################################
# 4 (g) i.

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

(obesidade$classes1 = cut(obesidade$Idade, breaks=cortes1, right=TRUE, include.lowest=TRUE))

View(obesidade[,c("Idade","classes1")])

# tabela de frequências
(ni45 = table(obesidade$classes1))   # frequências absolutas
(fi45 = round(prop.table(ni45),4))   # frequências relativas
(Ni45 = cumsum(ni45))                # frequências absolutas acumuladas
(Fi45 = round(cumsum(fi45),4))       # frequências relativas acumuladas

(tabela.frequencias.Idade = data.frame(i=1:nrow(ni45),
                                        classes=names(ni45),
                                        ni=as.integer(ni45),
                                        fi=as.numeric(fi45),
                                        Ni=as.integer(Ni45),
                                        Fi=as.numeric(Fi45)))


##############################################################
# 4 (g) ii.

# variável estatística: Altura

# mínimo e máximo dos dados
min(obesidade$Altura)
max(obesidade$Altura)

# 3 classes: [1.45,1.60], ]1.60,1.80] e ]1.80,2.00]
#extremos das classes
cortes2 = c(1.45,1.60,1.80,2.00)

# intervalos abertos à esquerda e fechados à direita
# a primeira classe fechada dos dois lados
(obesidade$classes2 = cut(obesidade$Altura, breaks=cortes2, right=TRUE, include.lowest=TRUE))

View(obesidade[,c("Altura","classes2")])


# tabela de frequências
(ni46 = table(obesidade$classes2))               # frequências absolutas
(fi46 = round(prop.table(ni46),4))   # frequências relativas
(Ni46 = cumsum(ni46))                # frequências absolutas acumuladas
(Fi46 = round(cumsum(fi46),4))       # frequências relativas acumuladas

(tabela.frequencias.Altura = data.frame(i=1:nrow(ni46),
                                         classes=names(ni46),
                                         ni=as.integer(ni46),
                                         fi=as.numeric(fi46),
                                         Ni=as.integer(Ni46),
                                         Fi=as.numeric(Fi46)))


##############################################################
# 4 (g) iii.

# variável estatística: Peso

# dimensão da amostra
n4

# mínimo e máximo dos dados
min(obesidade$Peso)
max(obesidade$Peso)

(k3 = trunc(1+log(n4)/log(2)))   # número de classes
(h3 =  (max(obesidade$Peso)-min(obesidade$Peso))/k3)   # amplitude das classes

# mínimo e máximo das classes
(valor.min3 = min(obesidade$Peso))
(valor.max3 = valor.min3 + h3*k3)

#extremos das classes
(cortes3 = seq(valor.min3, valor.max3, by=h3))

# intervalos fechados à esquerda e abertos à direita
# como o máximo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados
(obesidade$classes3 = cut(obesidade$Peso, breaks=cortes3, right=FALSE, include.lowest=TRUE))

View(obesidade[,c("Peso","classes3")])


# tabela de frequências
(ni47 = table(obesidade$classes3))                # frequências absolutas
(fi47 = round(prop.table(ni47),4))   # frequências relativas
(Ni47 = cumsum(ni47))                # frequências absolutas acumuladas
(Fi47 = round(cumsum(fi47),4))       # frequências relativas acumuladas

(tabela.frequencias.Peso = data.frame(i=1:nrow(ni47),
                                        classes=names(ni47),
                                        ni=as.integer(ni47),
                                        fi=as.numeric(fi47),
                                        Ni=as.integer(Ni47),
                                        Fi=as.numeric(Fi47)))


##############################################################
# 4 (h)

sum(tabela.frequencias.Idade$ni[6:8])

#####
#ou 
#sem usar a tabela de frequências
nrow(obesidade[obesidade$Idade>44,])


##############################################################
# 4 (i)

sum(tabela.frequencias.Altura$fi[1:2])*100

#####
#ou 
#sem usar a tabela de frequências
round((nrow(obesidade[obesidade$Altura<=1.80,])/n4)*100,2)

