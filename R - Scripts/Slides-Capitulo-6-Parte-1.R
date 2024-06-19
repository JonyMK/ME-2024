########################
# CAPÍTULO 6 (PARTE 1) #
########################


#######################################################################
#######################################################################
#######################################################################

#           Teste de Ajustamento do Qui-Quadrado                      #

#######################################################################
#######################################################################
#######################################################################

# função -> chisq.test()
# argumentos da função: Oi e pi

# os resultados só estão todos certos se r=0

#######################################################################

# Exemplo 1

# H0:X segue uma distribuição Uniforme Discreta(6)
# contra
# H1:X NÃO segue uma distribuição Uniforme Discreta(6)


#frequências observadas
(Oi = c(46, 35, 25, 19, 40, 45))

# probabilidades necessárias para as frequências esperadas
(pi = rep(1/6, 6))
sum(pi)  # tem de dar 1


r = 0 # não foi necessário estimar parâmetros

#teste de ajustamento do Qui-Quadrado -> neste caso todos os resultados estão certos pois r=0
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.1 = chisq.test(x=Oi,p=pi)
exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value  # valor-p
exemplo.1$observed  # Oi
exemplo.1$expected # Ei=npi


#################
#################

# verificar as regras recomendadas
(n = sum(Oi))     # dimensão da amostra
(k = length(Oi))  # número de linhas da tabela de frequências


# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.1$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.1$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}


#################
#################

# nível de significância
alfa = 0.05

# graus de liberdade
(gl = exemplo.1$parameter)
#ou
(gl = k-1-r)


#cálculo do quantil de probabilidade da Região crítica
qchisq(1-alfa, gl)

#cálculo do valor-p
1-pchisq(exemplo.1$statistic, gl)


#######################################################################################
#######################################################################################
# Exemplo 2

# H0:X segue uma distribuição Binomial(4,0.6)
# contra
# H1:X NÃO segue uma distribuição Binomial(4,0.6)


#frequências observadas
(Oi = c(0, 5, 12, 19, 14))

# número de linhas da tabela de frequências
(k = length(Oi))

# probabilidades necessárias para as frequências esperadas
(pi = dbinom(0:4,4,0.6))
sum(pi)  # tem de dar 1

r = 0 # não foi necessário estimar parâmetros

#teste de ajustamento do Qui-Quadrado -> neste caso todos os resultados estão certos pois r=0
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.2 = chisq.test(x=Oi,p=pi)
exemplo.2$statistic # Qobs
exemplo.2$parameter # graus de liberdade
exemplo.2$p.value  # valor-p
exemplo.2$observed  # Oi
exemplo.2$expected # Ei=npi


#################
#################

# verificar as regras recomendadas
(n = sum(Oi))     # dimensão da amostra
k                 # número de linhas da tabela de frequências

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.2$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.2$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}


#################
#################

# nível de significância
alfa = 0.05
#cálculo do quantil de probabilidade da Região crítica
qchisq(1-alfa, exemplo.2$parameter)

#cálculo do valor-p
1-pchisq(exemplo.2$statistic, exemplo.2$parameter)


#######################################################################################
#######################################################################################

# Exemplo 3.1

# H0:X segue uma distribuição de Poisson(2.1)
# contra
# H1:X NÃO segue uma distribuição de Poisson(2.1)


#frequências observadas
(Oi = c(14,22,18,15,10,9))

# número de linhas da tabela de frequências
(k = length(Oi))


r = 0 # não foi necessário estimar parâmetros

# probabilidades necessárias para as frequências esperadas
(pi =c(dpois(0:4,2.1), 1-ppois(4,2.1)))  #a última posição do vetor é P(X>=5)
sum(pi)  # tem de dar 1

#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.31 = chisq.test(x=Oi,p=pi)

exemplo.31$statistic # Qobs
exemplo.31$parameter # graus de liberdade
exemplo.31$p.value  # valor-p
exemplo.31$observed  # Oi
exemplo.31$expected # Ei=npi

#################
#################

# verificar as regras recomendadas
(n = sum(Oi))     # dimensão da amostra
k                 # número de linhas da tabela de frequências


# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.31$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.31$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#################
#################

# nível de significância
alfa = 0.01
#cálculo do quantil de probabilidade da Região crítica
qchisq(1-alfa, exemplo.31$parameter)

#cálculo do valor-p
1-pchisq(exemplo.31$statistic, exemplo.31$parameter)


#######################################################################################
#######################################################################################

# Exemplo 3.2

# H0:X segue uma distribuição de Poisson
# contra
# H1:X NÃO segue uma distribuição de Poisson


#frequências observadas
(Oi = c(14,22,18,15,10,9))

# número de linhas da tabela de frequências
(k = length(Oi))

# é necessário estimar o lambda -> média amostral
amostra = c(rep(0,14), rep(1,22), rep(2,18), rep(3,15), rep(4,10), rep(5,9))
(elx = mean(amostra))

r = 1  # estimou-se 1 parâmetro
(gl = k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =c(dpois(0:4,elx), 1-ppois(4,elx)))  #a última posição do vetor é P(X>=5)
sum(pi)


#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.32 = chisq.test(x=Oi,p=pi)

exemplo.32$statistic # Qobs
exemplo.32$parameter # graus de liberdade -> errado
exemplo.32$p.value  # valor-p -> errado
exemplo.32$observed  # Oi
exemplo.32$expected # Ei=npi

#################
#################

# verificar as regras recomendadas
(n = sum(Oi))     # dimensão da amostra
k                 # número de linhas da tabela de frequências

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.32$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.32$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#################
#################

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp = 1-pchisq(exemplo.32$statistic, gl)
valorp

# nível de significância
alfa = 0.01
#cálculo do quantil de probabilidade da a Região crítica
qchisq(1-alfa, gl)



#######################################################################################
#######################################################################################

# Exemplo 4

# H0:X segue uma distribuição de Poisson
# contra
# H1:X NÃO segue uma distribuição de Poisson


#frequências observadas
(Oi = c(6,14,10,7,2,1))

# número de linhas da tabela de frequências
(k = length(xi))


# é necessário estimar o lambda -> média amostral
amostra = c(rep(0,6), rep(1,14), rep(2,10), rep(3,7), rep(4,2), rep(5,1))
(elx = mean(amostra))


r = 1  # estimou-se 1 parâmetro
(gl = k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =c(dpois(0:4,elx), 1-ppois(4,elx)))  #a última posição do vetor é P(X>=5)
sum(pi)


#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.4 = chisq.test(x=Oi,p=pi)

#################
#################

# verificar as regras recomendadas

# dimensão da amostra maior que 30
(n = sum(Oi))     # dimensão da amostra
k                 # número de linhas da tabela de frequências


# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.4$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.4$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

exemplo.4$expected
# juntar linhas, as duas últimas linhas 
# (são as que têm as frequências que não respeitam a regra)

############################
############################


# perde 1 linha
k2 = k-1

#frequências observadas
(Oi2=c(Oi[1:4],Oi[5]+Oi[6]))

# probabilidades necessárias para as frequências esperadas
(pi2 = c(dpois(0:3,elx), 1-ppois(3,elx)))  #a última posição do vetor é P(X>=4)
sum(pi2)


#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi2,p=pi2)

exemplo.42 = chisq.test(x=Oi2,p=pi2)

#################
#################

# verificar as regras recomendadas
# verificar só a que falhou anteriormente

# não há mais de 20% das frequências Esperadas < 5
# 20% das frequências
if(length(which(exemplo.42$expected<5))>(k2*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#################
#################

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

#graus de liberdade
(gl2 = k2-1-r)

# valor-p correto
valorp2 = 1-pchisq(exemplo.42$statistic, gl2)
valorp2


# nível de significância
alfa = 0.01
#cálculo do quantil de probabilidade da a Região crítica
qchisq(1-alfa, gl2)



#######################################################################################
#######################################################################################

# Exemplo 5

# H0:X segue uma distribuição Exponencial
# contra
# H1:X Não segue uma distribuição Exponencial

amostra = c(1476,300,98,221,157,
             182,499,552,1563,36,
             246,442,20,796,31,
             47,438,400,279,247,
             210,284,553,767,1297,
             214,428,597,2025,185,
             467,401,210,289,1024)


# Regra de Sturges

# dimensão da amostra
(n = length(amostra))
# número de classes ou de linhas da tabela de frequências
(k = trunc(1+log(n)/log(2)))
# amplitude de cada classe
(h = (max(amostra)-min(amostra))/k)
# valor mínimo das classes
(valor.min = min(amostra))
# valor máximo das classes
(valor.max = valor.min + h*k)

#extremos das classes
(cortes = seq(valor.min, valor.max, by=h))
#definir as classes
(classes = cut(amostra, breaks=cortes, 
                right=TRUE, include.lowest=TRUE, dig.lab = 5))

# frequências Observadas
(Oi = table(classes))

# é necessário estimar o theta -> média amostral
(elx = mean(amostra))

r = 1  # estimou-se 1 parâmetro
(gl = k-1-r)  # graus de liberdade da distribuição Qui-Quadrado


# probabilidades necessárias para as frequências esperadas
pi = numeric(k)
pi[1] = pexp(cortes[2],1/elx)

for (i in 2:(k-1)) {
  pi[i] = pexp(cortes[i+1], 1/elx) - pexp(cortes[i], 1/elx)
}

pi[k] = 1-pexp(cortes[k], 1/elx)

pi
sum(pi)


#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.5 = chisq.test(x=Oi,p=pi)

#################
#################

# verificar as regras recomendadas
n     # dimensão da amostra
k     # número de linhas da tabela de frequências

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.5$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.5$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

exemplo.5$expected
# juntar linhas, as quatro últimas linhas 
#(são as que têm as frequências que não respeitam a regra)

############################
############################

# perde 3 linhas
k2 = k-3


#limites das novas classes
(cortes2 = c(cortes[1:k2],cortes[k+1]))

#definir as novas classes
(classes2 = cut(amostra, breaks=cortes2, right=TRUE, 
                 include.lowest=TRUE, dig.lab = 5))

# frequências Observadas
(Oi2 = table(classes2))

# probabilidades necessárias para as frequências esperadas
(pi2 =c(pexp(cortes2[2],1/elx),
       pexp(cortes2[3],1/elx)-pexp(cortes2[2],1/elx),
       1-pexp(cortes2[3],1/elx)))
sum(pi2)

#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi2,p=pi2)

exemplo.52 = chisq.test(x=Oi2,p=pi2)

#################
#################

# verificar as regras recomendadas
# verificar só a que falhou anteriormente

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.52$expected<5))>(k2*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#################
#################

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# graus de liberdade
(gl2 = k2-1-r)

# valor-p correto
(valorp2 = 1-pchisq(exemplo.52$statistic, gl2))

# nível de significância
alfa = 0.10
#cálculo do quantil de probabilidade da a Região crítica
qchisq(1-alfa, gl2)


#######################################################################################
#######################################################################################
#######################################################################################

#                      Teste de Ajustamento de Kolmogorv-Smirnov                      #

#######################################################################################
#######################################################################################
#######################################################################################

# função -> ks.test()

# função -> library(nortest) lillie.test()


#######################################################################################

# EXEMPLO 6


# H0:X segue uma distribuição Exponencial de média 730 horas
# contra
# H1:X Não segue uma distribuição Exponencial  de média 730 horas

# amostra
amostra6 = c(1476,182,246,300,499,442,98,552,20,221,1563,796,157,36,31)
length(amostra6)

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(amostra6, "pexp", rate=1/730)


#######################################################################################
#######################################################################################

# EXEMPLO 7

# amostra
amostra7 = c(75, 92, 80, 80, 84, 72, 84, 77, 81,
               77, 75, 81, 80, 92, 72, 77, 78, 76,
               77, 86, 77, 92, 80, 78, 68, 78, 92,
               68, 80, 81, 87, 76, 80, 87, 77, 86,
               74, 93, 79, 81, 83, 71, 83, 78, 80,
               76, 76, 80, 82, 91, 72, 76, 79, 75)

length(amostra7)


#############
# EXEMPLO 7.1

# H0:X segue uma distribuição Normal de média 80 e desvio padrão6.95
# contra
# H1:X Não segue uma distribuição Normal de média 80 e desvio padrão 6.95


# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(amostra7, "pnorm", mean=80, sd=6.95)


#############
# EXEMPLO 7.2


# H0:X segue uma distribuição Normal
# contra
# H1:X Não segue uma distribuição Normal


# teste de Ajustamento de Lilliefors
library(nortest)
lillie.test(amostra7)


#######################################################################################
#######################################################################################
#######################################################################################

#                         Teste de Ajustamento de Shapiro-Wilk                        #

#######################################################################################
#######################################################################################
#######################################################################################

# função -> shapiro.test()


#######################################################################################

# EXEMPLO 8

amostra8 = c(75, 92, 80, 80, 84, 72, 84, 77, 81,
              77, 75, 81, 80, 92, 72, 77, 78, 76,
              77, 86, 77, 92, 80, 78, 68, 78, 92,
              68, 80, 81, 87, 76, 80, 87, 77, 86)

length(amostra8)


# H0:X segue uma distribuição Normal
# contra
# H1:X Não segue uma distribuição Normal


# teste de Ajustamento de Shapiro-Wilk
shapiro.test(amostra8)

