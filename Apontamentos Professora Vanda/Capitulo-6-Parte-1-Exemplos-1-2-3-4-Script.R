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

# H0:X segue uma distribuição Uniforme discreta
# contra
# H1:X NÃO segue uma distribuição Uniforme discreta


#Domínio
(xi<- c(1,2,3,4,5,6))

# número de linhas da tabela de frequências
(k<-length(xi))

#frequências observadas (frequências absolutas, ou seja, os ni)
(Oi=c(46,35,25,19,40,45))

# dimensão da amostra
(n <- sum(Oi))

# probabilidades necessárias para as frequências esperadas
(pi =rep(1/6,k))

r <- 0 # não foi necessário estimar parâmetros
(gl <- k-1-r)  # graus de liberdade da distribuião Qui-Quadrado

#teste de ajustamento do Qui-Quadrado -> neste caso todos os resultados estão certos pois r=0
chisq.test(x=Oi,p=pi)


#dar um nome para poder ver os diversos campos
exemplo.1 <- chisq.test(x=Oi,p=pi)
exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value  # valor-p
exemplo.1$observed  # Oi
exemplo.1$expected # Ei=npi


#################
#################

# verificar as regras recomendadas

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

#cálculo do quantil de probabilidade da Região critíca
qchisq(0.95, gl)

#cálculo do valor-p
1-pchisq(exemplo.1$statistic, gl)



#######################################################################################
#######################################################################################

#######################################################################

# Exemplo 2

# H0:X segue uma distribuição Binomial com n=4 e p=0.6
# contra
# H1:X NÃO segue uma distribuição Binomial com n=4 e p=0.6


#Domínio: temos que colocar todos os valores possiveis,
#        mesmo que não se tenham observado

(xi<- c(0,1,2,3,4))  

# número de linhas da tabela de frequências
(k<-length(xi))

#frequências observadas (frequências absolutas)
(Oi=c(0,5,12,19,14))

# dimensão da amostra
(n <- sum(Oi))

# probabilidades necessárias para as frequências esperadas
(pi =dbinom(xi,4,0.6))

r <- 0 # não foi necessário estimar parâmetros
(gl <- k-1-r)  # graus de liberdade da distribuião Qui-Quadrado

#teste de ajustamento do Qui-Quadrado -> neste caso todos os resultados estão certos pois r=0
chisq.test(x=Oi,p=pi)


#dar um nome para poder ver os diversos campos
exemplo.2 <- chisq.test(x=Oi,p=pi)
exemplo.2$statistic # Qobs
exemplo.2$parameter # graus de liberdade
exemplo.2$p.value  # valor-p
exemplo.2$observed  # Oi
exemplo.2$expected # Ei=npi


#################
#################

# verificar as regras recomendadas

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

#cálculo do quantil de probabilidade da Região critíca
qchisq(0.95, gl)

#cálculo do valor-p
1-pchisq(exemplo.1$statistic, gl)



#######################################################################################
#######################################################################################





# Exemplo 3.1

# H0:X segue uma distribuição de Poisson de parâmetro 2.1
# contra
# H1:X NÃO segue uma distribuição de Poisson de parâmetro 2.1


#Domínio -> atenção que o 5 representa maior ou igual 5 (devido ao domínio da Poisson)
(xi<- c(0,1,2,3,4,5))  

# número de linhas da tabela de frequências
(k<-length(xi))

#frequências observadas (frequências absolutas)
(Oi=c(14,22,18,15,10,9))

# dimensão da amostra
(n <- sum(Oi))

######
r <- 0  # não se estimou parâmetro
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi=c(dpois(0,2.1),dpois(1,2.1),dpois(2,2.1),dpois(3,2.1),dpois(4,2.1))) #mais trabalhosa

#ou da seguinte forma, que faz para qualquer valor
(pi =dpois(xi,2.1))

(pi[k] <- 1-ppois(xi[k-1],2.1))  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

  #teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.3 <- chisq.test(x=Oi,p=pi)

exemplo.3$statistic # Qobs
exemplo.3$parameter # graus de liberdade -> errado
exemplo.3$p.value  # valor-p -> errado
exemplo.3$observed  # Oi
exemplo.3$expected # Ei=npi

#################
#################

# verificar as regras recomendadas

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra de que n>30")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.3$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra (Ei>=1 para todos)")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.3$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra de não haver mais de 20% das frequências Esperadas < 5")
}

#################
#################

#cálculo do quantil de probabilidade da a Região critíca
qchisq(1-0.01, gl)





# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp <- 1-pchisq(exemplo.2$statistic, gl)
valorp

#cálculo do quantil de probabilidade da a Região critíca
qchisq(0.99, gl)



#######################################################################################
#######################################################################################

# Exemplo 3.2

# H0:X segue uma distribuição de Poisson 
# contra
# H1:X NÃO segue uma distribuição de Poisson 


#Domínio -> atenção que o 5 representa maior ou igual 5 (devido ao domínio da Poisson)
(xi<- c(0:5)) # alternativa a xi<-c(0,1,2,3,4,5)

# número de linhas da tabela de frequências
(k<-length(xi))

#frequências observadas
(Oi=c(14,22,18,15,10,9))

# dimensão da amostra
(n <- sum(Oi))

#estimar a média
(elx<-sum(xi*Oi)/n)
   #ou pode-se criar uma base de dados do exercício e calcular a média a partir dessa base de dados
    amostra <- c(rep(0,14), rep(1,22), rep(2,18), rep(3,15), rep(4,10), rep(5,9))  
    amostra
    (elx <- mean(amostra))
######

r <- 1  # estimou-se um parâmetro
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

# probabilidades necessárias para as frequências esperadas
(pi =dpois(xi,elx))
pi[k] <- 1-ppois(xi[k-1],elx)  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.32 <- chisq.test(x=Oi,p=pi)
exemplo.32$statistic # Qobs
exemplo.32$parameter # graus de liberdade -> errado
exemplo.32$p.value  # valor-p -> errado
exemplo.32$observed  # Oi
exemplo.32$expected # Ei=npi

#################
#################

# verificar as regras recomendadas

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

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp <- 1-pchisq(exemplo.32$statistic, gl)
valorp

#cálculo do quantil de probabilidade da a Região critíca
qchisq(0.99, gl)







# Exemplo 4

# H0:X segue uma distribuição de Poisson
# contra
# H1:X NÃO segue uma distribuição de Poisson


#Domínio -> atenção que o 5 representa maior ou igual 5 (devido ao domínio da Poisson)
(xi<- c(0:5))

# número de linhas da tabela de frequências
(k<-length(xi))

#frequências observadas
(Oi=c(6,14,10,7,2,1))

# dimensão da amostra
(n <- sum(Oi))

# é necessário estimar o lambda -> média amostral
(elx <- sum(xi*Oi)/n)
#ou
amostra <- c(rep(0,6), rep(1,14), rep(2,10), rep(3,7), rep(4,2), rep(5,1))
(elx <- mean(amostra))


# probabilidades necessárias para as frequências esperadas
(pi =dpois(xi,elx))
pi[k] <- 1-ppois(xi[k-1],elx)  #P(X>=5)  # corrigir a última
sum(pi)
round(pi,4)

#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi,p=pi)

#dar um nome para poder ver os diversos campos
exemplo.4 <- chisq.test(x=Oi,p=pi)
exemplo.4$expected

#################
#################

# verificar as regras recomendadas

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
  print("juntar linhas da tabela de frequencias")
}else{
  print("respeita a regra")
}


############################
############################

# juntar linhas, as duas últimas linhas (são as que têm as frequências que não respeitam uma das regras)

## Nota: 6*0.2=1.2 pode haver no máximo 1 linha com Ei<5

exemplo.4$expected # Ei=npi e verificar quais as linhas a juntar -> as duas ultimas

# perde 1 linha
k2 <- k-1

(gl2 <- k2-1-r)


#Domínio -> atenção que o 4 representa maior ou igual 4 (devido ao domínio da Poisson)
(xi2<- xi[1:k2])

#frequências observadas
(Oi2=c(Oi[1:k2-1],Oi[k2]+Oi[k2+1]))
  #em alternativa, bastava escrever Oi com a soma das duas ultimas linhas
  (Oi2=c(6,14,10,7,2+1))

# probabilidades necessárias para as frequências esperadas
(pi2 =pi[1:k2])
pi2[k2] <- 1-ppois(xi2[k2-1],elx)  #P(X>=4)
sum(pi2)
round(pi2,4)


#teste de ajustamento do Qui-Quadrado
chisq.test(x=Oi2,p=pi2)

exemplo.4alt <- chisq.test(x=Oi2,p=pi2)
exemplo.4alt$expected
#################
#################

# verificar as regras recomendadas
# verificar só a que falhou anteriormente

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.4alt$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#################
#################

# os graus de liberdade e o valor-p estão errados pois não tem em consideração 
# que os parâmetros tiveram de ser estimados (r=1)

# valor-p correto
valorp2 <- 1-pchisq(exemplo.4alt$statistic, gl2)
valorp2

#cálculo do quantil de probabilidade da a Região critíca
qchisq(0.99, gl2)



#######################################################################################
#######################################################################################
