#########################
#  2.º TESTE 2023-2024  #
#########################

#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text
# colocar: Heading Yes

distonia2
str(distonia2)


##############################################################
##############################################################

# 1 (a)

# estimativa pontual para média da idade dos pacientes
mean(distonia2$age)

#estimativa pontual para o desvio padrão  da idade dos pacientes
sd(distonia2$age)

#estimativa pontual para a percentagem de pacientes que foram injetados com um placebo
prop.table(table(distonia2$treat))[1]*100
#ou
placebo = distonia2[distonia2$treat==1,]
(nrow(placebo)/nrow(distonia2))*100

##############################################################
##############################################################

# 1 (b)

# teste de ajustamento
ks.test(distonia2$age, "pnorm", mean=57, sd=15)

##############################################################
##############################################################

# 1 (c)

# filtrar os dados
feminino = distonia2[distonia2$sex==1,]

#amostra: idade dos pacientes do género feminino
feminino$age
#dimensão da amostra
(nF = length(feminino$age))

#TH para a média
library(BSDA)
(TH.1c = z.test(x=feminino$age, sigma.x=sd(feminino$age), alternative="greater", mu=55))
#valor observado da Estatística de teste sob a hipótese H0
TH.1c$statistic
#OU
(Zobs = (mean(feminino$age)-55)/(sd(feminino$age)/sqrt(nF)))

#quantil de probabilidade da Distribuição Normal Reduzida para a região crítica
qnorm(1-0.03)


##############################################################
##############################################################

# 1 (d)

# filtrar os dados
botoxB = distonia2[distonia2$treat==2,]

#amostras
(antes = botoxB$twstrs_1)
(depois = botoxB$twstrs_2)

#Teste de Wilcoxon: D = depois - antes
wilcox.test(x=depois, y=antes, alternative="less", mu=0, paired=TRUE)


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (a)
# amostras

MColorido = c(502, 488, 494, 481, 497, 488, 494, 489)
(nx = length(MColorido))

MPretoBranco = c(510, 498, 512, 497, 494, 495, 508)
(ny = length(MPretoBranco))


##############################################################
##############################################################

# 2 (a) (i)

# teste de ajustamento para X
shapiro.test(MColorido)

# teste de ajustamento para Y
shapiro.test(MPretoBranco)


##############################################################
##############################################################

# 2 (a) (ii)

#TH para o quociente de variâncias
(TH.2aii = var.test(x=MColorido, y=MPretoBranco, ratio = 1, alternative="two.sided"))
#valor observado da Estatística de teste sob a hipótese H0
TH.2aii$statistic
#OU
(Fobs = (var(MColorido)/var(MPretoBranco))*1)

#quantil de probabilidade da Distribuição F-Snedecor para a região crítica
qf(0.05/2,nx-1,ny-1)
qf(1-0.05/2,nx-1,ny-1)


##############################################################
##############################################################

# 2 (a) (iii)

#TH para a diferença de médias
(TH.2aiii = t.test(x=MColorido, y=MPretoBranco, alternative="greater", mu=0, 
                    paired=FALSE, var.equal=TRUE))
#valor-p
TH.2aiii$p.value

#ou

#valor observado da Estatística de teste sob a hipótese H0
TH.2aiii$statistic
#valor-p -> teste unilateral direito
1-pt(TH.2aiii$statistic, nx+ny-2)


##############################################################
##############################################################

# 2 (a) (iv)

#dimensão da amostra
ny
# variância amostral
var(MPretoBranco)

# cálculo do grau de confiança
(valor = ((ny-1)*var(MPretoBranco))/4.8662^2)
pchisq(valor,ny-1)
(alfa = 2*(1-pchisq(valor,ny-1)))  # nível de significância
1-alfa  # grau de confiança

# valor de k
sqrt(((ny-1)*var(MPretoBranco))/qchisq(alfa/2,ny-1))


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (b)
# proporção populacional

p = 0.08

##############################################################
##############################################################

# 2 (b) (i)

(valor2 = (0.10-0.08)/sqrt(0.08*(1-0.08)/250))
1-pnorm(valor2)

##############################################################
##############################################################

# 2 (b) (ii)

qnorm(1-0.01/2)
(qnorm(1-0.01/2)*0.5/0.02)^2

##############################################################
##############################################################

# 2 (b) (iii)

#amostras
(homens = c(rep(1,75),rep(0,850-75)))
(mulheres = c(rep(1,5),rep(0,2000-5)))

# desvios padrão
(s1 = sqrt(mean(homens)*(1-mean(homens))))
(s2 = sqrt(mean(mulheres)*(1-mean(mulheres))))

# IC a 90% para p1-p2
library(BSDA)
(IC.2biii = z.test(x=homens, y=mulheres, sigma.x=s1, sigma.y=s2, conf.level=0.90))
IC.2biii$conf.int
# transforma r em percentagens
IC.2biii$conf.int*100

#ou

#limite inferior do IC
((75/850)-(5/2000))-qnorm(1-0.10/2)*sqrt(((75/850)*(1-(75/850))/850)+((5/2000)*(1-(5/2000))/2000))
#limite superior do IC
((75/850)-(5/2000))+qnorm(1-0.10/2)*sqrt(((75/850)*(1-(75/850))/850)+((5/2000)*(1-(5/2000))/2000))
