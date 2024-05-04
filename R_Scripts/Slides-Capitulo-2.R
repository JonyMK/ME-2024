#############################################
#                                           #
#    Capítulo 2 - Distribuíções Teóricas    #
#                                           #
#############################################

#########################
# exemplo 18 - página 135


##############################################################################

# alínea (1) cálculos do integral

f1 = function(x){x/4}
f2 = function(x){1-(x/4)}

0 + integrate(f1, lower=0, upper=2)$value + integrate(f2, lower=2, upper=4)$value +0

##############################################################################

# alínea (2)

integrate(f1, lower=1, upper=2)$value + integrate(f2, lower=2, upper=3)$value

##############################################################################

# alínea (4) - resolução com base na função densidade de probabilidade

# a) c)
0 + integrate(f1, lower=0, upper=1)$value
# b)
integrate(f1, lower=1, upper=1)$value
# d)
integrate(f1, lower=1, upper=2)$value + integrate(f2, lower=2, upper=4)$value + 0
# e) f) g) h)
integrate(f1, lower=1, upper=2)$value + integrate(f2, lower=2, upper=3)$value


##############################################################################

# alínea (6)

f4 = function(x){x*(x/4)}
f5 = function(x){x*(1-x/4)}

#E[X]
media = integrate(f4, lower=0, upper=2)$value + integrate(f5, lower=2, upper=4)$value
media

##############################################################################

# alínea (8)

f6 = function(x){x^2*(x/4)}
f7 = function(x){x^2*(1-x/4)}

#E[X^2]
EX2 =  integrate(f6, lower=0, upper=2)$value + integrate(f7, lower=2, upper=4)$value
EX2

#variância
variancia = EX2 - media^2
variancia

##############################################################################

# alínea (9)

desvioPadrao = sqrt(variancia)
desvioPadrao
