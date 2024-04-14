# Exemplo 15

# 15.1
dbinom(3, 5, 0.1)

# 15.2
(1 - pbinom(2, 5, 0.1))

# 15.3
qbinom(0.75, 5, 0.1)

# 15.5
(dbinom(4, 9, 0.1) + dbinom(5, 9, 0.1) + dbinom(6, 9, 0.1))
# OU
(pbinom(6, 9, 0.1) - pbinom(3, 9, 0.1))
