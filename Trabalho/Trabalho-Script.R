# Descrição das Colunas presentes na BD: ####

## ID -> Identificação da Campanha.
## plataforma -> Platataforma onde a campanha foi efetuada (Google | Facebook | Instagram).
## anuncio -> Tipo de anúncio utilizado na campanha (Banner | Vídeo | Texto).
## mercado -> Segmento de mercado alvo da campanha (Tecnologia | Moda | Alimentação).
## investimento -> Valor investido na campanha (em milhares de euros).
## cliques -> Número de cliques recebidos pela campanha.
## conversoes -> Número de conversões geradas pela campanha.

"---------------------------------------------"































































































"---------------------------------------------"

# Função para Simular Dados ####

simular_dados <- function() {
  # Parâmetros Necessários para Simular os Dados:
  n <- sample(234:254, 1)
  m <- c(10, 500, 50)
  s <- c(2, 150, 20)
  vcr <- c(sample(65:95, 1)/100, sample(65:95, 1)/100, sample(65:95, 1)/100)
  crm <- matrix(c(1, -vcr[1], vcr[2], -vcr[1], 1, -vcr[3], vcr[2], -vcr[3], 1), nrow = 3)
  dd <- MASS::mvrnorm(n, mu = m, Sigma = crm * (s %*% t(s)))
  plat <- sample(c("Google", "Facebook", "Instagram"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  ta <- sample(c("Banner", "Vídeo", "Texto"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  sm <- sample(c("Tecnologia", "Moda", "Alimentação"), n, replace = TRUE, prob = c(0.3, 0.3, 0.4))
  
  # Criar uma Tabela com os Dados
  DadosMarkDig <- data.frame(
    ID = 1:n,
    plataforma = plat,
    anuncio = ta,
    mercado = sm,
    investimento = dd[,1],
    cliques = as.integer(dd[,2]),
    conversoes = as.integer(dd[,3])
  )
  
  # Guardar a Tabela num Ficheiro .TXT
  write.table(
    DadosMarkDig,
    "DadosMarkDig.txt",
    row.names = FALSE,
    quote = FALSE
  )
}

"---------------------------------------------"

# Simulação e Geração da BD: ####

simular_dados()

"---------------------------------------------"
