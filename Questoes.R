# Questão 4 ---------------------------------------------------------------

# Crie um data frame com as seguintes informações:

# Coluna 1: nome de duas cidades, cada uma repetindo 5 vezes.

# Coluna 2: variável temperatura, escolha o valor da temperatura para as cidades

# Para cada cidade, calcule a média (mean()) e o desvio padrão (sd()) da temperatura.

# Para cada cidade, use a função summary()

# Transforme o data frame em uma matriz. O que acontece?

data_frame_cidades <- data.frame(
  cidades = rep(c("Manaus", "Santarém"), each=5),
  temperatura = runif(10,28,35)
)


subset(data_frame_cidades, cidades=="Manaus")$temperatura |> mean()
subset(data_frame_cidades, cidades=="Manaus")$temperatura |> sd()
subset(data_frame_cidades, cidades=="Manaus")$temperatura |> summary()




# Questão 5  --------------------------------------------------------------

# Crie uma matriz 3× 5 contendo todos os números consecutivos entre 16 e 30, organize por colunas.

vec1 <- c(16:30)
matriz <- matrix(c(vec1), nrow=3,ncol = 5)
print(matriz)

# Usando o Pipe
seq(16:30)|> matrix(nrow = 3,ncol = 5)
