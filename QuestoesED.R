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


# Questão 6  --------------------------------------------------------------
require("dataset")
data(CO2)
datasets::CO2
?CO2


# Questão 7  --------------------------------------------------------------
# Crie duas matrizes 2X2. Coloque uma matriz abaixo da outra. Use o comando rbind(). 
# Coloque essa matriz e a matriz criada na questão 5 em uma lista.

matriz1 <- matrix(c(1,2,3,4),nrow = 2, ncol = 2)
matriz2 <- matrix(c(5,6,7,8),nrow = 2, ncol=2)

matriz_combinada <- rbind(matriz1, matriz2)
print(matriz_combinada)

nova_lista <- list(matriz_combinada,matriz)
print(nova_lista)


# Questão 8  --------------------------------------------------------------
umidade <- seq(from = 0.4, to = 1, by = 0.1) # O mais correto, funciona como a sintaxe de um laço 



# Questão 9 ---------------------------------------------------------------

matriz3 <- matrix(c(1:1000),nrow = 100, ncol = 10)

# Criando os nomes das linhas
nomes_linhas <- paste("Linha ", 1:100)

# Atribuindo os nomes à matriz
rownames(matriz3) <- nomes_linhas

# Criando os nomes das colunas
nomes_colunas <- paste("Coluna ", 1:10)

# Atribuindo os nomes à matriz
colnames(matriz3) <- nomes_colunas

