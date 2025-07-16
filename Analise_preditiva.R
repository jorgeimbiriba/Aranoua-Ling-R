
#' Apagar todos objetos criados
rm(list=ls()) 


# Carregar Pacotes --------------------------------------------------------
require(dplyr)
require(caret)

# Carregar base de dados --------------------------------------------------


load("dados/humidity_temperature_manaus.rda") # carregar base de dados no formato rda
dados_brutos = base_manaus
glimpse(dados_brutos) # mesmo que str(dados)
head(dados_brutos)
unique(dados_brutos$year) |> length()

dados_brutos

controle_ts <- trainControl(
  method = "timeslice",
  initialWindow = 5*12,  # usa os 60 primeiros meses como treino inicial
  horizon = 12,          # prevê 12 meses à frente
  fixedWindow = TRUE,
  savePredictions = "final",
  verboseIter = TRUE
)


# Em função do tempo ------------------------------------------------------
plot.ts(dados_brutos)

dados_novos_manaus = dados_brutos|> mutate(tempo=1:144, seno=sin(2*pi*tempo/12),cos=cos(2*pi*tempo/12))

set.seed(123) # Para reprodutibilidade dos resultados

modelo_ts <- caret::train(
  #initialWindow = 5*12,  # usa os 60 primeiros meses como treino inicial
  #horizon = 12,          # prevê 12 meses à frente
  dbt ~ tempo + rh,
  data = dados_novos_manaus,
  method = "lm", # Usando um modelo de regressão linear (Linear Model)
  trControl = controle_ts # O controle de validação cruzada de série temporal que você já definiu
)

# Visualizar o resumo do modelo
print(modelo_ts)
summary(modelo_ts) # Detalhes do modelo final ajustado


# 1. Gerar os valores ajustados (previsões do modelo para os próprios dados de treinamento)
dados_novos_manaus$temperatura_ajustada <- predict(modelo_ts, newdata = dados_novos_manaus)

# 2. Criar o gráfico de série temporal com dados observados e ajustados
ggplot(dados_novos_manaus, aes(x = tempo)) +
  geom_line(aes(y = dbt, color = "Observado"), size = 1) + # Dados reais
  geom_line(aes(y = temperatura_ajustada, color = "Ajustado pelo Modelo"), linetype = "dashed", size = 1) + # Dados ajustados
  labs(
    title = "Temperatura Observada vs. Ajustada pelo Modelo",
    x = "Mês",
    y = "Temperatura",
    color = "Tipo de Dado" # Legenda da cor
  ) +
  scale_color_manual(values = c("Observado" = "darkblue", "Ajustado pelo Modelo" = "red")) + # Cores personalizadas
  theme_minimal() + # Tema minimalista para o gráfico
  theme(legend.position = "bottom") # Posição da legenda

max(dados_novos_manaus$temperatura_ajustada)
