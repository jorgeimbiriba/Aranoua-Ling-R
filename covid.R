require(ggplot2)

dados_brutos = readxl::read_xlsx(
  "Jorge - Ling R/Aranoua-Ling-R/dados/Dados_Livro_Estatistica_e_CD/covid2.xlsx",
  sheet = "dados",
  col_types = c("date", "numeric", "numeric")
)

plot.ts(dados_brutos$obitos |> cumsum())

?cumsum()

dados = dados_brutos |>
  dplyr::mutate(
    ano = lubridate::year(dia),
    mes = lubridate::mouth(dia, label = T),
    dia_semana = lubridate::wday(dia, label = T),
    semana = lubridate::week(dia),
    letalidade = (obitos/casos*100))|>
  dplyr::relocate(dia,ano,mes.,diasemana,obitos,casos)
