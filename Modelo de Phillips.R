library(sidrar)
library(tidyverse)
#Coleta dos dados
ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")
#Limpeza
dplyr::glimpse(ipca_raw)
ipca <- ipca_raw |>
      dplyr::select("data"= "Mês (Código)",
                    "ipca"= "Valor") |>
      dplyr::mutate(data = lubridate::ym(data)) |>
      dplyr::filter(data >= "2004-01-01")
      dplyr::as_tibble()
#Analise Exploratória
ggplot2::ggplot(ipca)+ggplot2::aes(x=data,y=ipca)+ggplot2::geom_line()

summary(ipca)
      
ggplot2::ggplot(ipca)+ggplot2::aes(y=ipca)+ggplot2::geom_boxplot()
ggplot2::ggplot(ipca)+ggplot2::aes(x=ipca)+ggplot2::geom_histogram()

#Taxa de Desocupação
/t/6381/n1/all/v/4099/p/all/d/v4099%201 #API do Sidra IBGE
desocupacao_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

desocupacao <- desocupacao_raw |> 
            dplyr::select('data' = "Trimestre Móvel (Código)", 'desocupacao' = "Valor") |>
            dplyr::mutate(data = lubridate::ym(data)) |>
            dplyr::as_tibble()
df_dados <- ipca |> inner_join(desocupacao, by = "data")

df_dados |> 
  ggplot2::ggplot()+
  ggplot2::aes(x=data)+
  ggplot2::geom_line(aes(y=desocupacao,color = "Taxa de Desocupação"))+
  ggplot2::geom_line(aes(y=ipca,color = "Inflação"))+
  ggplot2::scale_color_manual(values = c("#800000", "#0000FF"))

modelo_phillips <- lm(ipca ~ desocupacao, data = df_dados)
summary(modelo_phillips)
