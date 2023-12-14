# Load all ---------------------------------
# instalar pacotes caso seja necessário
source(
  here::here(
    "03_scripts",
    "00_install_packcages.R"
  )
)


# Get data ---------------------------------
tempodist <- readr::read_rds(
  here::here(
    "02_data_rds",
    "mba_usp_sml",
    "tempodist.rds"
  )
)

tempodist


# Exploratory Data Analysis (EDA) ---------------------------------

# ver resumo dos dados
tempodist |>
  skimr::skim() |>
  tibble::tibble()

tempodist |> summary()


# gráfico de dispersao



# Passo 1: Especificações ---------------------------------
# Especificar:
# a) a f (a hipótese) com seus respectivos hiperparâmetros
# b) o pacote 'motor' (engine)
# c) a tarefa/modo ('regression' ou 'classification')

# para usar tidymodels
especificacao_lm <-
  parsnip::linear_reg() |>
  parsnip::set_engine(engine = "lm") |>
  parsnip::set_mode(mode = "regression")



# Passo 2: Ajuste do modelo ---------------------------------

# usando r base
modelo_lm_r_base <- lm(tempo ~ distancia, data = tempodist)
modelo_lm_r_base


# usando tidymodels
modelo_lm <- especificacao_lm |>
  parsnip::fit(tempo ~ distancia, data = tempodist)
modelo_lm

library(tidymodels)
# observar os parâmetros do modelo
modelo_lm_r_base |> summary() # usando r base
modelo_lm |> parsnip::extract_fit_engine() |> summary() # usando tidymodels
modelo_lm |> broom::tidy() # usando tidymodels (opção 2)

# outras maneiras de observar os parâmetros do modelo (necessita dos dados
# ajustados pelo R base)
modelo_lm_r_base |> jtools::summ()
modelo_lm_r_base |> jtools::export_summs()


# gerar um gráfico dot-and-whisker
# modelo_lm |>
#   yardstick::tidy() |>
#   dotwhisker::dwplot(
#     dot_args = list(size = 2, color = "black"),
#     whisker_args = list(color = "black"),
#     vline = ggplot2::geom_vline(xintercept = 0, colour = "grey50", linetype = 2)
#   )


# Passo 3: Analisar previsões ---------------------------------

# painel de gráficos auxiliares
modelo_lm |> performance::check_model()

# usando r base
tempodist_c_prev_rbase <- tempodist |>
  dplyr::mutate(
    tempo_previsto = modelo_lm_r_base$fitted.values,
    erro = modelo_lm_r_base$residuals
  )

# usando tidymodels
tempodist_c_previsao <- tempodist |>
  dplyr::mutate(
    tempo_previsto = stats::predict(modelo_lm, new_data = tempodist)$.pred,
    residuos = parsnip::extract_fit_engine(modelo_lm) |> stats::residuals()
  )


# gráfico didático para visualizar o conceito de rquadrado
tempodist_c_previsao |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = distancia,
      y = tempo
    )
  ) +
  # plotar os pontos
  ggplot2::geom_point(
    color = "#39568C",
    size = 2.5
  ) +
  # plotar uma reta teórica
  ggplot2::geom_smooth(
    ggplot2::aes(color = "Fitted Values"),
    method = "lm",
    formula = y ~ x,
    se = F,
    size = 2
  ) +
  # adicionar uma linha horizontal no y = 30
  ggplot2::geom_hline(
    yintercept = 30,
    color = "grey50",
    size = 0.5
  ) +
  # adicionar um segmento de reta para representar a diferença entre
  # o tempo previsto e a média do tempo
  ggplot2::geom_segment(
    ggplot2::aes(
      x = distancia,
      xend = distancia,
      y = tempo_previsto,
      yend = mean(tempo),
      color = "Yhat - Ymean"
    ),
    size = 0.7,
    linetype = 2
  ) +
  # adicionar um segmento de reta para representar a diferença entre
  # o tempo e o tempo previsto
  ggplot2::geom_segment(
    ggplot2::aes(
      x = distancia,
      xend = distancia,
      y = tempo,
      yend = tempo_previsto,
      color = "Erro = Y - Yhat"
    ),
    size = 0.7,
    linetype = 3
  ) +
  # renomear eixos
  ggplot2::labs(
    x = "Distância",
    y = "Tempo"
  ) +
  # estabeler título para a legenda e cores padronizadas
  ggplot2::scale_color_manual(
    name = "Legenda",
    values = c("#55C667FF", "grey50", "#440154FF")
  ) +
  # ajustar o tema
  ggplot2::theme_classic()






