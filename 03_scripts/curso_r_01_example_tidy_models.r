
# Load all ---------------------------------
# instalar pacotes caso seja necessário
source(
  here::here(
    "03_scripts",
    "00_install_packcages"
  )
)


# Get data ---------------------------------

# carregar dados
dados_diamantes <- ggplot2::diamonds


# Exploratory Data Analysis (EDA) --------------------------------- 
# essa etapa serve para extrairmos alguns insights da base para que na modelagem já
# tenhamos uma ideia de como algumas variáveis irão se comportar

# ver tipo dos dados
dados_diamantes

# ver resumo dos dados
info_skim <- dados_diamantes |> 
  skimr::skim() |>
  tibble::tibble()

info_skim



# gerar um resumo gráfico de correlações comentário pessoal: ainda tenho
# dificuldade de extrair infos muito úteis desse plot pois é muita informação em
# um só lugar, me atrapalha na análise
dados_diamantes |> 
  # cortar uma amostra aleatória de exemplo da base (2000 linhas)
  dplyr::sample_n(2000) |>
  # gerar um resumo de correlação das variáveis
  GGally::ggpairs()

# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Ajustar a f para um conjunto de dados

# Passo 1: Especificações ---------------------------------
# Especificar:
# a) a f (a hipótese) com seus respectivos hiperparâmetros
# b) o pacote 'motor' (engine)
# c) a tarefa/modo ('regression' ou 'classification')

# exemplo para árvore
especificacao_modelo <- 
  # vai ser uma árvore "tree"
  parsnip::decision_tree(
      # hiperparâmetro chutado, define a complexidade da árvore, nesse caso é muito baixa
    cost_complexity = 0.001
  ) |>
  # definir qual motor que será usado para fazer o ajuste
  parsnip::set_engine(engine = "rpart") |>
  # definir o modo (regressão ou classificação)
  parsnip::set_mode(mode = "regression")


# exemplo para regressão linear
especificacao_modelo_linear <- 
  parsnip::linear_reg() |>
  parsnip::set_engine(engine = "lm") |>
  parsnip::set_mode(mode = "regression")


# exemplo para random forest
especificacao_modelo_random_forest <- 
  parsnip::rand_forest() |>
  parsnip::set_engine(engine = "ranger") |>
  parsnip::set_mode(mode = "regression")


# Passo 2: Ajuste do modelo ---------------------------------
  
modelo <- especificacao_modelo |>
  # y (price) em função da variável x
  parsnip::fit(price ~ x, data = dados_diamantes)

# ver modelo
modelo

# ver a árvore de regressão
rpart.plot::prp(modelo$fit)



# Passo 3: Analisar previsões ---------------------------------

# obter e acessar previsão dos dados
# previsao <- predict(modelo, new_data = dados_diamantes)
# previsao$.pred


diamantes_com_previsao <- dados_diamantes |>
  dplyr::mutate(
    # criar coluna com a previsao dos dados geralmente aqui o argumento new_data
    # recebe a base de teste, e não a base de treino, para realmente validar a
    # previsão do modelo
    price_previsao = predict(modelo, new_data = dados_diamantes)$.pred
  ) |>
  print()


# plotar pontos observados + curva f
diamantes_com_previsao |>
  dplyr::filter(x > 0) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y   = price),
    alpha = 0.3
  ) +
  ggplot2::geom_step(
    mapping = ggplot2::aes(x = x, y = price_previsao),
    color = "red", 
    linewidth = 1
   ) +
  ggplot2::theme_bw()
  

# plotar pontos observados x esperados
diamantes_com_previsao |>
  dplyr::filter(x > 0) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = price_previsao, y = price)  
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    color = "purple"
  ) +
  ggplot2::theme_bw()


# Passo 4 - Como quantificar a qualidade de um modelo? ---------------------------------

# Métricas de erro
# erro quadrático médio: cerca de 1500$ por diamante
diamantes_com_previsao |> 
  yardstick::rmse(
    truth = price, 
    estimate = price_previsao
  )

# Resíduo = truth-estimate
# erro absoluto médio: cerca de 850$ por diamante
diamantes_com_previsao |> 
  yardstick::mae(
    truth = price, 
    estimate = price_previsao
  )

# R quadrado: cerca de 86% da variabilidade está sendo explicada com o modelo
diamantes_com_previsao |> 
  yardstick::rsq(
    truth = price, 
    estimate = price_previsao
  )

# esse modelo tem um problema com heterasticidade pois os preços grandes já
# possuam uma distribuiçao diferente da normal, a mairo parte dos dados está nos
# preços pequenos, o que acaba gerando uma boa estatística de erro mas na
# prática ainda erramos muito quando os preços são grandes