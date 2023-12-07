
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
  