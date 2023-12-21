
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


set.seed(8)

diamondiszinho <- dados_diamantes |>
  # filtra tudo onde x é maior que 0
  dplyr::filter(x > 0) |>
  # agrupa pela variável 'x'
  dplyr::group_by(x) |>
  # pega um representante de cada grupo
  dplyr::sample_n(1) |>
  # desagrupa
  dplyr::ungroup()


# Passo 1: Especificações ---------------------------------
# Especificar:
# a) a f (a hipótese) com seus respectivos hiperparâmetros
# b) o pacote 'motor' (engine)
# c) a tarefa/modo ('regression' ou 'classification')

# modelo normal
especificacao_mod1 <- 
  parsnip::decision_tree(
    # custo para realizar os cortes (min_n)
    # quanto maior o custo, menos cortes a árvore vai ter
    # quanto menor, mais fácil para a árvore fazer overfitting
    cost_complexity = 0.004,
    # a quantidade de pontos que a árvore vai ter que questionar comparando
    # o erro entre os pontos
    # quanto mais baixo, mais fácil fazer overfitting
    min_n = 5,
    # altura: o máximo de perguntas que a árvore pode fazer
    tree_depth = 10
  ) |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")


# overfitting
especificacao_mod2_of <- 
  parsnip::decision_tree(
    # a árvore pode crescer o quanto quiser, sem custo
    cost_complexity = 0,
    # só compara o erro com outros 2 pontos
    min_n = 2,
    # pode fazer 20 perguntas para cada ponto
    tree_depth = 20
  ) |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")


  

# Passo 2: Ajuste do modelo ---------------------------------
  
ajuste_modelo1 <- especificacao_mod1 |>
  parsnip::fit(price ~ x, data = diamondiszinho)

ajuste_modelo2_of <- especificacao_mod2_of |>
  parsnip::fit(price ~ x, data = diamondiszinho)


# Passo 3: Analisar previsões ---------------------------------

diamondiszinho_c_previsoes <- diamondiszinho |>
  dplyr::mutate(
    price_pred_mod1 = stats::predict(ajuste_modelo1, new_data = diamondiszinho)$.pred,
    price_pred_mod2 = stats::predict(ajuste_modelo2_of, new_data = diamondiszinho)$.pred,
  )


# Passo 4: Qualidade dos ajustes ---------------------------------

# pivotar dados das previsões dos modelos para uma única coluna
diamondiszinho_c_previsoes_long <- diamondiszinho_c_previsoes |>
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  )

diamondiszinho_c_previsoes_long


# estimativa de erro por modelo
diamondiszinho_c_previsoes_long |>
  dplyr::group_by(modelo) |>
  yardstick::rmse(
    truth = price,
    estimate = price_pred
  )

# r quadrado por modelo (de 0 a 1)
diamondiszinho_c_previsoes_long |>
  dplyr::group_by(modelo) |>
  yardstick::rsq(
    truth = price,
    estimate = price_pred
  )
