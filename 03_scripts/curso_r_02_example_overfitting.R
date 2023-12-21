
# Load all ---------------------------------
# instalar pacotes caso seja necessário
source(
  here::here(
    "03_scripts",
    "00_install_packcages.R"
  )
)
library(patchwork)


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


# plotar gráfico dos pontos observados + curva f
p1_diamondiszinho_c_previsoes <- diamondiszinho_c_previsoes |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = x, 
      y = price
    ),
    size = 3
  ) +
  ggplot2::geom_step(
    mapping = ggplot2::aes(
      x = x,
      y = price_pred_mod1,
      color = "Modelo 1"
    ),
    linewidth = 1
  ) +
  ggplot2::geom_step(
    mapping = ggplot2::aes(
      x = x,
      y = price_pred_mod2,
      color = "Modelo 2 - Overffiting"
    ),
    linewidth = 1
  )
p1_diamondiszinho_c_previsoes


# plotar gráfico de observado x esperado
diamondiszinho_c_previsoes |>
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = price_pred, 
      y = price,
      color = modelo
    ),
    size = 3
  ) +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    color = "purple"
  ) +
  ggplot2::theme_bw()


# plotar gráfico de resíduos x esperado por esse modelo não pressupostos, o
# formato dos resíduos não implica em nada a única garantia teórica que temos é
# que o erro que conseguirmos em uma base de teste, geralmente vai ser próximo
# do erro que conseguirmos com uma base em produção
p2_diamondiszinho_c_previsoes <- diamondiszinho_c_previsoes |>
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = price_pred, 
      y = price - price_pred,
      color = modelo
    ),
    size = 3
  ) +
  ggplot2::geom_abline(
    slope = 0,
    intercept = 0,
    color = "purple"
  ) +
  ggplot2::ylim(c(-10000, 10000)) +
  ggplot2::labs(
    y = "Resíduos (y - y_chapeu)"
  )
  ggplot2::theme_bw()

p2_diamondiszinho_c_previsoes





# ======================================================================
# Extra: Simular uma etapa de produção ---------------------------------
# ======================================================================


set.seed(3)
# dados novos chegaram...
diamantes_novos <- dados_diamantes |>
  dplyr::filter(x > 0) |>
  dplyr::sample_n(100)


# predições 
diamantes_novos_previsoes <- diamantes_novos |>
  dplyr::mutate(
    price_pred_mod1 = stats::predict(ajuste_modelo1, new_data = diamantes_novos)$.pred,
    price_pred_mod2 = stats::predict(ajuste_modelo2_of, new_data = diamantes_novos)$.pred,
  )

# pivotar para gerar as métricas de erro
diamantes_novos_previsoes_long <- diamantes_novos_previsoes |>
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  )

# estimativa de erro por modelo
diamantes_novos_previsoes_long |>
  dplyr::group_by(modelo) |>
  yardstick::rmse(
    truth = price,
    estimate = price_pred
  )

# r quadrado por modelo (de 0 a 1)
diamantes_novos_previsoes_long |>
  dplyr::group_by(modelo) |>
  yardstick::rsq(
    truth = price,
    estimate = price_pred
  )

# plotar gráfico de pontos observados + curva f
p1_diamantes_novos_previsoes <- diamantes_novos_previsoes |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = x, 
      y = price
    ),
    size = 3
  ) +
  ggplot2::geom_step(
    mapping = ggplot2::aes(
      x = x,
      y = price_pred_mod1,
      color = "Modelo 1"
    ),
    linewidth = 1
  ) +
  ggplot2::geom_step(
    mapping = ggplot2::aes(
      x = x,
      y = price_pred_mod2,
      color = "Modelo 2 - Overffiting"
    ),
    linewidth = 1
  )

# plotar o gráfico da base original com os dados novos
p1_diamondiszinho_c_previsoes / p1_diamantes_novos_previsoes


# plotar gráfico de resíduos x esperado
p2_diamantes_novos_previsoes <- diamantes_novos_previsoes |>
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = price_pred, 
      y = price - price_pred,
      color = modelo
    ),
    size = 3
  ) +
  ggplot2::geom_abline(
    slope = 0,
    intercept = 0,
    color = "purple"
  ) +
  ggplot2::ylim(c(-10000, 10000)) +
  ggplot2::labs(
    y = "Resíduos (y - y_chapeu)"
  )
  ggplot2::theme_bw()


# plotar o gráfico da base original com os dados novos
p2_diamondiszinho_c_previsoes / p2_diamantes_novos_previsoes


# podemos observar que o modelo com overffiting dessa vez não se comportou bem.
# Uma vez que ele basicamente se adaptou totalmente aos dados de treino, nào
# conseguiu se encontrar na base de teste (produção)