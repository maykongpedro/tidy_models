
# Carregar dados brutos da aula 1 de Supervised Machine Learning do MBA USP
# Base de dados: tempodist


# Get raw data ---------------------------------
# carregar dados
raw_tempodist <- readxl::read_excel(
  path = here::here(
    "01_raw_data",
    "mba_usp_sml",
    "tempodist.xls"
  )
) |> janitor::clean_names()

raw_tempodist


# Transform data ---------------------------------

tempodist <- raw_tempodist |>
  dplyr::select(tempo_y, distancia_x) |>
  dplyr::rename(
    tempo = tempo_y,
    distancia = distancia_x
  ) |>
  tidyr::drop_na()


tempodist


# Export data ---------------------------------
tempodist |>
  readr::write_rds(
    file = here::here(
      "02_data_rds",
      "mba_usp_sml",
      "tempodist.rds"
    )
  )
