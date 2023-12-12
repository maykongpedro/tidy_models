
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
