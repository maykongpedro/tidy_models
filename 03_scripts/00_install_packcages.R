
libs <- c(
  "tidyverse", # data manipulation
  "here", # folders and path manipulation,
  "tidymodels", # modeling with tidy universe
  "skimr", # alternative to base::summary()
  "GGally", # alternativa graphic
  "dotwhisker", #generate a dot-and-whisker plot
  "patchwork" # plots arrange
  # "equatiomatic" # extract model equation
)

# checar se todos os pacotes necessários já estão instalados
installed_libs <- libs %in% rownames(
  installed.packages()
)

# instalar pacotes que não estão instalados
if(any(installed_libs == FALSE)){
  install.packages(
    libs[!installed_libs]
  )
}

# carregar pacotes (etapa opcional)
# invisible(lapply(
#   libs, library,
#   character.only = TRUE
# ))

# remover tudo que consta no ambiente de trabalho
rm(list = ls())
