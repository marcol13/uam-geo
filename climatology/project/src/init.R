load_packages <- function(env = "dev") {
  print(sprintf("Loading packages for %s environment", env))

  if (env == "notebook") {
    packages <- c("rmarkdown", "Rcpp", "pandoc", "knitr", "dplyr", "config", "climate", "zoo", "tidyr", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata")
  } else if (env == "dev") {
    packages <- c("styler")
  } else {
    stop("Invalid environment specified. Use 'notebook' or 'dev'.")
  }

  package_check <- match(packages, utils::installed.packages()[, 1])

  packages_to_install <- packages[is.na(package_check)]

  if (length(packages_to_install) > 0L) {
    utils::install.packages(packages_to_install,
      repos = "https://cran.uni-muenster.de"
    )
  } else {
    print(sprintf("All requested packages already installed for %s environment", env))
  }

  for (package in packages) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE, quietly = TRUE)
    )
  }
}

validate_config <- function(config) {
  if (is.null(config)) {
    stop("Config is NULL. Please check the config file.")
  }

  if (!is.character(config$data_path) || nchar(config$data_path) == 0) {
    stop("data_path is not set in the config file.")
  }

  if (!is.character(config$pl_meteo_filename) || nchar(config$pl_meteo_filename) == 0) {
    stop("pl_meteo_filename is not set in the config file.")
  }

  if (!is.character(config$pzn_meteo_filename) || nchar(config$pzn_meteo_filename) == 0) {
    stop("pzn_meteo_filename is not set in the config file.")
  }

  print(sprintf("Config file loaded successfully."))
}
