
# Filtre le jeu de donnees de poissons selon une ou plusieurs especes
# et/ou une ou plusieurs communes.

filtrer_poissons <- function(data, espece = NULL, commune = NULL) {
  if (!is.data.frame(data)) {
    stop("data doit etre un data.frame.")
  }

  res <- data

  if (!is.null(espece)) {
    res <- dplyr::filter(res, ESPECE %in% espece)
  }

  if (!is.null(commune)) {
    res <- dplyr::filter(res, COMMUNE %in% commune)
  }

  res
}


# Calculer le nombre total moyen ou maximal d'individus par groupe


calcul_individus_par_groupe <- function(data, groupe = "ESPECE", resume = "sum") {
  if (!is.data.frame(data)) {
    stop("data doit etre un data.frame.")
  }

  groupes_valides <- c("ESPECE", "COMMUNE", "ANNEE")
  resumes_valides <- c("sum", "mean", "max", "median")

  if (!groupe %in% groupes_valides) {
    stop("groupe doit etre l'une de ces valeurs : ESPECE, COMMUNE, ANNEE.")
  }

  if (!resume %in% resumes_valides) {
    stop("resume doit etre l'une de ces valeurs : sum, mean, max, median.")
  }

  data_grouped <- dplyr::group_by(data, .data[[groupe]])

  result <- switch(
    resume,
    sum = dplyr::summarise(data_grouped, valeur = sum(NB_INDIVIDUS, na.rm = TRUE), .groups = "drop"),
    mean = dplyr::summarise(data_grouped, valeur = mean(NB_INDIVIDUS, na.rm = TRUE), .groups = "drop"),
    max = dplyr::summarise(data_grouped, valeur = max(NB_INDIVIDUS, na.rm = TRUE), .groups = "drop"),
    median = dplyr::summarise(data_grouped, valeur = stats::median(NB_INDIVIDUS, na.rm = TRUE), .groups = "drop")
  )

  result
}

# Représenter graphiquement le nombre d'individus par groupe


plot_individus_par_groupe <- function(data, groupe = "ESPECE", resume = "sum") {
  df_plot <- calcul_individus_par_groupe(data = data, groupe = groupe, resume = resume)

  ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[groupe]], y = valeur)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = paste("Nombre d'individus par", tolower(groupe)),
      x = groupe,
      y = resume
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# espece avec le plus grand nombre d'individus


espece_max_individus <- function(data) {
  if (!is.data.frame(data)) {
    stop("data doit etre un data.frame.")
  }

  data |>
    dplyr::group_by(ESPECE) |>
    dplyr::summarise(total = sum(NB_INDIVIDUS, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::slice(1)
}




