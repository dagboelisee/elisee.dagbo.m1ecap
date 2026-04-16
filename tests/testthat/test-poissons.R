test_that("filtrer_poissons retourne un data.frame", {
  res <- filtrer_poissons(df_poissons, espece = unique(df_poissons$ESPECE)[1])
  expect_s3_class(res, "data.frame")
})

test_that("filtrer_poissons filtre bien une espece", {
  espece_test <- unique(df_poissons$ESPECE)[1]
  res <- filtrer_poissons(df_poissons, espece = espece_test)
  expect_true(all(res$ESPECE == espece_test))
})

test_that("filtrer_poissons retourne toutes les lignes si aucun filtre", {
  res <- filtrer_poissons(df_poissons)
  expect_equal(nrow(res), nrow(df_poissons))
})

test_that("calcul_individus_par_groupe retourne un data.frame", {
  res <- calcul_individus_par_groupe(df_poissons, groupe = "ESPECE", resume = "sum")
  expect_s3_class(res, "data.frame")
})

test_that("calcul_individus_par_groupe contient la colonne valeur", {
  res <- calcul_individus_par_groupe(df_poissons, groupe = "COMMUNE", resume = "mean")
  expect_true("valeur" %in% names(res))
})

test_that("espece_max_individus retourne une ligne", {
  res <- espece_max_individus(df_poissons)
  expect_equal(nrow(res), 1)
})

test_that("plot_individus_par_groupe retourne un objet ggplot", {
  p <- plot_individus_par_groupe(df_poissons, groupe = "ANNEE", resume = "sum")
  expect_s3_class(p, "ggplot")
})
