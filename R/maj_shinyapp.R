# 1. Installer rsconnect si nécessaire
if (!require("rsconnect")) install.packages("rsconnect")

# 2. Se connecter (une seule fois)
rsconnect::setAccountInfo(
  name = "moncompte",
  token = "ABC123...",  # À remplacer
  secret = "XYZ456..."  # À remplacer
)


# Dans RStudio, dans le dossier du projet :
# renv::restore()  # Installe les dépendances du lockfile
# renv::update("Rcpp")  # Met à jour Rcpp
# renv::snapshot()  # Met à jour le lockfile


# 3. Déployer la mise à jour
rsconnect::deployApp(
  appDir = "D:/Github/PhytoCohortis/R",
  appName = "PhytoCohortis"
)
