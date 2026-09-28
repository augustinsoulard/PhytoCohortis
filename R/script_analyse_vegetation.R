# Introduction à R

# Installation de R et RStudio ########################################
# Pour commencer, assurez-vous d'avoir installé R et RStudio.
# Vous pouvez télécharger R depuis https://cran.r-project.org/
# et RStudio depuis https://posit.co/download/rstudio-desktop/.

# Installation de R et RStudio
# 1. Allez sur les sites mentionnés ci-dessus.
# 2. Téléchargez et installez R et RStudio.
# 3. Ouvrez RStudio et créez un nouveau script R.

# Manipulation de données avec R

# Chargement des packages ########################################

# Manipulation et transformation des données
library(dplyr)   # Opérations sur les data frames (filtrer, sélectionner, agrégat)
library(tidyr)   # Nettoyage et réorganisation des données (pivot, tidy data)
library(tibble)  # Création et manipulation de tibbles (data frames modernes)

# Import/export des données
library(readr)    # Lecture de fichiers (CSV, TSV, etc.)
library(openxlsx) # Écriture et lecture de fichiers Excel (.xlsx)
library(readODS) # Écriture et lecture de fichiers Libre Office calc (.ods)

# Visualisation
library(ggplot2)  # Création de graphiques statistiques
library(ggrepel)  # Évite le chevauchement des labels dans ggplot2

# Analyses écologiques
library(vegan)         # Analyses multivariées pour les communautés végétales (NMDS, PCA, etc.)
library(indicspecies) # Identification des espèces indicatrices
library(NbClust) # Package d'aide au choix du nombre de cluster
library(factoextra) # pour faire un cluster coloré

# Choix du répertoire de travail
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importation de données ########################################
# Utilisons un exemple de jeu de données de relevés phytosociologiques
# ?nous avons un fichier CSV nommé 'data_releve_type.csv'.
# Voici comment importer ces données :

# Exemple de code pour importer des données
data_releve = read_csv2("../data/data_releve_type.csv")


data_releve = data_releve %>% mutate(abondance_dominance = case_when(
  abondance_dominance == "+" ~ 0.5,
  TRUE ~ as.numeric(as.character(abondance_dominance))
))

# Filtrer les abondances dominances NAs
data_releve = data_releve %>% filter(!is.na(abondance_dominance))

# Visualiser des relevés ####
# Exemple ne choisir que certains relevés
data_releve_filtre = data_releve %>% filter(releve %in% c("R14","R5","R22"))
# Visualisation de données

# Création de graphiques heatmap pour visualiser les similitudes des relevés

ggplot(data_releve_filtre, aes(x = espece, y = releve, fill = abondance_dominance))+
  geom_tile() +
  scale_fill_gradient(low = "#f8c856", high = "#228822") +  # Ajustez la palette de couleurs selon vos préférences
  labs(x = "Espèce", y = "Relevé", fill = "Abondance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Filtrer les données utiles
releves_filtre = c("R21","R8","R24","R23","R28")
data_releve = data_releve %>% filter(!releve %in% releves_filtre)

# Créer une colonne combinée pour strate et espèce
data_releve_combined <- data_releve %>%
  mutate(combination = paste(espece, strate, sep = "_")) %>%
  group_by(releve, combination) %>%
  summarise(abondance_dominance = mean(abondance_dominance, na.rm = TRUE), .groups = 'drop')


# Préparer les données pour la table matricielle avec les données agrégées
data_releve_matrix <- data_releve_combined %>%
  pivot_wider(names_from = combination, values_from = abondance_dominance, values_fill = 0) %>%
  column_to_rownames(var = "releve") %>%  as.matrix()  # Assure une conversion en matrice# Remplacer les valeurs manquantes par 0, au cas où

# data_releve_matrix[is.na(data_releve_matrix)] = 0

# Calculer la matrice des distances de Bray-Curtis
distance_matrix = vegdist(data_releve_matrix, method = "bray")

# Effectuer la Classification Ascendante Hiérarchique  ########################################
cah_result <- hclust(distance_matrix, method = "ward.D2")
# Couper le dendrogramme pour obtenir des groupes (par exemple, 3 groupes)
  # Liste des indices à tester
  indices <- c("frey", "mcclain", "cindex", "silhouette", "dunn")
  
  # Calculer Best.nc pour chaque index et stocker dans une liste
  best_nc_list <- lapply(indices, function(idx) {
    NbClust(diss = distance_matrix, distance = NULL, method = "ward.D2", index = idx, min.nc = 2, max.nc = 30)$Best.nc #Retirer $Best.nc pour avoir les détails
  })
  
  # Nommage automatique de la liste
  names(best_nc_list) <- indices
  
  # Afficher/retourner la liste finale
  best_nc_list


  num_groups <- 8 # CHOIX DU NOMBRE DE GROUPE
  
  plot(cah_result$height,type ="s")
  abline(v = num_groups,h = cah_result$height[num_groups],col = "red", lty = 2)

  
  
  groups <- cutree(cah_result, k = num_groups)
  
  # Assigner des couleurs de base aux groupes (Groupe 1 = couleur 1, etc.)
  my_colors <- rainbow(num_groups)
  
  # Récupérer l'ordre des clusters tel qu'affiché de gauche à droite
  leaf_order <- order.dendrogram(as.dendrogram(cah_result))
  groups_in_dendro_order <- groups[leaf_order]
  cluster_order_in_dendro <- unique(groups_in_dendro_order)

  
  # Renumérotation : 1 = 1er groupe à gauche, 2 = 2ème, etc.
  relabel_map <- setNames(seq_along(cluster_order_in_dendro), as.character(cluster_order_in_dendro))
  new_groups <- relabel_map[as.character(groups)]  # Remplace les IDs bruts par 1..k
  names(new_groups) <- names(groups)  # **Conserve les noms des relevés**
  groups <- factor(new_groups, levels = 1:num_groups)  # Factor avec niveaux 1..k
  
  # Couleurs alignées sur l'ordre du dendrogramme
  dendro_colors <- rainbow(num_groups)[as.numeric(cluster_order_in_dendro)]
  
  # Tracer le dendrogramme
  fviz_dend(
    cah_result,
    k = num_groups,
    k_colors = "black", # couleur des branches
    color_labels_by_k = FALSE, # Pas de couleurs pour les étiquettes de relevé
    rect = TRUE,
    rect_fill = TRUE,
    rect_border = dendro_colors, # couleur des groupes
    rect_lty = 1,
    ggtheme = theme(
      plot.background = element_rect(fill = "#f4f8f9", color = NA),  # Fond gris clair
      panel.background = element_rect(fill = "#f4f8f9", color = "darkgreen"),
      text = element_text(family = "sans", color = "darkgreen"),
      axis.text.y = element_text(size = 10, color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid = element_blank()  # Supprime les grilles
    ),
    main = "Classification Ascendante Hiérarchique"
  )
  
  
  
# Exécuter l'analyse NMDS ########################################
set.seed(8) # pour la reproductibilité
nmds_result <- metaMDS(data_releve_matrix, k = 2, trymax = 100, autotransform = FALSE)
stress_val <- round(nmds_result$stress, 3)

# Récupérer les scores NMDS
nmds_sites <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_sites$label <- rownames(nmds_sites)

# Assigner les groupes aux relevés NMDS
nmds_sites$Groupe <- factor(groups[rownames(nmds_sites)], levels = 1:num_groups)

# Visualisation avec ggplot2
ggplot(nmds_sites, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Groupe), size = 3) +
  scale_color_manual(values = dendro_colors) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, max.overlaps = 100) +
  labs(title = "Ordination NMDS",
       subtitle = paste("Stress:", stress_val),
       x = "NMDS 1", y = "NMDS 2") +
  theme_minimal() +
  coord_equal()

# Analyses des données environnementales ########################################


# Charger les données environnementales
env_data <- read_csv2("envdata_releve.csv") # Remplacez par le chemin correct
env_data = env_data[order(env_data$Nom), ]
base::rownames(env_data) <- env_data$Nom # Assurez-vous que les lignes sont nommées par les relevés

#Retirer les relevés à retirer : 
env_data = env_data %>% filter(!Nom %in% releves_filtre)%>%
  {rownames(.) <- .$Nom; .}

# Ordonner les tableaux de la même manière
data_releve_matrix = data_releve_matrix[order(rownames(data_releve_matrix)), ]

# Forcer la correspondance des lignes avant la CCA
# env_data <- env_data[rownames(data_releve_matrix), ]

stopifnot(all(rownames(data_releve_matrix) == rownames(env_data))) # TRUE c'est que les données sont prêtes pour la CCA. AUtrement réordonner les tables


#Filtrer les variables utilisées
env_data = env_data %>% select(Altitude, Pente,Recouvrement_herbacee,Recouvrement_arbustive, Recouvrement_arboree,
                               Hauteur_herbacee,Hauteur_arbustive,Hauteur_arboree)# Supprimer la colonne 'releve' si elle est incluse dans les données environnementales

#Centrer et réduire les variables
env_data = as.data.frame(apply(env_data,2,function(x){
  if(is.numeric(x)){
    (x -mean(x,na.rm = TRUE))/sd(x,na.rm=TRUE)
  } else{x}
}
  
  ))




# Exécuter la CCA
cca_result <- cca(data_releve_matrix ~ ., data = env_data)
####IMPORTANT##### les relevés des tableaux data_releve_matrix et env_data doivent être les mêmes et dans le même ordre de ligne

# Extraire les scores des sites et des espèces
site_scores <- vegan::scores(cca_result,choices=c(1,2,3), display = "sites")
species_scores <- vegan::scores(cca_result,choices=c(1,2,3), display = "species")
biplot_scores <- vegan::scores(cca_result,choices=c(1,2,3), display = "bp") # Variables environnementales

# Créer un dataframe pour les sites
df_sites <- data.frame(
  Site = rownames(site_scores),
  CCA1 = site_scores[, 1],
  CCA2 = site_scores[, 2]
)

# Créer un dataframe pour les espèces
df_species <- data.frame(
  Species = rownames(species_scores),
  CCA1 = species_scores[, 1],
  CCA2 = species_scores[, 2]
)

# Créer un dataframe pour les variables environnementales (biplot)
df_env <- data.frame(
  Variable = rownames(biplot_scores),
  CCA1 = biplot_scores[, 1],
  CCA2 = biplot_scores[, 2]
)

### récupérer la contribution des axes :
# Stocker le résumé de l'analyse
sommaire_cca <- summary(cca_result)

# Récupérer le tableau de la contribution des axes contraints
contribution_axes <- sommaire_cca$concont$importance

barplot(contribution_axes[2,])

# Extraire la proportion de variance expliquée pour les axes 1 et 2
# et la convertir en pourcentage joliment formaté
cca1_percent <- round(sommaire_cca$concont$importance[2, 1] * 100, 1)
cca2_percent <- round(sommaire_cca$concont$importance[2, 2] * 100, 1)

# Créer les nouvelles étiquettes pour les axes
axe_x_label <- paste0("CCA1 (", cca1_percent, "%)")
axe_y_label <- paste0("CCA2 (", cca2_percent, "%)")

# Visualisation avec ggplot2 pour les relevés et variables environnementales
ggplot() +
  # --- Relevés ---
  geom_point(data = df_sites, aes(x = CCA1, y = CCA2),
             color = "black", size = 3) +
  geom_text(data = df_sites, aes(x = CCA1, y = CCA2, label = Site),
            vjust = -0.5, size = 3, color = "black") +
  
  # --- Variables environnementales ---
  geom_segment(data = df_env, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = df_env, aes(x = CCA1, y = CCA2, label = Variable),
                  size = 3, color = "red", segment.color = "grey50") +
  
  labs(title = "CCA Biplot", x = axe_x_label, y = axe_y_label) +
  theme_minimal()

#Visualisation des espèces et des variables environnementales
ggplot() +
  # --- Relevés ---
  geom_point(data = df_species, aes(x = CCA1, y = CCA2),
             color = "black", size = 1) +
  geom_text(data = df_species, aes(x = CCA1, y = CCA2, label = Species),
            vjust = -0.5, size = 2, color = "black") +
  
  # --- Variables environnementales ---
  geom_segment(data = df_env, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = df_env, aes(x = CCA1, y = CCA2, label = Variable),
                  size = 3, color = "red", segment.color = "grey50") +
  
  labs(title = "CCA Biplot", x = axe_x_label, y = axe_y_label) +
  theme_minimal()


# Récupérer les scores des espèces (display = "species" est valide)
species_scores_cca <- vegan::scores(cca_result, display = "species", choices = c(1, 2))

# Calculer la contribution (somme des carrés des scores)
species_contrib <- rowSums(species_scores_cca^2)

# Trier et sélectionner les top N
top_n <- 30 # CHOIX DU NOMBRE D'ESPECE A AFFICHER
top_species <- names(sort(species_contrib, decreasing = TRUE)[1:top_n])

# Filtrer df_species
df_species_filtered <- df_species[df_species$Species %in% top_species, ]

# Visualisation des top espèces et des variables environnementales
ggplot() +
  # --- Espèces (avec répétition des étiquettes) ---
  geom_point(data = df_species_filtered, aes(x = CCA1, y = CCA2),
             color = "black", size = 1) +
  geom_text_repel(
    data = df_species_filtered,
    aes(x = CCA1, y = CCA2, label = Species),
    size = 3,                     # Légèrement plus grand
    color = "black",
    max.overlaps = 100,            # Autorise jusqu'à 100 chevauchements initiaux
    box.padding = 0.5,             # Espace autour des étiquettes
    segment.color = "grey50",      # Couleur des segments
    segment.size = 0.2,            # Épaisseur des segments
    direction = "both",            # Déplace dans toutes les directions
    angle = 0                      # Garde le texte horizontal
  ) +
  
  # --- Variables environnementales (inchangé) ---
  geom_segment(data = df_env, aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(data = df_env, aes(x = CCA1, y = CCA2, label = Variable),
                  size = 3, color = "red", segment.color = "grey50") +
  
  labs(title = "CCA Biplot (Top espèces)", x = axe_x_label, y = axe_y_label) +
  theme_minimal()


# Test de permutation
# On teste l'hypothèse nulle, il n'y a pas de lien entre mes variables environnementales et mes espèces
# Si p-value <0.05, rejet de l'hypothèse nulle, donc il y a bien un lien entre mes varibles environnementales et les espèces

permutest(cca_result, permutations = 999) 


# Summary pour voir la variance expliquée
summary(cca_result)


# Indice Value ########################################

# Utilisez votre matrice de communauté (relevés x espèces), PAS la matrice de distance.
indval_res <- multipatt(data_releve_matrix, groups, 
                        func = "IndVal.g", 
                        control = how(nperm = 999))

# Extraire le tableau des espèces significatives (p-value <= 0.10)
indval_df = as.data.frame(indval_res$sign) %>% filter(p.value<=0.1)


# Ajouter les noms d'espèces comme une colonne (ils sont dans les noms de lignes)
indval_df$Espèce <- rownames(indval_df)


# Afficher le tableau final
print(indval_df)


### Appartenance des relevés aux groupes
# 1. Créer un data frame à partir de votre objet 'groups'
# L'objet 'groups' (créé avec cutree) contient déjà les noms des relevés et leur groupe.
df_groupes <- data.frame(Releve = names(groups), 
                         Groupe = groups)

# 2. Utiliser votre code dplyr (qui est parfait) pour résumer l'information
df_summary <- df_groupes %>%
  group_by(Groupe) %>%
  summarise(Releves_inclus = paste(Releve, collapse = ", "), .groups = 'drop')

# 3. Afficher le tableau final
print("Liste des relevés pour chaque groupe de la CAH :")
print(df_summary)

# Sauvegarder les résultats dans un même excel  ########################################
# 1. Créer un classeur Excel vide
wb <- createWorkbook()

# 2. Ajouter la première feuille et y écrire le premier tableau
addWorksheet(wb, "Especes_Indicatrices")
writeData(wb, "Especes_Indicatrices", indval_df)

# 3. Ajouter la deuxième feuille et y écrire le deuxième tableau
addWorksheet(wb, "Releves_Par_Groupe")
writeData(wb, "Releves_Par_Groupe", df_summary)

# 4. Enregistrer le fichier Excel sur votre ordinateur
# Le fichier s'appellera "Resultats_Analyse_Groupes.xlsx"
saveWorkbook(wb, file = "Resultats_Analyse_Groupes.xlsx", overwrite = TRUE)

# Exporter CSV
write.csv2(indval_df,"indval_df.csv")
write.csv2(df_summary,"df_summary.csv")


# ___________________________________________
# TWINSPAN ----------------------------------
#____________________________________________
# install.packages("twinspan",repos = c("https://jarioksa.r-universe.dev", "https://cloud.r-project.org"))

# chargement du package
library(twinspan)
# On repart de la matrice especes x releves
# Application de twinspan
tw <- twinspan(data_releve_matrix,   # data.frame ou matrix
                cutlevels = c(0, 0.5, 5, 25, 50, 75),
                levmax = 6,        # profondeur max de divisions
                groupmin = 5)      # taille minimale d'un groupe divisible)  # pseudo-espèces
      
                     
plot(tw, height = "chi",main = "Roleček et al. 2009")   # dendrogramme des divisions
summary(tw)

cl_modified <- cuth(tw, ngroups = 10)   # clusters selon Roleček et al. 2009

## --- Ordre phytosociologique (celui de twintable) ---
ord_quad <- tw$quadrat$index     # ordre des relevés
ord_spec <- tw$species$index     # ordre des espèces

tab <- t(data_releve_matrix[ord_quad, ord_spec])  # espèces en lignes, relevés en colonnes
tab <- as.data.frame(tab)
tab$espece <- rownames(tab)        # les noms d'espèces doivent être une colonne
tab <- tab[, c("espece", setdiff(names(tab), "espece"))]

# Séparation espece / strate (sur le DERNIER underscore du nom)
tab$strate <- sub("^.*_", "", tab$espece)      # ce qui suit le dernier _
tab$espece <- sub("_[^_]*$", "", tab$espece)   # le nom sans le suffixe

# Mettre la colonne strate en 2e position
tab <- tab[, c("espece", "strate", setdiff(names(tab), c("espece", "strate")))]

## --- Conversion : 0 -> "", 0.5 -> "+", le reste en caractère ---
tab_disp <- tab

num_cols <- vapply(tab_disp, is.numeric, logical(1))
tab_disp[num_cols] <- lapply(tab_disp[num_cols], function(col) {
  out <- as.character(col)
  out[!is.na(col) & col == 0]   <- ""    # absences -> cellule vide
  out[!is.na(col) & col == 0.5] <- "+"   # "+" de Braun-Blanquet
  out[is.na(col)]               <- ""    # NA éventuels -> vide aussi
  out
})

tab_disp <- as.data.frame(tab_disp, stringsAsFactors = FALSE)


## --- Feuille 2 : appartenance des relevés ---
releves <- data.frame(
  releve  = rownames(data_releve_matrix)[ord_quad],
  cluster = as.integer(cl_modified[ord_quad])
)

## --- Feuille 3 : hétérogénéité des groupes (choix du k) ---
chi_vec <- twintotalchi(tw)

hetero <- data.frame(
  groupe = seq_along(chi_vec),   # n° du groupe (ou de la division)
  chi    = as.numeric(chi_vec)
)

## --- Feuille 4 : historique des divisions (valeurs propres, indicateurs) ---
divisions <- capture.output(summary(tw))

## --- Export XLSX multi-feuilles ---

# --- Styles ---
# fond vert clair peu saturé pour les cellules non vides
fill_vert <- "#C8E6C9"          # vert clair désaturé ; alternative : "#D4EAD4"
style_cell <- createStyle(
  fgFill        = fill_vert,
  halign        = "center",
  fontSize      = 9,
  borderColour  = "#B0B0B0",
  border        = c("top", "bottom", "left", "right")
)
# en-têtes (noms d'espèces + noms de relevés)
style_header <- createStyle(
  textDecoration = "bold",
  fgFill          = "#E8E8E8",
  halign          = "center",
  border          = "Bottom",
  borderColour    = "#404040"
)
# colonne des noms d'espèces
style_especes <- createStyle(
  textDecoration = "bold",
  fontSize        = 9,
  halign          = "left"
)

# ---  Classeur ---
wb <- createWorkbook()

# ---- Feuille 1 : Tableau ----
addWorksheet(wb, "Tableau")
writeData(wb, "Tableau", tab_disp, rowNames = FALSE)

# en-têtes (ligne 1, colonne 2..n ; colonne 1 = "espece")
addStyle(wb, "Tableau", style_header,
         rows = 1, cols = 1:ncol(tab_disp), gridExpand = TRUE)
# noms d'espèces (colonne 1, lignes 2..n)
addStyle(wb, "Tableau", style_especes,
         rows = 2:(nrow(tab_disp) + 1), cols = 1, gridExpand = TRUE)

# cellules non vides -> fond vert : on parcourt la matrice logique

# matrice logique : une cellule est "remplie" si elle n'est ni vide ni NA
char_mat <- as.matrix(tab_disp[, -(1:2)])   # toutes les colonnes SAUF espece (1) et strate (2)
non_vide <- !is.na(char_mat) & char_mat != ""

# positions réelles des colonnes de relevés dans la feuille : 3..n
num_pos <- 3:ncol(tab_disp)

for (i in seq_len(nrow(non_vide))) {
  cols_remplies <- which(non_vide[i, ])
  if (length(cols_remplies) > 0) {
    addStyle(wb, "Tableau", style_cell,
             rows = i + 1,
             cols = num_pos[cols_remplies],
             gridExpand = FALSE)
  }
}

# largeurs : espèces larges, relevés étroits
setColWidths(wb, "Tableau", cols = 1, widths = 30)
setColWidths(wb, "Tableau", cols = 2:(ncol(tab_disp)), widths = 7)

# figer la première ligne et la première colonne
freezePane(wb, "Tableau", firstActiveRow = 2, firstActiveCol = 2)

# ---- Feuille 2 : Relevés / clusters ----
addWorksheet(wb, "releves_clusters")
writeData(wb, "releves_clusters", releves)
addStyle(wb, "releves_clusters", style_header,
         rows = 1, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "releves_clusters", cols = 1:2, widths = c(20, 10))

# ---- Feuille 3 : Hétérogénéité ----
hetero <- hetero[order(-hetero$chi), ]   # tri par hétérogénéité décroissante
addWorksheet(wb, "heterogeneite")
writeData(wb, "heterogeneite", hetero)
addStyle(wb, "heterogeneite", style_header,
         rows = 1, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "heterogeneite", cols = 1:2, widths = c(12, 12))

# ---- Feuille 4 : Divisions (sortie summary) ----
addWorksheet(wb, "divisions")
writeData(wb, "divisions",
          data.frame(ligne = seq_along(divisions), texte = divisions))
setColWidths(wb, "divisions", cols = 1, widths = 8)
setColWidths(wb, "divisions", cols = 2, widths = 110)

# ---  Sauvegarde ---
saveWorkbook(wb, "tableau_phytosociologique.xlsx", overwrite = TRUE)

