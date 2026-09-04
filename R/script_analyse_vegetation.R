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

# Filtrer les données utiles
# Exemple ne choisir que certains relevés
data_releve_filtre = data_releve %>% filter(releve %in% c("R27","R20"))
# Visualisation de données

# Création de graphiques heatmap pour visualiser les similitudes des relevés

ggplot(data_releve_filtre, aes(x = espece, y = releve, fill = abondance_dominance))+
  geom_tile() +
  scale_fill_gradient(low = "#f8c856", high = "#228822") +  # Ajustez la palette de couleurs selon vos préférences
  labs(x = "Espèce", y = "Relevé", fill = "Abandance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Retirer des relevés
data_releve = data_releve %>% filter(!releve %in% c("R21","R8","R24","R23","R28"))

# Créer une colonne combinée pour strate et espèce
data_releve_combined <- data_releve %>%
  mutate(combination = paste(espece, strate, sep = "_")) %>%
  group_by(releve, combination) %>%
  summarise(abondance_dominance = mean(abondance_dominance, na.rm = TRUE), .groups = 'drop')


# Préparer les données pour la table matricielle avec les données agrégées
data_releve_matrix <- data_releve_combined %>%
  pivot_wider(names_from = combination, values_from = abondance_dominance, values_fill = 0) %>%
  column_to_rownames(var = "releve") %>%  as.matrix()  # Assure une conversion en matrice# Remplacer les valeurs manquantes par 0, au cas où

data_releve_matrix[is.na(data_releve_matrix)] = 0

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
  groups <- cutree(cah_result, k = num_groups)
  
  # Convertir les groupes en facteur
  groups <- factor(groups)
  
  # Assigner des couleurs de base aux groupes (Groupe 1 = couleur 1, etc.)
  my_colors <- rainbow(num_groups)
  
  # Récupérer l'ordre des clusters tel qu'affiché de gauche à droite
  leaf_order <- order.dendrogram(as.dendrogram(cah_result))
  groups_in_dendro_order <- groups[leaf_order]
  cluster_order_in_dendro <- unique(groups_in_dendro_order)
  
  # CORRECTION : Assigner les couleurs exactement dans l'ordre d'apparition
  dendro_colors <- my_colors[cluster_order_in_dendro]
  
  # Tracer le dendrogramme
  library(factoextra)
  fviz_dend(
    cah_result,
    k = num_groups,
    k_colors = rep("black", num_groups), # Attention au double virgule corrigé ici
    color_labels_by_k = FALSE,
    rect = TRUE,
    rect_border = dendro_colors, # Utilise les couleurs réordonnées
    rect_fill = FALSE,           
    rect_lty = 1,                
    ggtheme = theme_minimal()
  )


# Exécuter l'analyse NMDS ########################################
set.seed(124) # pour la reproductibilité
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
  scale_color_manual(values = my_colors) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, max.overlaps = 100) +
  labs(title = "Ordination NMDS",
       subtitle = paste("Stress:", stress_val),
       x = "NMDS 1", y = "NMDS 2") +
  theme_minimal() +
  coord_equal()


# Analyses des données environnementales ########################################


# Charger les données environnementales
env_data <- read_csv2("../data/envdata_releve.csv") # Remplacez par le chemin correct
env_data = env_data[order(env_data$Nom), ]
base::rownames(env_data) <- env_data$Nom # Assurez-vous que les lignes sont nommées par les relevés

#Retirer les relevés à retirer : 
env_data = env_data %>% filter(!Nom %in% c("R21","R8","R24","R23","R28"))%>%
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
    x -mean(x,na.rm = TRUE)/sd(x,na.rm=TRUE)
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
top_n <- 20 # CHOIX DU NOMBRE D'ESPECE A AFFICHER
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


df_species


# Indice Value ########################################

# Assurez-vous que 'groups' est un facteur
groups <- factor(groups)

# Utilisez votre matrice de communauté (relevés x espèces), PAS la matrice de distance.
# Je suppose qu'elle s'appelle 'data_releve_matrix' d'après votre script précédent.
indval_res <- multipatt(data_releve_matrix, groups, 
                        func = "IndVal.g", 
                        control = how(nperm = 999))

# Extraire le tableau des espèces significatives (p-value <= 0.05 par défaut)
indval_df <- as.data.frame(indval_res$sign)

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

