# Chargement des bibliothèques nécessaires
library(openxlsx)
library(ggplot2)
library(ggrepel)
library(shiny)
library(tidyr)
library(dplyr)
library(DT)        # Pour les tableaux interactifs
library(factoextra)
library(ape)       # Pour la visualisation de l'arbre de classification
library(data.table) # Pour utiliser dcast()
library(vegan)     # Pour NMDS et analyse de similarité
library(NbClust) # pour le nombre de groupe dans la CAH
library(indicspecies) # Pour les espèces indicatrices avec multipatt()

# Fonction pour convertir les codes Braun-Blanquet en valeurs numériques (ex: '+' = 1, '1' = 2, etc.)
convert_bb <- function(x) {
  bb_codes <- c("+" = 1, "1" = 2, "2" = 3, "3" = 4, "4" = 5, "5" = 6)
  return(as.numeric(bb_codes[as.character(x)]))
}

# Interface utilisateur principale (barre de navigation)
ui <- navbarPage("Application Phytosociologique",
                 
                 # Onglet principal pour l'analyse
                 tabPanel("Analyse",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file", "Charger un fichier CSV", accept = ".csv"),
                              uiOutput("select_releves"),
                              numericInput("n_clusters", "Nombre de groupes à identifier:", value = 2, min = 2),
                              actionButton("run", "Lancer l'analyse")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Données brutes (head)", tableOutput("data_head")),
                                tabPanel("Données pivotées", DTOutput("pivoted_data"),downloadButton("download_pivoted", "Télécharger les données pivotées")),
                                tabPanel("Classification",
                                         plotOutput("clustering_plot"),
                                         br(),
                                         h4("Niveaux de fusion (aide au choix de la coupe)"),
                                         plotOutput("inertia_plot"),
                                         br(),
                                         h4("Nombre optimal de groupes (NbClust)"),
                                         verbatimTextOutput("nbclust_text")
                                ),
                                tabPanel("Ordination (NMDS)", plotOutput("nmds_plot")),
                                tabPanel("Espèces caractéristiques",
                                         HTML("<p><strong>Définition :</strong> Cette section utilise la fonction <code>multipatt()</code> du package <code>indicspecies</code> pour identifier les espèces les plus représentatives (indicatrices) des groupes de relevés définis par la classification hiérarchique. Elle calcule pour chaque espèce un score combinant sa fidélité (présence fréquente dans un groupe) et sa spécificité (présence exclusive dans ce groupe). Même sans p-value significative, une forte valeur d'indice peut indiquer une affinité marquée avec un groupe.</p>"),
                                         DTOutput("indval_table"),
                                         DTOutput("groupe_releves_table"),
                                         downloadButton("download_indval", "Télécharger les espèces caractéristiques"),
                                         downloadButton("download_groupes", "Télécharger les relevés par groupe")
                                )
                              )
                            )
                          )
                 ),
                 
                 # Onglet séparé pour visualiser les espèces par relevé
                 tabPanel("Liste des espèces par relevé",
                          DTOutput("especes_par_releve")
                 )
)

# Partie serveur de l'application
server <- function(input, output, session) {
  
  data_input <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE, sep = ";")
  })
  
  output$data_head <- renderTable({
    head(data_input(), 10)
  })
  
  output$select_releves <- renderUI({
    req(data_input())
    releves <- unique(data_input()$releve)
    checkboxGroupInput("selected_releves", "Relevés à inclure:", choices = releves, selected = releves)
  })
  
  data_pivoted <- eventReactive(input$run, {
    df <- data_input()
    colnames(df) <- tolower(colnames(df))
    required_cols <- c("releve", "espece", "strate", "abondance_dominance")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("Colonnes manquantes dans le fichier :", paste(missing_cols, collapse = ", ")))
    }
    df <- df %>%
      filter(releve %in% input$selected_releves) %>%
      mutate(abondance_dominance = trimws(as.character(abondance_dominance))) %>%
      mutate(abondance = convert_bb(abondance_dominance)) %>%
      filter(!is.na(abondance)) %>%
      mutate(espece_strate = paste0(espece, "_", strate))
    df_dt <- as.data.table(df)
    df_pivot <- dcast(
      df_dt,
      releve ~ espece_strate,
      value.var = "abondance",
      fun.aggregate = sum,
      fill = 0
    )
    df_pivot <- as.data.frame(df_pivot)
    rownames(df_pivot) <- df_pivot$releve
    df_pivot <- df_pivot[ , !(names(df_pivot) %in% c("releve"))]
    df_pivot[] <- lapply(df_pivot, as.numeric)
    attr(df_pivot, "releve") <- df_dt[, unique(releve)]
    return(df_pivot)
  })
  
  output$pivoted_data <- renderDT({
    req(data_pivoted())
    datatable(data_pivoted(), options = list(pageLength = 10))
  })
  
  output$download_pivoted <- downloadHandler(
    filename = function() {
      paste0("donnees_pivotees_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- data_pivoted()
      releve <- attr(df, "releve")
      df_export <- cbind(releve = releve, df)
      write.csv2(df_export, file, row.names = FALSE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # CLASSIFICATION DE REFERENCE (une seule fois, partagée par tous les affichages)
  # ---------------------------------------------------------------------------
  # Bray-Curtis : une seule fois, réutilisé partout
  bray_dist <- reactive({
    req(data_pivoted())
    vegdist(data_pivoted(), method = "bray")
  })
  
  # CAH aussi mise en cache : changer le nombre de groupes
  hc_clust <- reactive({
    hclust(bray_dist(), method = "ward.D2")
  })
  
  # --------
  # NBCLUST : détermination du nombre optimal de groupes
  # --------
  nbclust_result <- eventReactive(input$run, {
    mat <- data_pivoted()
    dist_mat <- bray_dist()
    
    indices <- c("frey", "mcclain", "cindex", "silhouette", "dunn")
    
    best_nc_list <- lapply(indices, function(idx) {
      tryCatch({
        # capture.output() intercepte tout ce que NbClust imprime (cat, message, warning)
        invisible(capture.output(
          res <- NbClust(diss = dist_mat, distance = NULL, method = "ward.D2",
                         index = idx, min.nc = 2, max.nc = 30)$Best.nc
        ))
        res
      },
      error = function(e) NA
      )
    })
    
    names(best_nc_list) <- indices
    best_nc_list
  })
  # Affichage du résultat
  output$nbclust_text <- renderPrint({
    res <- nbclust_result()
    
    # Affiche une ligne par indice
    for (idx in names(res)) {
      x <- res[[idx]]
      if (length(x) == 1 && is.na(x)) {
        cat(sprintf("%-12s : échec du calcul\n", idx))
      } else {
        cat(sprintf("%-12s : %s groupes (critère = %s)\n",
                    idx, x[1], x[2]))
      }
    }
    
    # Nombre optimal
    nb_values <- sapply(res, function(x) {
      if (length(x) == 1 && is.na(x)) NA else as.numeric(x[1])
    })
    nb_values <- na.omit(nb_values)
    if (length(nb_values) > 0) {
      cat("\n Nombre optimal :", names(which.max(table(nb_values))), "groupes\n")
    }
  })
  
  # Renvoie une liste :  hclust object + matrice + attributs de coupure
  hc_result <- reactive({
    mat <- data_pivoted()
    clust <- hc_clust()
    groups <- cutree(clust, k = input$n_clusters)          # ids BRUTS de cutree
    leaf_order <- order.dendrogram(as.dendrogram(clust))   # ordre des feuilles (gauche->droite)
    list(mat = mat, clust = clust, groups = groups, leaf_order = leaf_order)
  })
  
  # Ordre (gauche->droite) des groupes BRUTS de cutree dans le dendrogramme
  dendro_cluster_order <- reactive({
    res <- hc_result()
    unique(res$groups[res$leaf_order])
  })
  
  # RENUMEROTATION : chaque groupe reçoit un numéro = sa position gauche->droite
  # Groupe 1 = premier groupe à gauche du dendrogramme, groupe 2 = suivant, etc.
  group_labels <- reactive({
    res <- hc_result()
    relabel <- setNames(seq_along(dendro_cluster_order()), dendro_cluster_order()) # id brut -> 1..k
    new <- unname(relabel[as.character(res$groups)])
    names(new) <- names(res$groups)     # noms = relevés
    new[rownames(res$mat)]               # aligné sur l'ordre des relevés
  })
  
  # Affectation factorielle de chaque relevé (niveaux 1..k, triés de gauche à droite)
  cluster_membership <- reactive({
    factor(group_labels(), levels = as.character(seq_len(input$n_clusters)))
  })
  
  # Palette : une couleur par numéro de groupe 1..k (gauche->droite)
  group_palette <- reactive({
    setNames(grDevices::rainbow(input$n_clusters), as.character(seq_len(input$n_clusters)))
  })
  
  # Couleurs utilisées pour le dessin des rectangles du dendrogramme (ordre dendro)
  # NB : fviz_dend attribue lui-même ses couleurs internes par groupe de coupe.
  ordered_colors <- reactive({
    grDevices::rainbow(input$n_clusters)[dendro_cluster_order()]
  })
  
  output$clustering_plot <- renderPlot({
    res <- hc_result()
    fviz_dend(
      as.dendrogram(res$clust),
      k = input$n_clusters,
      k_colors = "black",
      color_labels_by_k = FALSE,
      rect = TRUE,
      rect_fill = TRUE,
      rect_border = ordered_colors(),
      rect_lty = 1,
      ggtheme = theme(
        plot.background = element_rect(fill = "#f4f8f9", color = NA),
        panel.background = element_rect(fill = "#f4f8f9", color = "darkgreen"),
        text = element_text(family = "sans", color = "darkgreen"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        panel.grid = element_blank()
      ),
      main = "Classification Ascendante Hiérarchique"
    )
  })
  
  # Diagramme en escalier des hauteurs de fusion (aide au choix du nombre de groupes)
  output$inertia_plot <- renderPlot({
    cah_result <- hc_clust()
    n <- length(cah_result$height)      # nombre de fusions = nb_releves - 1
    nb_releves <- n + 1
    
    # Axe x = nombre de groupes restants après la fusion (n-1, n-2, ..., 1)
    plot(x = (nb_releves - 1):1,
         y = cah_result$height,
         type = "s",
         main = "Niveaux de fusion de la CAH",
         xlab = "Nombre de groupes",
         ylab = "Hauteur de fusion",
         col = "darkgreen", lwd = 2)
    
    # Marqueur de coupe pour le k choisi : position x = k
    num_groups <- input$n_clusters
    abline(v = num_groups,
           h = cah_result$height[nb_releves - num_groups],
           col = "red", lty = 2)
  })
  
  output$nmds_plot <- renderPlot({
    mat <- hc_result()$mat
    groups <- cluster_membership()
    nmds <- metaMDS(bray_dist(), k = 2, trymax = 100, autotransform = FALSE)
    nmds_sites <- as.data.frame(scores(nmds, display = "sites"))
    nmds_sites$label <- rownames(nmds_sites)
    nmds_sites$Groupe <- groups[rownames(nmds_sites)]
    
    stress_val <- round(nmds$stress, 3)
    
    ggplot(nmds_sites, aes(x = NMDS1, y = NMDS2)) +
      geom_point(aes(color = Groupe), size = 3) +
      scale_color_manual(values = ordered_colors()) +
      ggrepel::geom_text_repel(aes(label = label), size = 3, max.overlaps = 100) +
      labs(title = "Ordination NMDS",
           subtitle = paste("Stress:", stress_val),
           x = "NMDS 1", y = "NMDS 2") +
      theme_minimal() +
      coord_equal()
  })
  
  
  output$indval_table <- renderDT({
    mat <- hc_result()$mat
    groups <- cluster_membership()          # numérotés 1..k (gauche->droite)
    if (length(unique(groups)) < 2) {
      return(datatable(data.frame(Message = "Moins de 2 groupes détectés")))
    }
    indval_res <- multipatt(mat, groups, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
    indval_df <- as.data.frame(indval_res$sign)
    if (nrow(indval_df) == 0) {
      return(datatable(data.frame(Message = "Aucune espèce caractéristique détectée (p > 0.05)")))
    }
    indval_df$Espèce <- rownames(indval_df)
    indval_df <- indval_df %>%
      filter(p.value <= 0.05) %>%
      mutate(Groupe = index) %>%
      select(Espèce, Groupe, stat, p.value)
    colnames(indval_df) <- c("Espèce", "Groupe indicateur", "Indice IndVal", "p-value")
    datatable(indval_df, options = list(pageLength = 10, dom = 'Blfrtip'), filter = 'top')
  })
  
  output$groupe_releves_table <- renderDT({
    clusters <- cluster_membership()       # factor, niveaux ordonnés 1..k
    df_groupes <- data.frame(Releve = names(clusters), Groupe = clusters)
    df_summary <- df_groupes %>%
      group_by(Groupe) %>%
      summarise(Relevés = paste(Releve, collapse = ", ")) %>%
      ungroup()
    # Les groupes sont affichés dans l'ordre 1..k = gauche->droite du dendrogramme
    df_summary <- df_summary[order(as.numeric(as.character(df_summary$Groupe))), , drop = FALSE]
    datatable(df_summary, options = list(pageLength = 5))
  })
  
  output$download_indval <- downloadHandler(
    filename = function() {
      paste0("especes_caracteristiques_", Sys.Date(), ".csv")
    },
    content = function(file) {
      mat <- hc_result()$mat
      groups <- cluster_membership()
      indval_res <- multipatt(mat, groups, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
      indval_df <- as.data.frame(indval_res$sign)
      indval_df$Espèce <- rownames(indval_df)
      indval_df <- indval_df %>%
        filter(p.value <= 0.05) %>%
        mutate(Groupe = index) %>%
        select(Espèce, Groupe, stat, p.value)
      colnames(indval_df) <- c("Espèce", "Groupe indicateur", "Indice IndVal", "p-value")
      write.csv2(indval_df, file, row.names = FALSE)
    }
  )
  
  output$download_groupes <- downloadHandler(
    filename = function() {
      paste0("releves_par_groupe_", Sys.Date(), ".xlsx") # Extension .xlsx
    },
    content = function(file) {
      clusters <- cluster_membership()
      
      # 1. Création du tableau "Détail" (Non aggrégé : Relevé | Groupe)
      df_detail <- data.frame(Releve = names(clusters), Groupe = clusters)
      
      # 2. Création du tableau "Résumé" (Aggrégé), trié 1..k
      df_summary <- df_detail %>%
        group_by(Groupe) %>%
        summarise(Relevés = paste(Releve, collapse = ", ")) %>%
        ungroup()
      df_summary <- df_summary[order(as.numeric(as.character(df_summary$Groupe))), , drop = FALSE]
      
      # 3. Création du fichier Excel avec openxlsx
      wb <- createWorkbook()
      
      # Ajout de la feuille 1 : Résumé
      addWorksheet(wb, "Résumé par Groupe")
      writeData(wb, "Résumé par Groupe", df_summary)
      
      # Ajout de la feuille 2 : Détail (votre demande spécifique)
      addWorksheet(wb, "Détail Relevés")
      writeData(wb, "Détail Relevés", df_detail)
      
      # Sauvegarde
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$especes_par_releve <- renderDT({
    req(data_input())
    df <- data_input()
    colnames(df) <- tolower(colnames(df))
    required_cols <- c("releve", "espece", "strate", "abondance_dominance")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      return(DT::datatable(data.frame(Erreur = paste("Colonnes manquantes:", paste(missing_cols, collapse=", ")))))
    }
    datatable(
      df %>% dplyr::select(releve, espece, strate, abondance_dominance),
      options = list(pageLength = 10, dom = 'Blfrtip'),
      filter = 'top'
    )
  })
}

shinyApp(ui = ui, server = server)
