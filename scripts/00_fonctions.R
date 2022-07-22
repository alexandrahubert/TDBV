#' Produire un nuage de points personnalisé
#'
#' @param data Le dataframe contenant les données.
#' @param x,y,col Variables pour l'abscisse, l'ordonnée et la couleur des points.
#' @param_label Variable qui apparaît dabs les popups si l'on passe en ggplotly().
#' @param tendance Caractère. Type de courbe de tendance au sens de ggplot2::geom_smooth().
#'     Par défaut c'est linéaire ("lm").
#' @param tendance_tous_jeux Booléen. Ajouter une courbe de tendance sur l'ensemble des données ?
#'     Par défaut tendance_tous_jeux = TRUE
#' @param y_log Booléen. L'axe des ordonnées doit-il être en échelle log ? Par défaut TRUE.
#' @param x_lab,y_lab Caractère. Etiquette des axes.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' mon_nuage(data = ref,
#' x = Surface_BV_km2,
#' y = Lpb_moy,
#' col = jeu_donnees)
#' }
mon_nuage <- function(data,
                      x,
                      y,
                      col,
                      label,
                      tendance = "lm",
                      tendance_tous_jeux = TRUE,
                      y_log = TRUE,
                      x_lab = NULL,
                      y_lab = NULL)
  
  {
  
  g <- ggplot(data = data,
              aes(x = {{ x }},
                  y = {{ y }},
                  col = {{ col }},
                  label = {{ label }})) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = tendance) +
  scale_color_manual(values = wes_palette(n = 3,
                                          name = "Darjeeling1"),
                     name = "Jeu de donn\u00e9es")  +
  theme(legend.position = "bottom")
  
  # gestion des échelles et étiquettes des axes
  if(y_log) {g <- g + scale_y_log10() }
  if(!is.null(x_lab)) {g <- g + labs(x = x_lab) }
  if(!is.null(y_lab)) {g <- g + labs(y = y_lab) }
  
  # éventuellement ajout de la courbe de tendance tous jeux de données confondus
  if(tendance_tous_jeux) {
  g <- g +
    geom_smooth(data = data,
                aes(x = {{ x }},
                    y = {{ y }}),
                col = "grey50",
                linetype = "dashed",
                se = FALSE) }
  
  # affichage
  g

}

# --------------------------------------- #
#  modèles
# --------------------------------------- #



    # -------------- Fonction pour un modèle ------------------------- #
    lm_unitaire <- function(data,
                            var_dep,
                            jeu_donnees_selectionne = NULL) {
      
      # construction de la formule en fonction de la variable dépendante

        fm <- as.formula(paste0("log(",
                                var_dep,
                                ", base = 10) ",
                                "~ log(Surface_BV_km2, base = 10) + pente_eau_m_m"))

        
      # sélection des données
        
        mod_data <- data %>% 
          filter(jeu_donnees == jeu_donnees_selectionne,
                 !is.na(Surface_BV_km2),
                 !is.na(pente_eau_m_m))
      
      # on cale le modèle initial
      mod <- lm(formula = fm,
                data = mod_data)
      
      # recherche et suppression des outliers
      dist_cook <- cooks.distance(mod)
      seuil_cook <- 4 / nrow(data)
      mod_data <- mod_data %>%
        cbind(dist_cook) %>% # ajout de la colonne avec les distances
        filter(dist_cook < seuil_cook) # suppression des observations avec distance > 4/N
      
      # on cale le modèle une fois les données expurgées des outliers
      mod <- lm(formula = fm,
                data = mod_data)
      
      # récupération de ce qui ous intéresse dans les résultats et mise en forme

      resultat <-
        summary(mod)$coefficients[c("(Intercept)",
                                    "log(Surface_BV_km2, base = 10)",
                                    "pente_eau_m_m"),] %>%
        as.data.frame() %>%
        select(coef = Estimate,
               pval = `Pr(>|t|)`) %>%
        mutate(
          sig = case_when(
            pval > 0.05 ~ "NS",
            pval <= 0.05 & pval > 0.01 ~ "*",
            pval <= 0.01 & pval > 0.001 ~ "**",
            TRUE ~ "***"
          )
        ) %>%
        mutate(text = paste0(round(coef, 3),
                             " (",
                             sig,
                             ")")) %>%
        select(text) %>%
        t()
        
      
      # on nomme la ligne d'après le nom de la métrique
      row.names(resultat) <- paste(var_dep, jeu_donnees_selectionne, sep = " / ")
      
      # ajout du r2
      # resultat <- c(resultat, summary(mod)$adj.r.squared %>% 
      #                 round(digits = 3))

      as.data.frame(resultat) %>% 
        mutate(r2 = summary(mod)$adj.r.squared)
      
    }



lm_unitaire(data = ref,
            var_dep = "Lpb_moy",
            jeu_donnees_selectionne = "tbv_ref")
