# --------------------------------------- #
#  GRAPHIQUES
# --------------------------------------- #

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
#' y = Lpb,
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
  scale_x_log10(labels = label_number()) +
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
# --------------------------- fin mon_nuage() -------------------------

ma_densite <- function(data,
                       x,
                       x_lab = NULL,
                       y_lab = "Densit\u00e9 de probabilit\u00e9",
                       x_log = FALSE)

{
  data <- data %>%
    filter(pente_eau_m_m > 0)
  
  moy <- data %>% 
    group_by(jeu_donnees) %>% 
    summarise(moy = mean({{ x }}))
  
  g <- ggplot(data = data,
              aes(x = {{ x }},
                  col = jeu_donnees,
                  fill = jeu_donnees)) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = wes_palette(n = 3,
                                          name = "Darjeeling1")) +
  scale_fill_manual(values = wes_palette(n = 3,
                                         name = "Darjeeling1")) +
  labs(y = "Densité de probabilité",
       fill = "",
       col = "") +
  geom_vline(xintercept = moy$moy,
             col = wes_palette(n = 3,
                               name = "Darjeeling1"),
             size = 1) +
  theme(legend.position = "bottom") +
  labs(y = y_lab)
  
  # gestion des échelles et étiquettes des axes
  if(x_log) {g <- g + scale_x_log10() }
  if(!is.null(x_lab)) {g <- g + labs(x = x_lab) }
  
  g
  
}

# ma_densite(data = ref,
#            x = Lpb)

# --------------------------- fin ma_sensite() -------------------------



# --------------------------------------- #
#  modèles
# --------------------------------------- #


lm_unitaire <- function(data,
                        var_dep,
                        jeu_donnees_selectionne = NULL,
                        pente_incluse = TRUE) {
  
  # construction de la formule en fonction de la variable dépendante
  fm <- 
    paste0(
     # "log10(1 + ",
      var_dep,
     # ") ",
      "~ log10(1 + Surface_BV_km2)"
    )
  
  if (pente_incluse) {
    fm <- paste0(fm, "+ log10(1 + pente_eau_m_m)")
  }
  
  
  fm <- as.formula(fm)
  
  # sélection des données
  
  mod_data <- data %>%
    filter(
      jeu_donnees %in% jeu_donnees_selectionne,
      !is.na(Surface_BV_km2),
      !is.na(pente_eau_m_m),
      !is.na({{ var_dep }}),
      !is.infinite({{ var_dep }})
    )
  
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
  
  if (pente_incluse) {
    
  resultat <-
    summary(mod)$coefficients[c("(Intercept)",
                                "log10(1 + Surface_BV_km2)",
                                "log10(1 + pente_eau_m_m)"), ]
  }else{
    
    resultat <-
      summary(mod)$coefficients[c("(Intercept)",
                                  "log10(1 + Surface_BV_km2)"), ]
  }
  
  resultat <-  resultat %>% 
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
  jeux_donnees <- paste(jeu_donnees_selectionne, collapse = " + ")
  row.names(resultat) <-
    paste(var_dep, jeux_donnees, sep = " / ")
  
  # ajout du r2
  resultat <- resultat %>%
    as.data.frame() %>% 
    mutate(r2 = summary(mod)$adj.r.squared)
  
  list(mod, resultat)
  
}



# lm_unitaire(data = ref,
#             var_dep = "Lpb",
#             jeu_donnees_selectionne = "carhyce_ref_armo",
#             pente_incluse = TRUE)
