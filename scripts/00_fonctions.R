#' Produire un nuage de points personnalisé
#'
#' @param data Le dataframe contenant les données.
#' @param x,y,col Variables pour l'abscisse, l'ordonnée et la couleur des points.
#' @param tendance Caractère. Type de courbe de tendance au sens de ggplot2::geom_smooth().
#'     Par défaut c'est linéaire ("lm").
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
                      tendance = "lm")
  
  {
  
  g <- ggplot(data = data,
              aes(x = {{ x }},
                  y = {{ y }},
                  col = {{ col }})) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = tendance)

}




