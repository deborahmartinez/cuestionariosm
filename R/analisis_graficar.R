#' Parámetros estéticos del tema de las gráficas
#'
#' @return
#' @export
#'
#' @examples
tema <- function(){
  theme_minimal()+
    theme(text = element_text(family = "Lato"),
          plot.title = element_text(hjust = 0),
          axis.text.y = element_text(color = "#4B5154"),
          axis.text.x = element_text(color = "#4B5154"),
          panel.grid.minor = element_blank(),
          panel.grid.major =  element_line(linetype = "longdash"),
          axis.line.x = element_line(size = .4, color = "#A7B3B8"),
          panel.grid.major.y = element_blank(),
          legend.position="bottom",
          legend.title = element_blank(),
          plot.background = element_rect(fill = "#F7F6F1", color = "#F7F6F1")
    )
}


#' Title
#'
#' @param bd base de datos del cuestionario
#' @param grupo patrón (string) de la respuesta multi, que debe ser el nombre de la variable
#'
#' @return
#' @export
#'
#' @examples
transformar_multirespuesta <- function(bd, grupo) {
  aux <- bd %>% select(matches(glue::glue("^{grupo}_"))) %>%
    filter_all(any_vars(!is.na(.)))
  aux <- aux %>% tidyr::pivot_longer(everything()) %>%
    mutate(n = nrow(aux)) %>% rename(!!grupo := value) %>%
    mutate(!!grupo := if_else(name == glue::glue("{grupo}_otro"), "Otro",
                                !!rlang::sym(grupo)))

  return(aux)
}


#' Para analizar y hacer una gráfica de barras de una pregunta en particular
#'
#' @param bd base de datos
#' @param pregunta nombre de la columna a analizar
#' @param titulo Título que se quiere en la gráfica
#' @param multi
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
distBarras <- function(bd, pregunta,multi = F, titulo = ""){

  # De la base de datos, sacar porcentaje por pregunta y redondear
  if(multi){
    bd <- bd %>%  count({{pregunta}}) %>%  na.omit() %>%
      mutate(pct = n/unique(bd$n))
  } else{
    bd <-  bd %>%
      count({{pregunta}} ) %>% na.omit() %>%
      mutate(pct =n/sum(n))
  }

  # Gráfica
  browser()
  p<-bd %>%
    ggplot(aes(x=forcats::fct_reorder( {{pregunta}}, pct),
               y=pct,label = pct %>%  scales::percent(accuracy = 1))) +
    # ¿Parametrizar width, fill?
    geom_bar(stat = "identity", width = .5, alpha = .8)+
    coord_flip()+
    scale_y_continuous(labels=scales::percent)+
    labs(title = titulo, x = "", y = "" )+
    tema()+
    geom_hline(yintercept = 0, linetype = "solid", size = .6, color = "#395C6B")

  return(p)
}
