#' Para leer la base de datos de Survey Monkey
#'
#' @param archivo dirección de la base de datos del cuestionario
#' @param dir_codigos dirección en la que se escribirá la base de códigos
#' @param crear_codigo Si se quiere o no crear la base de códigos
#'
#' @return bd y codigos.csv
#' @export
#' @import dplyr
#' @examples
leer_base <- function(archivo, dir_codigos = "data/", crear_codigo= T){
  bd <- readr::read_csv(file = archivo  )
  nombre <- names(bd)
  info_aux <- bd %>%  slice(1) %>%  as.character()
  bd <-readr::read_csv(file = archivo, skip = 2)
  clases <- purrr::map_chr(.x = bd,~.x %>% class )
  if(crear_codigo){
    codigos <- tibble(nombre, info_aux, clase=clases, codigo= "")
    codigos %>% readr::write_excel_csv(glue::glue("{dir_codigos}codigos.csv"))
  }
  return(bd)
}

#' Para poner los nombres de códigos a la base de respuestas
#'
#' @param bd base de respuestas
#' @param bd_codigos base de datos de códigos con la columna "codigo"
#'
#' @return bd renombrada con codigos
#' @export
#'
#' @examples
nombrar_base <- function(bd, bd_codigos){
  names(bd)<-bd_codigos$codigo
  return(bd)
}
