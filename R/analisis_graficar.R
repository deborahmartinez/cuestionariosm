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

#' Tema para la gráfica de gauge
#'
#' @return
#' @export
#'
#' @examples
tema_gauge <- function(){
  theme_void()+
    theme(text = element_text(family = "Lato"),
          plot.title = element_text(hjust = .5),
          aspect.ratio = 1,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "#F7F6F1", color = NA),
          panel.background = element_rect(fill = "#F7F6F1", color = "#F7F6F1"),
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "cm")
    )
}


#' Parámetros estéticos de la gráfica de burbujas
#'
#' @return
#' @export
#'
#' @examples
tema_burbujas <- function(){
  theme_minimal()+
    theme(text = element_text(family = "Lato"),
          plot.title = element_text( hjust = 0),
          axis.text.y = element_text(color = "#4B5154"),
          axis.text.x = element_text(color = "#4B5154"),
          panel.grid.major = element_blank(),
          panel.grid.minor =  element_line(linetype = "longdash"),
          axis.line = element_line(size = .4, color = "#A7B3B8"),
          axis.title = element_blank(),
          legend.position="none",
          legend.title = element_blank(),
          plot.background = element_rect(fill = "#F7F6F1", color = "#F7F6F1")
    )
}


#' Parámetros estéticos de la gráfica de segmento
#'
#' @return
#' @export
#'
#' @examples
tema_segmento <- function(){
  theme(
    # Texto
    text = element_text(family = "Lato"),
    plot.title = element_text( colour = "#003049" ),
    axis.text.y = element_blank(),

    # Panel
    panel.background = element_blank(),
    # Grid
    panel.grid.major = element_blank(),
    # Ejes
    axis.line.x = element_line(colour = "grey70"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # Leyenda
    legend.position = "none",
    plot.background = element_rect(fill = "#F7F6F1", color = "#F7F6F1")
  )
}


#' Transformar la base de datos en el formato adecuado para graficar preguntas multirrespuesta
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
#' @param multi si se graficará una pregunta que es miltirrespuesta
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
graficar_barras <- function(bd, pregunta,multi = F, titulo = ""){

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
  # browser()
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

#' Función para hacer el procesamiento y la gráfica de lollipop o paleta
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta nombre de la columna que se va a graficar
#' @param multi si se graficará una pregunta que es miltirrespuesta
#' @param titulo  el título que se mostrará en la gráfica
#'
#' @return
#' @export
#'
#' @examples
graficar_paletas <- function(bd, pregunta, multi = F, titulo = ""){
  # De la base de datos, sacar porcentaje por pregunta y redondear
  if(multi){
    # browser()
    bd <- bd %>%  count({{pregunta}}) %>%  na.omit() %>%  mutate(pct = n/bd$n[1])
  } else{
    bd <-  bd %>%
      count({{pregunta}}) %>% na.omit() %>%
      mutate(pct = n/sum(n))
  }

  p<-bd %>% ggplot(aes(x=forcats::fct_reorder( {{pregunta}}, pct), y=pct)) +
    geom_segment( aes( xend={{pregunta}},y=0, yend=pct),size = 2.5)+
    geom_point(aes(y = pct), size =8,stroke = 2,alpha=.8) +
    coord_flip()+
    geom_text( aes(label = pct %>%  scales::percent(accuracy = 1), y = pct),
               color = "white", fontface="bold",size = 3)+
    scale_y_continuous(labels=scales::percent,limits = c(0,max(bd$pct) + .1))+
    labs(title =titulo, x = "", y = "" )+
    geom_hline(yintercept = 0, linetype = "solid", size = .6, color = "#395C6B")+
    tema()

  return(p)
}


#' Para graficar un gauge
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta nombre de la columna que se va a graficar
#' @param grupo si si quiere agrupar por otra variable
#' @param colorPolos color de las líneas del gauge
#' @param titulo título que se mostrará en la gráfica
#' @param tam el tamaño de la la línea gauge máxima
#'
#' @return
#' @export
#'
#' @examples
graficar_media_gauge <- function(bd, pregunta, grupo,
                       colorPolos=c("#F78154","#78B874"), titulo="", tam=10){
  bd <- bd %>%
    group_by({{grupo}}) %>%
    summarise({{pregunta}}:=mean({{pregunta}}, na.rm = T))
  g <-  bd %>%
    ggplot() +
    annotate(x=1, xend=1, y=0, yend=10,size=tam*1.1, color="#586994",geom = "segment", alpha=.5)+
    geom_segment(aes(x = 1, y = 0, color={{pregunta}}, xend = 1, yend = {{pregunta}}),
                 lineend = "round", linejoin = "round",
                 size =  tam, arrow = arrow(length = unit(0, "inches"))
    ) +
    scale_color_gradient2(midpoint = 5, low=colorPolos[1],
                          high = colorPolos[2], mid = "#F78154")+
    labs(title = titulo)+
    geom_text(aes(color={{pregunta}},
                  label=round({{pregunta}},digits = 1)),
              x=-5, y=5,size=30/.pt, fontface="bold")+
    # facet_wrap(~grupo)+
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(-5,2)) +
    scale_y_continuous(limits = c(0, 10))+
    tema_gauge()
  return(g)
}


#' Para graficar un respuesta en porcentaje con "bolitas" de colores por categoría
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta nombre de la columna que se va a graficar
#' @param titulo  Título que aparecerá en la gráfica
#'
#' @return
#' @export
#'
#' @examples
graficar_item <- function(bd, pregunta, titulo = ""){
  bd <- bd %>%  count({{pregunta}}) %>% na.omit() %>%  mutate(n= round(n*100/sum(n))) %>%  arrange(desc(n))
  bd <- tibble (respuesta =rep(bd %>%  pull({{pregunta}}), bd %>%  pull(n)),
                x = rep(c(1:10), c(rep(10, 10))),
                y= rep(10: 1, 10))

  p<-bd %>%   ggplot(aes(x =x, y = y, color =respuesta))+
    geom_point(size = 5)+
    theme_minimal()+
    labs(title = titulo, y = "", x = "" ) +
    theme(text = element_text(family = "Avenir", size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.background = element_rect(fill = "#F7F6F1", color = "#F7F6F1"))
  return(p)
}





#' Para graficar burbujas
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta1 nombre de la primera columna a graficar
#' @param pregunta2 nombre de la columna que se graficará con la segunda
#'
#' @return
#' @export
#' @import stringr
#' @examples
graficar_burbujas <- function(bd, pregunta1, pregunta2){
  bd <- bd %>%
    count({{pregunta1}}, {{pregunta2}})
  bd %>%
    ggplot()+
    geom_point(aes(y=str_wrap({{pregunta1}},10),
                   x=str_wrap({{pregunta2}}, 20),
                   size=n), alpha=.6) +
    geom_text(aes(y=str_wrap({{pregunta1}},10),
                  x=str_wrap({{pregunta2}},20),
                  label=n), size=12/.pt, fontface="bold") +
    scale_size_area(max_size = 12*4/.pt)+
    tema_burbujas()

}


#' Para graficar pirámide poblacional o una pregunta con solo 2 tipos de repuestas agrupada
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta nombre de la columna que se va a graficar (usualmente edad)
#' @param grupo Nombre de la columna por la que se agrupará (usualmente sexo)
#' @param titulo Título que aparecerá en la gráfica
#'
#' @return
#' @export
#'
#' @examples
graficar_piramide <- function(bd, pregunta, grupo, titulo = ""){
  # Paso 1: Frecuencias
  bd <- bd %>%  count({{grupo}}, {{pregunta}})
  grupos <- unique(bd %>% pull({{grupo}}))
  ncat<-n_distinct(bd %>% pull({{pregunta}}))

  # bd$pregunta<- fct_reorder(bd$pregunta, n)
  # Cambiar la direccion
  bd <- bd %>%
    mutate(n2 = case_when({{grupo}} ==grupos[1]~n,
                          {{grupo}} ==grupos[2]~ -n))

  p <- bd %>%
    #Graficar
    ggplot(aes(x = n2, y = {{pregunta}}, fill = {{grupo}}))+
    geom_col(alpha =.9)+
    #quitar signo negativo y balancear
    scale_x_continuous(labels = abs, limits = max(bd$n) * c(-1,1))+
    # Títulos
    labs(title =titulo, y = "", x = "" )+
    # Etiquetas
    ggplot2::annotate(x=max(bd$n)/2,y=ncat,label= grupos[1], geom="text", color="#64113F",hjust = -.5)+
    ggplot2::annotate(x=min(bd$n2)/2,y=ncat,label= grupos[2], geom="text", color="#DB9D47", hjust = .5)+
    # scale_y_continuous(limits=c(min(bd %>% pull({{pregunta}})),max(bd %>% pull({{pregunta}}))))+
    #Colores
    scale_fill_manual(values =c("#DB9D47", "#64113F"))+
    #Tema (parámetros estéticos)
    tema() + theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank())

  return(p)
}



#' Title
#'
#' @param bd base de datos en la que vienen las respuestas del cuestionario
#' @param pregunta Variable numérica a graficar
#' @param grupos grupo por el que se calificó
#' @param dominio rango entre el mínimo y máximo que permitia la pregunta
#' @param polos Nombre que va en cada uno de los extremos de la gráfica
#' @param colorPolos color  de los polos
#' @param titulo Título que aparecerá en la gráfica
#'
#' @return
#' @export
#'
#' @examples
graficar_segmento<- function(bd, pregunta, grupos, dominio=c(0,10),
                   polos=c("Polo 1", "Polo 2"),
                   colorPolos=c("#F78154","#78B874"),
                   titulo=""){
  # Eliminar NA
  a <- bd %>%
    select({{grupos}},{{pregunta}}) %>%
    na.omit()

  ngrupos <- a %>%
    pull({{grupos}}) %>%
    n_distinct()

  # Agrupar
  b <- a %>%
    group_by({{grupos}}) %>%
    summarise(ymin=quantile({{pregunta}}, c(0,.05,.25)),
              y=mean({{pregunta}}),
              ymax=quantile({{pregunta}}, c(1,.95,.75))) %>%
    arrange(desc(y)) %>%
    mutate(rw=row_number(), "{{grupos}}":=forcats::as_factor({{grupos}}))

  # Graficar
  res <- ggplot(b,aes(x=forcats::fct_reorder({{grupos}}, y))) +
    # Mediana
    #Bolita
    geom_point(aes(y=y))+
    # Ejes
    # scale_y_continuous(breaks = c(dominio[1], (dominio[2]-dominio[1])/2,dominio[2]),
    #                    # expand = expansion(add = c(2,2)),
    #                    labels = c(polos[1],(dominio[2]-dominio[1])/2,polos[2]))+
    # Etiquetas
    labs(title = titulo, y="") +

    # Fondo
    # Bajo
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=dominio[1],
             ymax=(dominio[2]-dominio[1])/5,
             fill=colorPolos[1], alpha=.6, geom="rect")+
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=(dominio[2]-dominio[1])/5,
             ymax=2*(dominio[2]-dominio[1])/5,
             fill=colorPolos[1], alpha=.4, geom="rect")+
    # Neutral
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=2*(dominio[2]-dominio[1])/5,
             ymax=3*(dominio[2]-dominio[1])/5,
             fill=colorPolos[1], alpha=.1, geom="rect")+
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=2*(dominio[2]-dominio[1])/5,
             ymax=3*(dominio[2]-dominio[1])/5,
             fill=colorPolos[2], alpha=.1, geom="rect")+
    # Alto
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=3*(dominio[2]-dominio[1])/5,
             ymax=4*(dominio[2]-dominio[1])/5,
             fill=colorPolos[2], alpha=.4, geom="rect")+
    annotate(xmin=0, xmax=ngrupos+.5,
             ymin=4*(dominio[2]-dominio[1])/5,
             ymax=dominio[2],
             fill=colorPolos[2], alpha=.6, geom="rect")+
    # Límites
    geom_segment(y=dominio[1], x=0, yend=dominio[1], xend= ngrupos+.5,
                 color=colorPolos[1], size=5)+
    geom_segment(y=dominio[2], x=0, yend=dominio[2], xend= ngrupos+.5,
                 color=colorPolos[2], size=5)+
    annotate(y=dominio[1], x=(ngrupos+.5)/2, label=polos[1], color="white",
             fontface="bold", geom="text", angle=90)+
    annotate(y=dominio[2], x=(ngrupos+.5)/2, label=polos[2], color="white",
             fontface="bold", geom="text", angle=270)+
    # Distribución
    geom_linerange(aes(ymin=ymin,
                       size=rw,
                       alpha=rw,
                       ymax=ymax)) +
    scale_alpha_continuous(range = c(.1,.8))+
    # Mediana
    #Bolita
    geom_point(aes(y=y))+
    # Número
    geom_text(aes(y=y, label=round(y, 1)),
              color="white", fontface="bold")+
    scale_color_gradient2(low = colorPolos[1],
                          high=colorPolos[2],
                          mid = "black",
                          midpoint = (dominio[2]-dominio[1])/2)+

    # Temas
    coord_flip()+
    tema_segmento()

  return(res)
}
