#Script para graficar el crecimiento del SNI por anio
#BioFreelancer
# Cargar librerias necesarias
library("dplyr")  #Para el data handling
library("ggplot2") #Para graficar
library("cowplot") #Mejorar graficas
library("ggsci") #Paquete de colores
library("scales") #Escalas


# Leer los datos
sni.df <- read.table(file = "resumen_sni_por_sexo_nivel_anio.csv",
                     header = T, sep = ",", stringsAsFactors = F)


# Creamos el grafico de lineas base

lineas_1.p <- sni.df %>% filter( nivel_de_investigador == "Candidato" ) %>%
  ggplot( mapping = aes( x = anio, 
                         y = miembros, 
                         color = titulo ) ) +
  geom_line()

# visualizamos
lineas_1.p


# Agregamos una capa de puntos, y ajustamos la escala Y de 0 a 11mil
lineas_2.p <- lineas_1.p +
  geom_point() +
  scale_y_continuous( limits = c(0, 11000) )

#visualizamos
lineas_2.p

# Usamos colores con mejor contraste, y agregamos titulo dinamico al plot
# usaremos la paleta d3 del paquete ggsci
lineas_3.p <- lineas_2.p +
    scale_color_d3() +
    ggtitle( paste0("Crecimiento del SNI \nNivel ", "Candidato") )

# visualizamos
lineas_3.p


# Utilizamos el tema cowplot para limpiar el grafico
# y eliminamos el titulo del eje X, usando la capa theme()
lineas_4.p <- lineas_3.p +
    theme_bw() +
    theme( axis.title.x = element_blank() )
  
#visualizamos
lineas_4.p


# Ahora hagamos un plot con la proporcion de Dr / Dra
# Hacer el plot de porcentajes
barras_1.p <- sni.df %>% 
  filter( nivel_de_investigador == "Candidato" ) %>%
  ggplot( aes( fill = titulo, y = proporcion, x = anio ) ) + 
  geom_bar( position = "stack", stat = "identity" ) # Especificamos que queremos las barra encimadas "stacked"
    # y que no queremos ninguna transformacion stadistica: stat = "identity"

#Visualizamos
barras_1.p


# Convertimos el eje Y a porcentajes (inicialmente es fraccion)
barras_2.p <- barras_1.p +
    scale_y_continuous( 
      breaks = seq(from = 0, to = 1, by = 0.1), # Podemos indicar cada cuanto queremos las marcas en el eje Y
      label = percent )  # percent es una transformacion del paquete "scales"

#Visualizamos
barras_2.p


# Ajustemos el eje X para que muestre todos los anios de 2015 a 2020
barras_3.p <- barras_2.p +
  scale_x_continuous( breaks = 2015:2020)

#Visualizamos
barras_3.p


# Usamos colores con mejor contraste para el FILL, y agregamos titulo dinamico al plot
# usaremos la paleta d3 del paquete ggsci
barras_4.p <- barras_3.p +
  scale_fill_d3() +
  ggtitle( paste0("Crecimiento del SNI \nNivel ", 
                  "Candidato") )

#Visualizamos
barras_4.p


#Utilizamos el tema cowplot para limpiar el grafico
# y eliminamos el titulo del eje X, usando la capa theme()
barras_5.p <- barras_4.p +
  theme_bw() +
  theme( axis.title.x = element_blank() )

#Visualizamos
barras_5.p


# Ahora si metemos ambos plots finales en un grid vertical ----
plot_grid( lineas_4.p, barras_5.p, ncol = 1 )


# Hagamoslo todo en una funcion!
graficadora.f <- function(la_data, el_nivel) {

  # generamos el plot de lineas  
  las_lineas.p <- la_data %>% filter( nivel_de_investigador == el_nivel ) %>%
    ggplot( mapping = aes( x = anio, y = miembros, color = titulo ) ) +
    geom_line() +
    geom_point() +
    scale_y_continuous( limits = c(0, 11000) ) +
    scale_color_d3() +
    ggtitle( paste0("Crecimiento del SNI \nNivel ", el_nivel) ) +
    theme_bw() +
    theme( axis.title.x = element_blank() )
  
  # generamos el plot de barras  
  las_barras.p <- la_data %>% filter( nivel_de_investigador == el_nivel ) %>%
    ggplot( aes( fill = titulo, y = proporcion, x = anio ) ) + 
    geom_bar( position = "stack", stat = "identity" ) +
    scale_y_continuous( breaks = seq(from = 0, to = 1, by = 0.1),
                        label = percent ) +
    scale_x_continuous( breaks = 2015:2020) +
    scale_fill_d3() +
    ggtitle( paste0("Crecimiento del SNI \nNivel ", el_nivel) ) +
    theme_bw() +
    theme( axis.title.x = element_blank() )

  #Juntamos los plots en una rejilla de columna
  plot_grid( las_lineas.p, las_barras.p, ncol = 1)
}


# probamos la funcion
graficadora.f( la_data = sni.df, el_nivel = "Candidato" )


# Generamos los 4 plots, 1 por cada nivel para comparacion
# Usemosla para generar graficos para cada nivel
candidato.p <- graficadora.f( la_data = sni.df, el_nivel = "Candidato" )

uno.p <- graficadora.f( la_data = sni.df, el_nivel = "1" )

dos.p <- graficadora.f( la_data = sni.df, el_nivel = "2" )

tres.p <- graficadora.f( la_data = sni.df, el_nivel = "3" )

# Juntamos todo en una rejilla
final.p <- plot_grid( candidato.p, uno.p, dos.p, tres.p, nrow = 1)

#visualizamos
final.p


#guardamos en png
ggsave( filename = "crecimiento_sni.png",
        plot = final.p,
        device = "png",
        height = 7, width = 14,
        units = "in")
