#A.Paulina Perez-Gonzalez
#paulinapglz.99@gmail.com


###############################
#SCRIPT PARA BioFreelancer#
###############################

#Este script genera un grafico tipo donut con datos basicos de 1000 Genomas
#https://www.kaggle.com/datasets/daiearth22/1000-genome-data


#Las librerias 

library("pacman")

p_load("vroom", 
       "dplyr", 
       "ggplot2", 
       "RColorBrewer")

#Visualizacion de datos

genomes.df <- vroom(file = "1000genomesinfo.csv")

#Quuiero hacer una cuenta de cuantas muestras por pais hay
#para ello

counts_per_country <- genomes.df %>%   #Primero llamo a los datos. Uso el símbolo %>% para conectar las operaciones
  group_by(Population) %>%   #Luego indico que quiero un grupo por cada valor único en
  tally() #tally cuenta cuantos elementos hay en cada grupo

#Vamos a graficar un barplot sencillo

genomes.p <- ggplot(counts_per_country, 
                    aes(x = Population, 
                        y = n)) +
  geom_col(position = "stack", 
           fill = "pink", 
           color = "violet")
#Vis

genomes.p

#Vamos a darle formato a este plot

genomes_1.p <- genomes.p +
  coord_polar()

#Vis

genomes_1.p

#Plot 2

genomes_2.p <- genomes_1.p +
geom_text(aes(label= n),
          check_overlap = T)

#Vis

genomes_2.p

#Plot3
genomes_3.p <-  genomes_2.p+
  theme_minimal() +
theme(axis.title.y = element_blank())

#Vis

genomes_3.p

#Plot4 

genomes_4.p <- genomes_3.p + 
  labs(title="Número de genomas por población", 
       subtitle="Según el estudio 1000 Genomes", 
       caption="source: kaggle.com") + 
  xlab("Poblaciones")
  

#Vis

genomes_4.p


# Guardamos el plot
ggsave( filename = "genomes_per_population.png",  # el nombre del archivo de salida
        plot = genomes_4.p,           # guardamos el ultimo grafico que hicimos
        width = 8,                # ancho de 8 pulgadas
        height = 9,               # alto 7 de pulgadas
        dpi = 600 )               # resolucion de 600 puntos por pulgada

# FIN DE EJERCICIO