# Afromexicanos distribuidos en el país
library(readxl)

# Inicializamos paquetes para el mapa

x = c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", 
      "tidyr", "tmap", "tmaptools","leaflet")
# install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages
mxgeo = read_shape("MEX_adm1.shp")

# Cambiando factors a caracteres
mxgeo@data$NAME_1 = as.character(mxgeo@data$NAME_1)
mxgeo@data$NAME_1[9] = "Ciudad de México"
estados = mxgeo@data$NAME_1

# Leyendo datos
pob_afro = read_excel("~/Proyectos/desarrolloeconomico/desarrollo/05_etnicidad.xls", 
                            sheet = "Hoja9", skip = 2)
pob_afro = data.frame(pob_afro)

BC = pob_afro[3,]
pob_afro[3,] = pob_afro[4,]
pob_afro[4,] = BC
CO = pob_afro[6:7,]
pob_afro[6:7,] = pob_afro[8:9,]
pob_afro[8:9,] = CO

pob_afro_e = pob_afro[-1,]
pob_afro_e$Entidad.federativa = estados

# SOn iguales?
identical(mxgeo@data$NAME_1, pob_afro_e$Entidad.federativa)

# Juntamos los datos
mxmap = append_data(mxgeo, pob_afro_e, 
                    key.shp = "NAME_1", key.data="Entidad.federativa")

pob_afro.map.porc = tm_shape(mxmap) +
  tm_fill("Se.considera", title = '',
          style = "kmeans",
          palette = "Blues",
          labels = c('0.01% - 0.17%',
                     '0.17% - 0.50%',
                     '0.50% - 1.13%',
                     '1.13% - 4.11%',
                     '4.11% - 6.50%')) +
  tm_borders(alpha=.8) +
  tm_text("NAME_1", size="AREA", col = "black")+
  tm_credits("Elaboración propia con datos de la encuesta intercensal 2015 del INEGI", align = "left",
             position = c("left","bottom"))+
  tm_layout(title="Población afromexicana \n (Porcentaje)",
            title.position = c("right", "top"), 
            legend.position = c(.73,.55))

pob_afro.map.porc
save_tmap(pob_afro.map.porc, filename = 'mapa_porc')
 
pob_afro.map.abs = tm_shape(mxmap) +
  tm_fill("Población.total", title='', 
          style = "kmeans",
          palette = "Blues",
          labels = c('.71 a 2.2 millones',
                     '2.2 a 4.2 millones',
                     '4.2 a 7 millones',
                     '7 a 12.6 millones',
                     '12.6 a 16.2 millones')) +
  tm_borders(alpha=.8) +
  tm_text("NAME_1", size="AREA", col = "black")+
  tm_credits("Elaboración propia con datos de la encuesta intercensal 2015 del INEGI", align = "left",
             position = c("left","bottom"))+
  tm_layout(title="Población afromexicana \n (Total)",
            title.position = c("right", "top"), 
            legend.position = c(.73,.55))
pob_afro.map.abs
save_tmap(pob_afro.map.abs, filename = 'mapa_abs')
