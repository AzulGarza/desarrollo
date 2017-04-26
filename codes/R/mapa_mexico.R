#Los paquetes a utilizar.
require(ggmap)
require(ggplot2)
require(maptools)
require(car)

mex.shp = "~/Proyectos/afromexicanos/afromexicanos/codes/MEX_adm/MEX_adm1.shp"

#Descarguemos el shapefile en R. La función readShapeSpatial viene en el paquete maptools 
#(busquen más información sobre este paquete en la documentación en línea al respecto).
estados.shape = readShapeSpatial(mex.shp)


#Convirtamos este shapefile a un data.frame de R para poder utilizarlo con ggmap, ggplot2 o crear más variables.
#La función para convertir este shapefile en un data.frame se llama fortify y viene en el paquete ggplot2.
estados.poly = fortify(estados.shape)

#Veamos que información tiene el data.frame que acabamos de crear.
#Vemos que tiene dos variables numéricas para longitud y latitud (long y lat respectivamente), order (variable de enteros que une los puntos para crear los polígonos), etc.
str(estados.poly)

# Mapa de México


# Cada argumento lo explico en el párrafo de aquí arriba.
imagen = get_map(location="Mexico", source="osm", color="color")

#Veamos el mapa con la función ggmap.
ggmap(imagen)


#Añado un 1 para ajustar el identificador a las claves del INEGI.
estados.poly$id2 = as.numeric(estados.poly$id) + 1

#Creo un factor para identificar qué partido gobierna qué estado (vean que utilizo la función recode para indicar qué id's, es decir, qué estados, gobierna cada partido.
#Por ejemplo, el PAN gobierna los estados 2, 3, 11, 21, 25, 26; así para los otros partidos.
estados.poly$id3 = estados.poly$id2
estados.poly$id3 = recode(estados.poly$id3, "c(2,3,11,21,25,26)='PAN'; c(1,4,6,7,8,10,13,14,15,16,18,19,22,23,24,28,29,30,31,32)='PRI'; c(9,12,17,20,27)='PRD'; 5='PVEM'")
estados.poly$id3 = as.factor(estados.poly$id3)

mapa = ggmap(imagen) + #Esto crea la imagen de México que ya vimos anteriormente.
  geom_polygon(data=estados.poly, aes(x=long, y=lat, group=group), colour="grey", fill="white") + #Crea el relieve de los estados con un contorno gris y un fondo blanco.
  geom_polygon(data=estados.poly, aes(x=long, y=lat, group=group, fill=id3), alpha=0.5) + #Creo un relleno de colores según mi factor id3 (que contiene el nombre de los partidos según el estado correspondiente).
  scale_fill_manual(values=c("#003399","#FFCC33","#CC3300","#009966"), name="Partido") + #Indico que quiero colores de manera manual: azul, amarillo, rojo y verde.
  labs(x="Longitud", y="Latitud") + #El nombre de los ejes.
  ggtitle("Mexico: gobiernos estatales \n (Julio 2013) \n") #El título

#Vemos el mapa:
mapa




x = c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", 
      "tidyr", "tmap", "tmaptools","leaflet")
# install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages
mxgeo = read_shape(mex.shp)
qtm(mxgeo)


# Cambiando factors a caracteres
mxgeo@data$NAME_1 = as.character(mxgeo@data$NAME_1)
estados = mxgeo@data$NAME_1


# Leyendo datos de población

library(readxl)
pob_data = "~/Proyectos/afromexicanos/afromexicanos/data/porce_pob.xls"
porce_pob = read_excel(pob_data, 
                       col_names = FALSE, skip = 5)
porce_pob = data.frame(porce_pob[1:32,])
BC = porce_pob[2,2]
porce_pob[2,2] = porce_pob[3,2]
porce_pob[3,2] = BC
CO = porce_pob[5:6,2]
porce_pob[5:6,2] = porce_pob[7:8,2]
porce_pob[7:8,2] = CO
porce_pob[,1] = estados
porce_pob[,'X3'] = 0
porce_pob$X2 = as.numeric(porce_pob$X2)
mean(porce_pob$X2)
for(i in 1:32){
  
  
}

# Son iguales los nombres?
identical(mxgeo@data$NAME_1,porce_pob$X1)

# Juntamos los datos
mxmap = append_data(mxgeo, porce_pob, 
                    key.shp = "NAME_1", key.data="X1")
qtm(mxmap, "X2")

mxtm = tm_shape(mxmap) +
  tm_fill("X2", title="Población afromexicana", style = "quantile",palette = "Blues",n=4) +
  tm_borders(alpha=.8) +
  tm_text("NAME_1", size="AREA", col = "black")+
  tm_credits("Elaboración propia con datos de la encuesta intercensal 2015 del INEGI", align = "left",
             position = c("left","bottom"))
  
mxtm
