#Instalar paquetes
install.packages("rgdal")
install.packages("RColorBrewer")
install.packages("gpclib")
install.packages("maptools")
install.packages("showtext")

#Cargar paquetes
library(tidyverse) 
library(rgdal)
library(RColorBrewer)
library(gpclib)
library(maptools)
library(showtext)

font_add_google("Lora", family = "Lora")

## Carga de shapefiles de los estados para asignarlos a capa_estados
capa_estados <-readOGR("~/Desktop/Marcela/RNPS/code_mapas/shapemex", layer="areas_geoestadisticas_estatales")
class(capa_estados) 
capa_estados$NOM_ENT
capa_estados$CVE_ENT

#Primer mapa. Tasa de personas sancionadas por VPMRG, por cada 100 mil habitantes
## Importar base
psviolencia<- read.csv("datos_pp_ps.csv", colClasses = c(id = "character"),encoding = "Latin-1", header = T)
#coClasses evita que se eliminen los 0 al inicio de cada cifra  como 01, 02, 03, etc.
psviolencia<- psviolencia %>%
  mutate(CVE_ENT=as.factor(CVE_ENT))

gpclibPermitStatus()
gpclibPermit()

capa_estados_df <- fortify(capa_estados, region="CVE_ENT") 
#region="CVE_ENT", el agrupamiento de los polígonos. Coincide en nombre y contenido con mediaIM$id.    
capa_estados_df_guia <- inner_join(capa_estados_df, psviolencia, by="id") 
#Uno los valores relacionados a personas sancionadas con los polígonos por la variable clave: id. 
#Para ello debo mencionar primero el dataframe de los poligonos (capa_estados_df) y despues mencionar el dataframe de las las personas sancionadas (psviolencia)

## Mapa 1.
ggplot(psviolencia, aes(map_id = id)) + 
  geom_map(aes(fill = VAL), color="gray7", map = capa_estados_df, show.legend = FALSE) + 
  expand_limits(x = capa_estados_df$long, y = capa_estados_df$lat) +
  labs(title = "Tasa de personas sancionadas por Violencia Política contra las Mujeres en Razón de Género,\npor cada 100 mil habitantes mayores de edad")+
  scale_fill_continuous(low="white", high="seagreen4") +  #Cambio la escala contínua de colores.
  theme_void() + #Elimina escalas, marcas de coordenadas, etc.
  theme(panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=15,color="gray7",family = "Lora"))


#Segundo mapa. Tasa de expedientes por VPMRG, por cada 100 mil mujeres mayores de edad
## Importar base
e.violencia<- read.csv("datos_pm_ev.csv", colClasses = c(id = "character"),encoding = "Latin-1", header = T)
#coClasses evita que se eliminen los 0 al inicio de cada cifra  como 01, 02, 03, etc.
e.violencia<- e.violencia %>%
  mutate(CVE_ENT=as.factor(CVE_ENT))

#capa_estados_df_ev <- inner_join(capa_estados_df, e.violencia, by="id") 
#Uno los valores relacionados a personas sancionadas con los polígonos por la variable clave: id. 
#Para ello debo mencionar primero el dataframe de los poligonos (capa_estados_df) y despues mencionar el dataframe de las las personas sancionadas (psviolencia)

## Mapa 2.
ggplot(e.violencia, aes(map_id = id)) + 
  geom_map(aes(fill = VAL), color="gray7", map = capa_estados_df, show.legend = FALSE) + 
  expand_limits(x = capa_estados_df$long, y = capa_estados_df$lat) +
  labs(title = "Tasa de expedientes por Violencia Política contra las Mujeres en Razón de Género, \npor cada 100 mil mujeres mayores de edad") +
  scale_fill_continuous(low="white", high="darkorchid4") +  #Cambio la escala contínua de colores.
  theme_void() + #Elimina escalas, marcas de coordenadas, etc.
  theme(panel.grid.major =element_blank(),
        panel.grid.minor =element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size=15,color="gray7",family = "Lora"))
