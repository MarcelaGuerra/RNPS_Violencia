#Instalar paquetes
install.packages("tidyverse")

#Cargar paquetes
library(tidyverse) 
library(readxl)

#Fijar directorio de trabajo 

#Importar archivos
## Base 1: Proyecciones poblacionales a mitad de año - CONAPO
datos_pob<- read_csv("pob_mit_proyecciones.csv", locale = locale(encoding = "Latin1")) # Línea de código básico para importar el .csv
View(datos_pob) # Línea de código que permite visualizar la base de datos en otra pestaña

###Proyección de hombres y mujeres de 18 y más años de edad, por entidad federativa, en 2021.
datos_pob_clean_2021 <- datos_pob %>%
  filter(AÑO==2021 & EDAD>=18 & !ENTIDAD %in% "República Mexicana") %>%
  group_by(ENTIDAD) %>% 
  summarise(POBLACION = sum(POBLACION))

###Proyección de mujeres de 18 y más años de edad, por entidad federativa, en 2021.
datos_pob_clean_2021_m <- datos_pob %>%
  filter(AÑO==2021 & EDAD>=18 & SEXO=="Mujeres" & !ENTIDAD %in% "República Mexicana") %>%
  group_by(ENTIDAD) %>% 
  summarise(POBLACION_M = sum(POBLACION))

## Base 2: Registro Nacional de Personas Sancionadas en Materia de Violencia Política contra las Mujeres en Razón de Género - INE
datos_RNPS<- read_excel("Datos_RNPS_VPMRG.xlsx") # Línea de código básico para importar el .xlsx
View(datos_RNPS) # Línea de código que permite visualizar la base de datos en otra pestaña

###Personas sancionadas, por entidad federativa
datos_RNPS_clean_psan <- datos_RNPS %>% 
  distinct(NOMBRE, .keep_all = TRUE) %>%
  count(`ENTIDAD FEDERATIVA`)%>%
  rename(ENTIDAD="ENTIDAD FEDERATIVA") %>%
  mutate(ENTIDAD = recode(ENTIDAD, 'Estado de México' = 'México'))

###Expedientes sobre VPMRG, por entidad federativa
datos_RNPS_clean_exp <- datos_RNPS %>% 
  rename(EXPEDIENTE="NÚMERO DE EXPEDIENTE") %>%
  distinct(EXPEDIENTE, .keep_all = TRUE) %>%
  count(`ENTIDAD FEDERATIVA`)%>%
  rename(ENTIDAD="ENTIDAD FEDERATIVA")%>%
  mutate(ENTIDAD = recode(ENTIDAD, 'Estado de México' = 'México'))

## Unir bases para calcular tasas
### Unir proyecciones hombres y mujeres de 18 a más años de edad, por entidad federativa, en 2021; con base de personas sancionadas por entidad federativa.
datos_pp_ps<- left_join(datos_pob_clean_2021,datos_RNPS_clean_psan, by="ENTIDAD")
datos_pp_ps$tasa_ps<-(datos_pp_ps[,3]*100000)/datos_pp_ps[,2]
datos_pp_ps_desc<-datos_pp_ps%>%
  arrange(desc(tasa_ps))

### Unir proyecciones mujeres de 18 a más años de edad, por entidad federativa, en 2021; con base de expedientes sobre VPMRG, por entidad federativa.
datos_pm_ev<- left_join(datos_pob_clean_2021_m,datos_RNPS_clean_exp, by="ENTIDAD")
datos_pm_ev$tasa_ev<-(datos_pm_ev[,3]*100000)/datos_pm_ev[,2]
datos_pm_ev_desc<-datos_pm_ev%>%
  arrange(desc(tasa_ev))

### RNPS, expedientes por tipo de violencia
datos_RNPS_exp<- datos_RNPS %>% 
  rename(EXPEDIENTE="NÚMERO DE EXPEDIENTE") %>%
  distinct(EXPEDIENTE, .keep_all = TRUE) %>%
  select(`ENTIDAD FEDERATIVA`,EXPEDIENTE,verbal, simbólica,psicológica,física,patrimonial,económica,sexual,curso)

datos_RNPS_tipo_v<- read_csv("datos_RNPS_tipo_v.csv") 

ggplot(datos_RNPS_tipo_v, aes(x=`Tipo de violencia`, y=`Total de expedientes`)) +
  geom_bar(stat='identity', fill="tomato3", width = .9)+
  labs(title = "Tipo de violencia acreditada en expedientes por Violencia Política \ncontra las Mujeres en Razón de Género") +
  theme_minimal() +
  theme(panel.grid.major =element_blank(),
        #panel.grid.minor =element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=10,color="gray7", family = "Lora"),
        axis.title.x = element_text(margin = margin(0.5,0.5,0.5,0.5,"cm")),
        axis.title.y = element_text(margin = margin(0.5,0.5,0.5,0.5,"cm")),
        title = element_text(size=15,color="gray7", family = "Lora"))+
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70), limits = c(0,70))
