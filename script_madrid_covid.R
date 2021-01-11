#________________________________________
# Lbraries ---------------------
#________________________________________

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(rstatix)
library(ggpubr)

#________________________________________
# Loading data ---------------------
#________________________________________


## The files in locale; for more information of sources see Bibliography
path_covid <- file.path("~", "Desktop", "covid19_tia_muni_y_distritos.csv") 


covid_madrid <- read.csv(path_covid,sep=";", encoding="utf-8") %>% 
  filter(municipio_distrito %like% "Madrid-") %>%  ##filter only for municipality = Madrid
  collect()
  
covid_madrid

## check we have the 21 districts in Madrid
#b <- distinct(covid_madrid, municipio_distrito) 
#view(b)


path_demo_pob <- file.path("~", "Desktop", "1_madrid_demo_pob.csv") 
madrid_demo_pob <- read.csv(path_demo_pob,sep=",") %>% 
  mutate(a = as.character(a)) %>% 
  collect()

madrid_demo_pob




## Merging and cleaning the dataset
final_data <- inner_join(covid_madrid,madrid_demo_pob, by = c("municipio_distrito" = 'a'), copy = TRUE) %>% 
  mutate(fecha = as.Date(fecha_informe,  format = "%d/%m/%y")) %>% 
  mutate(distrito = str_sub(municipio_distrito, start= 8)) %>%  # eliminate "Madrid-"
  mutate( tasa_incidencia_acumulada_ultimos_14dias = as.numeric(gsub(",", ".", tasa_incidencia_acumulada_ultimos_14dias))) %>%  ##change to numeric
  mutate( tasa_incidencia_acumulada_total = as.numeric(gsub(",", ".", tasa_incidencia_acumulada_total))) %>%  ##change to numeric
  mutate( hectarea = as.numeric(gsub(",", ".", hectarea))) %>%  ##change to numeric
  mutate( poblacion = as.numeric(gsub(",", ".", poblacion))) %>%  ##change to numeric
  mutate(densidad_poblacion = poblacion/hectarea) %>% 
  mutate(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = tasa_incidencia_acumulada_ultimos_14dias / (1/densidad_poblacion)) %>% 
##  mutate(densidad_poblacion_cuartil = within(final_data, quartile <- as.character(cut(densidad_poblacion, quantile(densidad_poblacion, probs=0:4/4), include.lowest=TRUE)))) %>% 
  select(distrito, fecha, casos_confirmados_ultimos_14dias, tasa_incidencia_acumulada_ultimos_14dias, 
         casos_confirmados_totales, tasa_incidencia_acumulada_total, hectarea, poblacion, 
         densidad_poblacion, tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion)
  

view(final_data)



#________________________________________
# Analisis ---------------------
#________________________________________



ggplot(final_data, aes(y = tasa_incidencia_acumulada_ultimos_14dias,  x = fecha)) +
  geom_line() + facet_wrap(~distrito) # Figura 1

summary(final_data)  #Figura 2
ggboxplot(final_data, y = "densidad_poblacion") +  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)


ggplot(final_data, aes(y = distrito, x = densidad_poblacion)) +
  geom_point(aes(size = poblacion))  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #Figura 3


ggplot(final_data, aes(y = tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion,  x = fecha)) +
  geom_line() + facet_wrap(~distrito) #Figura 4




descriptive <- final_data %>% 
  mutate(boolean_higher_median = ifelse(densidad_poblacion > 11.9955, "1", "0")) %>% 
  group_by(boolean_higher_median) %>% 
  summarise(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion)) 

descriptive #grafica 3


a_boolean <- final_data %>% 
     mutate(boolean_higher_median = ifelse(densidad_poblacion > 11.9955, "1", "0")) %>% 
     filter(boolean_higher_median == 1) %>% 
     group_by(fecha) %>% 
     summarise(sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion)) 

view(a_boolean)


ggplot(a_boolean, aes(y = sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion, x = fecha)) + geom_line() #Figura 5


b_boolean <- final_data %>% 
  mutate(boolean_higher_median = ifelse(densidad_poblacion > 11.9955, "1", "0")) %>% 
  filter(boolean_higher_median == 0) %>% 
  group_by(fecha) %>% 
  summarise(sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion))

view(b_boolean)


ggplot(b_boolean, aes(y = sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion, x = fecha)) + geom_line()   #Figura 6

 




descriptive <- final_data %>% 
  mutate(boolean_higher_median = ifelse(densidad_poblacion >= 0.2714, "1", "0")) %>% 
  group_by(boolean_higher_median) %>% 
  summarise(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion)) 


descriptive #grafica 4


a_boolean <- final_data %>% 
  mutate(boolean_higher_median = ifelse(densidad_poblacion >= 0.2714, "1", "0")) %>% 
  filter(boolean_higher_median == 1) %>% 
  group_by(fecha) %>% 
  summarise(sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion)) 

view(a_boolean)


ggplot(a_boolean, aes(y = sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion, x = fecha)) + geom_line() #Figura 7



b_boolean <- final_data %>% 
  mutate(boolean_higher_median = ifelse(densidad_poblacion >= 0.2714, "1", "0")) %>% 
  filter(boolean_higher_median == 0) %>% 
  group_by(fecha) %>% 
  summarise(sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion = mean(tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion))

view(b_boolean)


ggplot(b_boolean, aes(y = sum_tasa_incidencia_acumulada_ultimos_14dias_densidad_poblacion, x = fecha)) + geom_line()   #Figura 8

