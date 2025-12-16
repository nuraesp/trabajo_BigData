library(tidyverse)

library(eurostat)
library(dplyr)

my_table <- "demo_fabort" #tabla seleccionada

label_eurostat_tables(my_table)     #- gives information about the table
str(my_table) 

#DESCARGAR TABLA  
df <- get_eurostat(my_table, time_format = 'raw', keepFlags = TRUE)
str(df)

#LABELS O DESCRIPTRES DE LAS VARIABLES
df_names <- names(df)
df <- label_eurostat(df, code = df_names, fix_duplicated = TRUE)
rm(df_names, my_table)

#FILTRAMOS DESDE LEGALIDAD DE ABORTO
#FILTRAMOS DATOS DE PAIS DE INTERES SPAIN
df %>%
  distinct(geo)

df %>% 
  count(geo == "Spain")

#
df <- df %>% 
  mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%  #- pasamos date (q es character) a numeric
  filter(TIME_PERIOD >= 1985)
df

#ABORTOS LEGALMENTE INDUCIDOS EN ESPAÑA DESPUES DE "LEGALIZARSE" 1985 HASTA 2010----
df_spain_85_10 <- df %>%
  filter(
    geo == "Spain",
    between(TIME_PERIOD, 1985, 2010)
  ) 

aa<- df_spain_85_10 %>% filter(age == "Total")
aa

#GRAFICA  
aa<- df_spain_85_10 %>% filter(age == "Total")

g1 <- ggplot(aa,aes(x=TIME_PERIOD, y=values))+
  geom_line(color="plum",linewidth=1) + 
  geom_point(color="navyblue",size= 3)+
  labs(title = "Evolución del aborto en España  desde 1985 y 2010",
       x="Años",
       y="Abortos"
  )

print(g1)     


#GRAFICAS RESPECTO A VALORES TOTALES VALORES DECONOCIDOS

#ABORTOS DESPUES DE INTRODUCIRSE LEY SALUD SEXUAL Y REPRODUCTIVA 2010 HASTA 2023 EN ESPAÑA----
df_spain_2010 <- df %>%
  filter(geo=="Spain",TIME_PERIOD>=2010)

#grafica
bb <-df_spain_2010 %>% filter(age == "Total") 

g2 <- ggplot(bb,aes(x=TIME_PERIOD, y=values))+
  geom_line(color="plum",linewidth=1) + 
  geom_point(color="navyblue",size= 2)+
  labs(title = "Evolución del aborto en España desde 2010 hasta 2023",
       x="Años",
       y="Abortos"
  )

print(g2)     


#TASAS DE ABORTO MAYOR MENOR Y MEDIA DESDE 1985 HASTA 2023 EN SPAIN + TOTAL---

df_spain_85_23 <- df %>%
  filter(
    geo == "Spain",
    between(TIME_PERIOD, 1985, 2023)
  )

df_summary <- df_spain_85_23 %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    max_ev = max(values, na.rm = TRUE),   #ABORTO MAXIMO
    mean_ev = mean(values, na.rm = TRUE), #ABORTO MEDIO
    min_ev = min(values, na.rm = TRUE),   #ABORTO MEDIO
    .groups = 'drop'
  )
gt::gt(df_summary)

#TABLA
library(dplyr)
library(gt)
install.packages("gtExtras")
library(gtExtras)

tabla_gt1 <- df_summary %>%
  gt() %>%
  tab_header(
    title = "Resumen de Abortos en España (1985-2023)"
  ) %>%
  cols_label(
    TIME_PERIOD = "Año",
    min_ev = "N.ºAbortos Mínimo",
    mean_ev = "N.ºAbortos Medio",
    max_ev = "N.ºAbortos Máximo",
  ) %>%
  data_color(
    columns = c(max_ev, min_ev),  
    colors = scales::col_numeric(
      palette = c("gray", "plum"),
      domain = NULL
    )
  ) 

print(tabla_gt1)



#CCAA DATOS DE MINISTERIO
  
#READ.CSV.2
  
ruta_ccaa <- "./datasets/abortos_pccaa.csv"
datos_aborto_pccaa <- rio::import(ruta_ccaa)

datos_aborto_limpio <- datos_aborto_pccaa %>%
  rename_with(
      .fn = ~str_remove(., "X"), 
      .cols = starts_with("X20") 
    )
datos_aborto_limpio <- import(
  "./datasets/abortos_pccaa.csv", 
  setclass = "tibble",
  header = TRUE
)
  
#CONVERTIR TABLA A FORMATO LARGO
  datos_long <- datos_aborto_limpio %>%
    pivot_longer(
      cols = c(`2015`:`2024`),  # Selecciona las columnas de años (asegúrate de que los nombres coincidan)
      names_to = "Year",         # La nueva columna de nombres será 'Year'
      values_to = "Tasa"         # La nueva columna de valores será 'Tasa'
    )
  
# Convertir el nuevo 'Year' a tipo numérico o entero
  datos_long$Year <- as.integer(datos_long$Year)
  
  
#DEFINIR YEAR COMO VARIABLE NUMERICA
  datos_long <- datos_long %>%
    mutate(Year = as.integer(Year))
  
  unique(mapa_ccaa$NAME_LATN)  
  
#top 5 CCAA CON MAS ABORTOS 2024----
  
  top5_mas_abortos <- datos_long %>% 
    filter(Year == 2024) %>%
    slice_max(Tasa, n = 5) 
  
top5_mas_abortos

#TOP 5 MENOS ABORTOS 2024----
top5_menos_abortos <- datos_long %>% 
  filter(Year == 2024) %>%
  slice_min(Tasa, n = 5) 

top5_menos_abortos

str(datos_long)
#CCAA QUE HAN EXPERIEMENTADO MAYOR TASA DE CRECIMIENTO ENTRE 2015 Y 2024----
crecimiento_abortos <- datos_long %>%
  filter(Comunidad.Autónoma != "Total") %>%
  group_by(Comunidad.Autónoma) %>%
  #CALCULO
  mutate(
    # Crecimiento Interanual (Tasa de este año - Tasa del año anterior)
    crecimiento_anual = Tasa - lag(Tasa)# lag() busca el valor de la fila anterior dentro del grupo actual.
  ) %>%
  summarise(
    Tasa_Inicial_2015 = first(Tasa), # Tasa en el año inicial (2015)
    Tasa_Final_2024 = last(Tasa),   # Tasa en el año final (2024)
    Crecimiento_Total = Tasa_Final_2024 - Tasa_Inicial_2015,
    .groups = 'drop' 
  ) %>%
  
  slice_max(Crecimiento_Total, n = 10) %>%
  
  arrange(desc(Crecimiento_Total))

print(crecimiento_abortos)

#COROPLETA --------
library(sf)
library(giscoR)


datos_long <- datos_long %>%
  rename(Comunidad = `Comunidad.Autónoma`)
str(datos_long)


unique(mapa_ccaa$NAME_LATN)

datos_2024_completo <- datos_long %>% #FILTRO POR YEAR Y EXCLUYO "TOTAL"
  filter(Year == 2024) %>%
  filter(Comunidad != "Total") %>% # Seguimos excluyendo el Total

  mutate(
    CCAA_Limpia = case_when(

      Comunidad == "Asturias, Principado de" ~ "Principado de Asturias",
      Comunidad == "Baleares, Illes" ~ "Illes Balears",
      Comunidad == "C. Valenciana" ~ "Comunidad Valenciana",
      Comunidad == "Madrid, Comunidad de" ~ "Comunidad de Madrid",
      Comunidad == "Murcia, Región de" ~ "Región de Murcia",
      Comunidad == "Navarra, Comunidad Foral de" ~ "Comunidad Foral de Navarra",
      Comunidad == "Castilla - La Mancha" ~ "Castilla-La Mancha", 
      Comunidad == "Ceuta y Melilla, Ciudades Autónomas" ~ "Ciudad Autónoma de Ceuta",
      
      TRUE ~ Comunidad  ))


datos_2024_completo <- bind_rows(
  datos_2024_completo,
  datos_2024_completo %>%
    filter(CCAA_Limpia == "Ciudad Autónoma de Ceuta") %>%
    mutate(CCAA_Limpia = "Ciudad Autónoma de Melilla")
)

anti_join(
  datos_2024_completo,
  mapa_ccaa,
  by = c("CCAA_Limpia" = "NAME_LATN")
)


datos_2024_completo <- datos_2024_completo %>% #DEFINIR UMBRALES DE COLOR
  mutate(Categoria = cut(Tasa,
                    breaks = c(-Inf, 9.0, 13.0, Inf), # Umbrales
                    labels = c("Baja (Gris)", "Media (Violeta Claro)", "Alta (Violeta)"),
                    right = FALSE ))

#DESCARGAR Y UNIR DATOS CON NUTS 
mapa_ccaa <- gisco_get_nuts(
  country = "ES",         
  nuts_level = "2",       
  resolution = "3")
mapa_final <- mapa_ccaa %>% #AJUSTAR NOMBRES
  mutate(
    CCAA_Limpia = case_when(
      NAME_LATN == "Ceuta" ~ "Ceuta y Melilla",
      NAME_LATN == "Melilla" ~ "Ceuta y Melilla",
      TRUE ~ NAME_LATN # Mantenemos el resto
      )
  ) %>%
  
  left_join(datos_2024_completo, by = "CCAA_Limpia")
  
  
  
paleta_colores <- c("Baja (Gris)" = "lightgrey", 
                    "Media (Violeta Claro)" = "#E6E6FA", 
                    "Alta (Violeta)" = "plum")

mapa_coropleta <- ggplot(mapa_final) +
  geom_sf(
    aes(fill = Categoria),
    color = "grey60", 
    linewidth = 0.2
  ) +
  scale_fill_manual( #APLICO COLORES
    values = paleta_colores,
    name = "Tasa de Abortos (por 1.000) - 2024"
  ) +
  geom_sf_text( #ETIQUETAS CCAA
    aes(label = CCAA_Limpia),
    size = 2,
    fontface="bold",
    check_overlap = TRUE
  ) +
#AJUSTES
  labs(
    title = "Tasa de Abortos por CCAA (2024)",
    caption = "Clasificación: Verde < 9.0, Amarillo < 13.0, Rojo ≥ 13.0",
 fill="categoria de tasa"
 ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "left",
    legend.title = element_text(face = "bold.italic"),  
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust= 0.5, face="bold.italic",size= 16)
     
  )

print(mapa_coropleta)


#ABORTOS PRECOVID 2019 POST COVID 2022 TABLA-----

datos_long<- datos_long%>%
  filter(Comunidad != "Total")

datos_long <- datos_long %>%
  mutate(
    Bloque = case_when(
      Year <= 2019 ~ "Pre-COVID",
      Year %in% 2020:2021 ~ "COVID",
      Year >= 2022 ~ "Post-COVID"
    )
  )

comparacion <- datos_long %>%
  group_by(Bloque) %>%
  summarise(
    Mayor_Tasa_CCAA = Comunidad[which.max(Tasa)],
    Mayor_Tasa = max(Tasa),
    Menor_Tasa_CCAA = Comunidad[which.min(Tasa)],
    Menor_Tasa = min(Tasa)
  ) %>%
  ungroup()
#TABLA 
library(gt)

tabla_gt2 <- comparacion %>%
  gt() %>%
  tab_header(
    title = "CCAA con la Mayor y Menor Tasa de Abortos por Bloques de Años: Pre-COVID, COVID y Post-COVID"
  ) %>%
  cols_label(
    Bloque = "",
    Mayor_Tasa_CCAA = "CCAA Mayor Tasa",
    Mayor_Tasa = "Mayor Tasa",
    Menor_Tasa_CCAA = "CCAA Menor Tasa",
    Menor_Tasa = "Menor Tasa"
  ) %>%
  tab_source_note(
    source_note = "Las tasas están expresadas por 1.000 habitantes de cada CCAA."
  )  %>%
  tab_style(
    style = list(
      cell_text(size = px(10))  #AJUSTAR SIZE
    ),
    locations = cells_source_notes()
  )

print(tabla_gt)

tabla_gt%>% gtExtras::gt_theme_pff()

names(datos_aborto_limpio)






