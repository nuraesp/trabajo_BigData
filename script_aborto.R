library("eurostat")
library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)
library(dplyr)
library(tidyr)
library(stringr)

#tabla eurostat :0
eurostat_aborto <- search_eurostat("abort", type = "all")
stat_aborto <- "demo_fabort"
label_eurostat_tables(stat_aborto)
df_aborto <- get_eurostat(stat_aborto, time_format = 'raw', keepFlags = TRUE)
rio::export(df_aborto, "./datasets/aborto_eurostat.csv")

#tabla europa
ruta_global_abort_ind <- "./datasets/Global Abortion Incidence Dataset (1).csv"
global_abortion <- rio::import(ruta_global_abort_ind)

global_abort_europa <- global_abortion %>% 
  select(country, yearstart, numberofabortions, region) %>%
  filter(region == "Europe and Northern America") %>% 
  filter(!(country %in% c("Canada", "Ukraine", "United States of America", 
                        "Russian Federation", "Romania"))) %>% 
  filter(!is.na(numberofabortions)) 

library(ggplot2)

p1 <- ggplot() +
  geom_line(data = global_abort_europa %>% group_by(yearstart) %>% slice_max(numberofabortions),
            aes(yearstart, numberofabortions), color = "black") +
  geom_line(data = global_abort_europa %>% group_by(yearstart) %>% slice_min(numberofabortions),
            aes(yearstart, numberofabortions), color = "black") +
  geom_line(data = global_abort_europa%>% filter(country %in% c("Spain", "Italy")),
            aes(x = yearstart, y = numberofabortions, color = country)) +
  labs(
    title = "Abortos en Europa entre 1990 y 2017", 
    x = "Año",
    y= "Abortos") +
  theme_minimal()


p2 <- p1 + 
  geom_label(data = global_abort_eu_uk %>% group_by(yearstart) %>% slice_max(numberofabortions) %>%
      filter(yearstart == max(yearstart)), aes(yearstart, numberofabortions, label = country),
      show.legend = FALSE, size = 3) +
  geom_label(data = global_abort_eu_uk %>% group_by(yearstart) %>% slice_min(numberofabortions) %>%
      filter(yearstart == max(yearstart)), aes(yearstart, numberofabortions, label = country),
      show.legend = FALSE, size = 3)

p2


#tabla mundo

global_abort_mundial <- global_abortion %>% 
  select(yearstart, numberofabortions, region) %>%
  filter(!is.na(numberofabortions)) 


global_abort_mundial <- global_abort_mundial %>%
  mutate(region = case_when(
    region %in% c("Australia and New Zealand", "Oceania (excluding Australia and New Zealand)") ~ "Oceanía",
    region %in% c("Central and Southern Asia", "East and Southeastern Asia") ~ "Asia",
    TRUE ~ region
  ))

abort_region_mundial <- global_abort_mundial %>% 
  group_by(region, yearstart) %>% 
  summarise(numberofabortions = sum(numberofabortions, na.rm = TRUE))




p3 <- ggplot() +
  geom_line(data = abort_region_mundial, aes(yearstart, numberofabortions, group = region, color= region))+
  labs(
    title = "Abortos en el mundo entre 1990 y 2017", 
    x = "Año",
    y= "Abortos") +
  theme_minimal()

p4 <- p3 + scale_y_continuous(breaks = c(0, 100000, 1000000, 10000000), labels = scales::comma)

p4 <- p3 +
  scale_y_log10(
    labels = scales::comma)
p4

p4 <- p3 +
  facet_wrap(~ region, scales = "free_y")
p4


#relacion abortos-pib

ruta_pibpc_ccaa <- "./datasets/pr_cre_pr.xlsx"
pibpc <- rio::import(ruta_pibpc_ccaa)

ruta_abortospccaa <- "./datasets/abortos_pccaa.csv"
abortospcca <- rio::import(ruta_abortospccaa)

names(abortospcca) <- abortospcca[1, ]
abortospcca <- abortospcca[-1, ]

pibpc_long <- pibpc %>% pivot_longer(cols = -"Comunidad Autónoma" ,        
                                     names_to = "year",        
                                     values_to = "valor"       
) 

abortospcca_long <- abortospcca %>% pivot_longer (cols = -"Comunidad Autónoma" ,        
names_to = "year",        
values_to = "valor"       
) 


abortospcca_long <- abortospcca_long %>% filter(!is.na(`Comunidad Autónoma`) & !is.na(year))
pibpc_long <- pibpc_long %>% filter(!is.na(`Comunidad Autónoma`) & !is.na(year))

abortospcca_long$`Comunidad Autónoma` <- str_to_lower(str_trim(abortospcca_long$`Comunidad Autónoma`))
pibpc_long$`Comunidad Autónoma` <- str_to_lower(str_trim(pibpc_long$`Comunidad Autónoma`))

abortospcca_long$year <- as.numeric(abortospcca_long$year)
pibpc_long$year <- as.numeric(pibpc_long$year)

abortospcca_long$abort_rate <- as.numeric(gsub(",", ".", abortospcca_long$abort_rate))
pibpc_long$gdp <- as.numeric(gsub(",", ".", pibpc_long$gdp))  # solo si tuvieran comas

abortosypibpc <- inner_join(abortospcca_long, pibpc_long, by = c("Comunidad Autónoma", "year"))


abortosypibpc$valor.x <- as.numeric(gsub(",", ".", abortosypibpc$valor.x))

library(dplyr)
library(dplyr)

cor_por_año <- abortosypibpc %>%
  group_by(year) %>%  
  summarise(correlacion = cor(valor.x, valor.y, use = "complete.obs")) %>%
  arrange(year)

cor_por_comunidad <- abortosypibpc %>%
  group_by(`Comunidad Autónoma`) %>%
  summarise(correlacion = cor(valor.x, valor.y, use = "complete.obs")) %>%
  arrange(desc(correlacion))

library(ggplot2)

ggplot(cor_por_comunidad, aes(x = reorder(`Comunidad Autónoma`, correlacion), y = correlacion)) +
  geom_col(fill = "plum") +
  coord_flip() +  
  labs(
    x = "Comunidad Autónoma",
    y = "Correlación PIB vs Tasa de abortos",
    title = "Correlación entre PIB y tasa de abortos por comunidad"
  ) +
  theme_classic()


#regresion 

regresion_pib_abortos <- lm(valor.x ~ valor.y, data = abortosypibpc)

# ver resultados
summary(regresion_pib_abortos)












