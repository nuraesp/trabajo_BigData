library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

#tabla united nations - importacion de datos
united_nations_ruta <-"./datasets/united_nations.csv"
df <- rio::import(united_nations_ruta)


# condiciones por aborto y desopues aborto
df_aborto <- df %>%
  select(V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13)

colnames(df_aborto) <- c(
  "country",
  "country code",
  "life_risk",
  "physical_health",
  "rape_incest",
  "fetal_impairment",
  "medical_authorization",
  "judicial_consent_minors",
  "husband_consent",
  "criminal_charge",
  "post_abortion_law",
  "contradictory_legal_systems"
)

head(df_aborto)

# bases legales por las cuales el aborto está permitido en el país y conversion de "yes" y "no" respectivamente en 1 y 0 
#eliminacion de los NA

df_basi_legali_noNA <- df_aborto %>%
  select(country, life_risk, physical_health, rape_incest, fetal_impairment) %>%
  mutate(
    across(
      c(life_risk, physical_health, rape_incest, fetal_impairment),
      ~ case_when(
        . == "Yes" ~ 1,
        . == "No"  ~ 0,
        TRUE ~ NA_real_))) %>%
  drop_na(life_risk, physical_health, rape_incest, fetal_impairment)

# count yes por piases 

aa <- df_basi_legali_noNA %>%
  summarise(
    across(
      c(life_risk, physical_health, rape_incest, fetal_impairment),
      ~ sum(., na.rm = TRUE))
    )
aa

#  results =  life_risk physical_health rape_incest fetal_impairment
#        99              88          73               77

#cuantos paises tienen si en todas las variables 

paises_si <- df_basi_legali_noNA %>%
  filter(
    life_risk == 1,
    physical_health == 1,
    rape_incest == 1,
    fetal_impairment == 1
  )
paises_si$country

#grafico

baseleg_long <- df_aborto %>%
  select(country, life_risk, physical_health, rape_incest, fetal_impairment) %>%
  pivot_longer(
    cols      =  -country,
    names_to  = "si_por_variable",
    values_to = "value") %>%
  filter(value ==  "Yes") %>%       
  count(si_por_variable, name = "n_yes")

print(baseleg_long)


ggplot(baseleg_long, aes(x = si_por_variable, y = n_yes)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Países que autorizan el aborto en todas las bases legales",
    x = "Variable",
    y = "Número de países"
  ) +
  theme_minimal()

#segunda parte: RESTRICCIONES adicionales cuando el aborto es legal 

df_restriccion <- df_aborto %>%
  select(country, medical_authorization, judicial_consent_minors, husband_consent) %>%
  mutate(
    across(
      c(medical_authorization, judicial_consent_minors, husband_consent),
      ~ case_when(
        . == "Yes" ~ 1,
        . == "No"  ~ 0,
        TRUE ~ NA_real_))) %>%
  drop_na(medical_authorization, judicial_consent_minors, husband_consent)

restrictions_long <- df_aborto %>%
  select(country, medical_authorization, judicial_consent_minors, husband_consent) %>%
  pivot_longer(
    cols      = -country,
    names_to  = "restriction",
    values_to = "value"
  ) %>%
  filter(value == "Yes") %>%
  count(restriction, name = "n_yes")

print(restrictions_long)

ggplot(restrictions_long, aes(x = restriction, y = n_yes)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Paesi con restrizioni procedurali per l'aborto",
    x = "Tipo di restrizione",
    y = "Numero di paesi"
  )

#¿En cuántos países está penalizado el aborto? ¿Cuántos países cuentan con leyes sobre atención postaborto? ¿Y cómo se relaciona esta información con las bases legales?

df_crimen <- df_aborto %>%
  select(country, life_risk, physical_health, rape_incest, fetal_impairment,
         criminal_charge, post_abortion_law, contradictory_legal_systems)

# resumen de criminalización y leyes post-aborto
crimen_summary <- df_crimen %>%
  summarise(
    criminalized_yes      = sum(criminal_charge == "Yes", na.rm = TRUE),
    criminalized_no       = sum(criminal_charge == "No",  na.rm = TRUE),
    post_abortion_yes     = sum(post_abortion_law == "Yes", na.rm = TRUE),
    post_abortion_no      = sum(post_abortion_law == "No",  na.rm = TRUE),
    contradictions_yes    = sum(contradictory_legal_systems == "Yes", na.rm = TRUE),
    contradictions_no     = sum(contradictory_legal_systems == "No",  na.rm = TRUE)
  )

crimen_summary

#grafico por numero de criminalización y leyes post-aborto
crimen_long <- tibble(
  categoria = c("criminal_charge", "criminal_charge",
                "post_abortion_law", "post_abortion_law",
                "contradictory_legal_systems", "contradictory_legal_systems"),
  value     = c("Yes", "No", "Yes", "No", "Yes", "No"),
  n         = c(crimen_summary$criminalized_yes,
                crimen_summary$criminalized_no,
                crimen_summary$post_abortion_yes,
                crimen_summary$post_abortion_no,
                crimen_summary$contradictions_yes,
                crimen_summary$contradictions_no)
)

ggplot(crimen_long, aes(x = categoria, y = n, fill = value)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Criminalización del aborto y leyes post-aborto",
    x = "Categoría",
    y = "Número de países",
    fill = "Valor"
  ) +
  theme_minimal()



