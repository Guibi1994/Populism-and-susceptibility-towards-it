# Percecpción de igeologías de partidos
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(stringr)


# 1. Cargar ingormación 

a0_raw <- googlesheets4::sheets_read("https://docs.google.com/spreadsheets/d/1GQg_MAML0aS3M-5Wu59Cm1lp6Y_3Y5hgHTdBibQXv5A/edit?usp=sharing")


# 2. Seleccioanr información relevante

a1_partidos <- a0_raw %>% 
  select(Timestamp, id = `Cédula (opcional)`, gender = `Género (opcional)`,
         year_birht = `Año de nacimiento`,
         edu = `Nivel máximo de estudios alcanzados, o que curse actualmente (ocpcional)`,
         univ_exp = `Años de experiencia profesional en universidades y centros de pensamiento`,
         43:58) 

names(a1_partidos)[7:ncol(a1_partidos)] <- 
  names(a1_partidos)[7:ncol(a1_partidos)] %>% 
  str_extract(., pattern = "\\[(.*)\\]")

pr <-  a1_partidos %>%
  mutate(across(1:ncol(a1_partidos),~as.character(.))) %>% 
  reshape2::melt(id.vars = c("Timestamp", "id", "gender", "year_birht", "edu", "univ_exp"),
                 variable.name = "partido",value.name = "valor") %>% 
  mutate(partido =str_replace_all(partido, pattern = "\\[|\\]|\\(|\\)|\\d+",
                                  replacement = ""),
         valor = as.numeric(str_extract(valor, "\\d{1,}")),
         no_valuation = ifelse(is.na(valor),T,F))



pr %>% ggplot(aes(valor, reorder(partido, valor, mean, na.rm =T)), 
              color = valor)+
  geom_boxplot(outlier.shape = NA)+
  labs(title = "Distribución ideológica de partidos políticos en colmbia",
       subtitle = "n = 248", 
       x = "Escala ideológica", y = "Partidos")+
  stat_summary(fun.y = mean, geom = "point", color = "brown3")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, color ="cyan4"),
        plot.subtitle = element_text(hjust = 0.5))
