# Percecpción de igeologías de partidos
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(stringr)
library(grid)




# 1. Descaragar encuesta  ----

a1_encuesta <- googlesheets4::sheets_read("https://docs.google.com/spreadsheets/d/1GQg_MAML0aS3M-5Wu59Cm1lp6Y_3Y5hgHTdBibQXv5A/edit?usp=sharing")


# 2. Edicion general de la base de respuestas ----

b1_partidos <- a1_encuesta %>% 
  ## 2.1. Crear ID por respuesta ----
  mutate(id_sourvey = row_number()) %>% 
  ## 2.2. Selecionar varibles relevantes ----
  select(Timestamp, id_sourvey,id_per = `Cédula (opcional)`,
         respondent_gender = `Género (opcional)`,
         respondent_birth_year = `Año de nacimiento`,
         respondent_education_level =  `Nivel máximo de estudios alcanzados, o que curse actualmente (ocpcional)`,
         respondent_years_academic_jobs = `Años de experiencia profesional en universidades y centros de pensamiento`,
         respondent_particip_local_elections = `Seleccione las elecciones para alcalde de Bogotá, en las que ha participado como votante`,
         43:58) %>%
  mutate(
    ## 2.3. Desanidar respuestas compuestas ----
    across(1:ncol(.),~as.character(.)), 
    ## 2.4. Cambiar variables a formatos numericos ----
    respondent_years_academic_jobs = as.integer(respondent_years_academic_jobs),
    respondent_birth_year = as.integer(respondent_birth_year),
    ## 2.5. Numero de participaciones en elecciones locales ----
    respondent_particip_local_elections =
           str_count(respondent_particip_local_elections, "Gana"),
         collapse = NULL,
    ## 2.6. Identificación de ID's repetidos ----
    id_per = ifelse(is.na(id_per),id_sourvey, id_per)) %>% 
  group_by(id_per) %>% mutate(order = row_number()) %>% as.data.frame() %>% 
  filter(order == 1) %>% arrange(Timestamp) %>% select(-id_per,-order) %>% 
  ## 2.7. Valores ideológicos
  mutate(across(8:23, ~as.numeric(str_extract_all(.,"\\d+"))),
         total = rowSums(across(8:23), na.rm = T)) %>% 
  ## 2.8. Filtrar sin respuestas e indexar por respuesta ----
  filter(total != 0, respondent_birth_year > 1920) %>% select(-total) %>% 
  mutate(id_sourvey = row_number())

## 2.9. Renombrar varaibles de partidos ----
names(b1_partidos)[8:ncol(b1_partidos)] <- 
  names(b1_partidos)[8:ncol(b1_partidos)] %>% 
  str_extract(.,pattern = "\\[(.*)\\]") %>% 
  str_remove_all(., "\\(|\\)|\\[|\\]|\\d") %>% 
  str_remove(., "\\s$")


# 4. Transposición de la matriz ideológica ----
b1_partidos <-  b1_partidos %>%
  reshape2::melt(id.vars = c("Timestamp","id_sourvey", "respondent_gender","respondent_birth_year",
                             "respondent_education_level", "respondent_years_academic_jobs",
                             "respondent_particip_local_elections"),
                 variable.name = "party",value.name = "idiological_value")  %>% 
  arrange(id_sourvey, idiological_value)


# 3. Exportar la base al github ----
write.csv(b1_partidos, "00_some_data/01_ideological_sourvey_colombia_2020.csv",
          row.names = F)



n_fun <- function(x){
  return(data.frame(y = mean(x)+0.3, label = paste0((
    round(mean(x, na.rm = T),1)))))
}



com1 <- grobTree(
  textGrob("Dercha", x=0.85,  y=0.97, hjust=0.5,rot = 0,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic")))  
com2 <- grobTree(
  textGrob("Centro-Dercha", x=0.61,  y=0.97, hjust=0.5,rot = 0,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 
com3 <- grobTree(
  textGrob("Centro-izquierda", x=0.39,  y=0.97, hjust=0.5,rot = 0,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 
com4 <- grobTree(
  textGrob("Izquerda", x=0.15,  y=0.97, hjust=0.5,rot = 0,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 


rbind(pr,
      pr[1,] %>% mutate(valor = NA,partido = " ")) %>%
    ggplot(aes(valor, reorder(partido, valor, mean, na.rm =T)))+
  
  geom_boxplot(outlier.shape = NA, lwd =0.45, color = "grey40")+
  labs(#title = "Distribución ideológica de partidos políticos en colmbia (n = 248)",
       x = "Escala ideológica", y = "Partidos",
       color = "Experiencia: ")+
  stat_summary(fun.y = mean, geom = "point", color = "brown3", size =3)+
  
  stat_summary(fun.data = n_fun, geom = "text", family = "serif",size =2.5)+
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_vline(xintercept = c(3,5,7), lty = 2, color ="cyan4", lwd =0.5)+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, color ="cyan4"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  annotation_custom(com1)+annotation_custom(com2)+
  annotation_custom(com3)+annotation_custom(com4)+


ggsave("02_figures/01_idiology_perceptions.png",h=6,w=7)



  