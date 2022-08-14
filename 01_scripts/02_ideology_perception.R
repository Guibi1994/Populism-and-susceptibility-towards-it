# Percecpción de igeologías de partidos
library(dplyr)
library(googlesheets4)
library(ggplot2)
library(stringr)
library(grid)

# 1. Cargar ingormación 

a1_encuesta <- googlesheets4::sheets_read("https://docs.google.com/spreadsheets/d/1GQg_MAML0aS3M-5Wu59Cm1lp6Y_3Y5hgHTdBibQXv5A/edit?usp=sharing")
a2_

# 2. Seleccioanr información relevante

b1partidos <- a1_encuesta %>% 
  ## 2.1. Selecionar varibles relevantes
  select(Timestamp, 
         respondent_gender = `Género (opcional)`,
         respondent_year_birth = `Año de nacimiento`,
         respondent_education_level =  `Nivel máximo de estudios alcanzados, o que curse actualmente (ocpcional)`,
         respondent_years_academic_jobs = `Años de experiencia profesional en universidades y centros de pensamiento`,
         respondent_particip_local_elections = `Seleccione las elecciones para alcalde de Bogotá, en las que ha participado como votante`,
         43:58) %>%
  ##
  mutate(across(1:22,~as.character(.))) %>% 
  mutate(respondent_years_academic_jobs = as.numeric(respondent_years_academic_jobs),
         respondent_particip_local_elections =
           str_count(respondent_particip_local_elections, "Gana"),
         collapse = NULL)

names(a1_partidos)[7:ncol(a1_partidos)] <- 
  names(a1_partidos)[7:ncol(a1_partidos)] %>% 
  str_extract(., pattern = "\\[(.*)\\]")




pr <-  a1_partidos %>%
  
  reshape2::melt(id.vars = c("Timestamp", "id", "gender", "year_birht", "edu", "univ_exp", "particip"),
                 variable.name = "partido",value.name = "valor") %>% 
  mutate(partido =str_replace_all(partido, pattern = "\\[|\\]|\\(|\\)|\\d+",
                                  replacement = ""),
         valor = as.numeric(str_extract(valor, "\\d{1,}")),
         no_valuation = ifelse(is.na(valor),T,F),
         univ_exp = as.numeric(univ_exp)) %>% 
  # Categorias de participación
  mutate(cat_part = case_when(
    particip == 0~"0 eleciones",
    particip %in% 1:2~"de 1 a 2 eleciones",
    T~"mas de 3 elecciones"))




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



  