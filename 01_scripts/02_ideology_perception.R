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
  mutate(id_sourvey = row_number()) %>%
  
  ## 2.10. Transponer la matriz ----
  mutate(type = "party", estimation = "direct",party_or_candidate_code = "") %>% 
  reshape2::melt(id.vars = c("Timestamp","id_sourvey", "respondent_gender","respondent_birth_year",
                             "respondent_education_level", "respondent_years_academic_jobs",
                             "respondent_particip_local_elections","type","estimation",
                             "party_or_candidate_code"),
                 variable.name = "party_or_candidate_name",value.name = "idiological_value")  %>% 
  arrange(id_sourvey, idiological_value) %>% 
  
  ## 2.11. Cambiar nombres de partidos ----
  mutate(party_or_candidate_name = party_or_candidate_name %>% 
  str_extract(.,pattern = "\\[(.*)\\]") %>% 
  str_remove_all(., "\\(|\\)|\\[|\\]|\\d") %>% 
  str_remove(., "\\s$")) %>% 
  
  ## 2.12. Asiganr codigos por partido 
  group_by(party_or_candidate_name) %>% 
  mutate(party_or_candidate_code = 
           paste0("PP_",
           as.character(group_indices()) %>% 
           str_pad(string = ., pad = "0", width = 2)))



# 3. Exportar la base al github ----
write.csv(b1_partidos, "00_some_data/01_ideological_sourvey_colombia_2020.csv",
          row.names = F)













# 5. Matriz Ideológica Presedenciales -----

a2_presidenciales <- rbind(

## 5.1. 2006 elections ----

## 5.2. 2010 elections ----

### 5.2.1. Santos I ----

data.frame(
  "year" = 2010,
  "place" = "1st",
  "second.round" = T,
  "canidate_name" = "Juan Manuel Santos",
  "candidate_alias" = "Santos I",
  "candidate_code" = "CP001",
  "parties" = c("PP_13","PP_04")),

### 5.2.2. Antanas Mockus ----

data.frame(
  "year" = 2010,
  "place" = "2nd",
  "second.round" = T,
  "canidate_name" = "Antanas Mockus",
  "candidate_alias" = "Mockus",
  "candidate_code" = "CP002",
  "parties" = c("PP_01")),

### 5.2.3. German Vargas Llegar ----

data.frame(
  "year" = 2010,
  "place" = "3rd",
  "second.round" = F,
  "canidate_name" = "German Vargas Lleras",
  "candidate_alias" = "Vargas Llleras",
  "candidate_code" = "CP003",
  "parties" = c("PP_03")),

### 5.2.4. Gustavo Petro ----

data.frame(
  "year" = 2010,
  "place" = "4th",
  "second.round" = F,
  "canidate_name" = "Gustavo Petro",
  "candidate_alias" = "Petro",
  "candidate_code" = "CP004",
  "parties" = c("PP_16")),

### 5.2.5. Noemi Sanin ----

data.frame(
  "year" = 2010,
  "place" = "5th",
  "second.round" = F,
  "canidate_name" = "Noemi Sanin",
  "candidate_alias" = "Noemi",
  "candidate_code" = "CP005",
  "parties" = c("PP_12")),


## 5.3. 2014 elections ----

### 5.3.1. Santos II ----

data.frame(
  "year" = 2014,
  "place" = "1st",
  "second.round" = T,
  "canidate_name" = "Juan Manuel Santos",
  "candidate_alias" = "Santos II",
  "candidate_code" = "CP006",
  "parties" = c("PP_13", "PP_03", "PP_14")),

### 5.3.2. Oscar Ivan Zuluaga ----

data.frame(
  "year" = 2014,
  "place" = "2nd",
  "second.round" = T,
  "canidate_name" = "Oscar Iván Zuluaga",
  "candidate_alias" = "Zuluaga",
  "candidate_code" = "CP007",
  "parties" = c("PP_04")),

### 5.3.3. Marta Lucia Ramírez ----

data.frame(
  "year" = 2014,
  "place" = "3rd",
  "second.round" = F,
  "canidate_name" = "Marta Lucia Ramírez",
  "candidate_alias" = "ML Ramirez",
  "candidate_code" = "CP008",
  "parties" = c("PP_12")),

### 5.3.4. Clara Lópeza ----

data.frame(
  "year" = 2014,
  "place" = "4th",
  "second.round" = F,
  "canidate_name" = "Clara Lópeza",
  "candidate_alias" = "C. Lópeza",
  "candidate_code" = "CP009",
  "parties" = c("PP_16")),

## 5.4. 2018 elections ----

### 5.4.1. Iván Duque----

data.frame(
  "year" = 2018,
  "place" = "1st",
  "second.round" = T,
  "canidate_name" = "Iván Duque",
  "candidate_alias" = "Duque",
  "candidate_code" = "CP010",
  "parties" = c("PP_04","PP_06","PP_09")),

### 5.4.2. Gustavo Petro ----

data.frame(
  "year" = 2018,
  "place" = "2nd",
  "second.round" = T,
  "canidate_name" = "Gustavo Petro",
  "candidate_alias" = "Petro",
  "candidate_code" = "CP011",
  "parties" = c("PP_05","PP_15")),

### 5.4.3. Sergio Fajardo ----

data.frame(
  "year" = 2018,
  "place" = "3rd",
  "second.round" = F,
  "canidate_name" = "Sergio Fajardo",
  "candidate_alias" = "Fajardo",
  "candidate_code" = "CP012",
  "parties" = c("PP_01")),

### 5.4.4. German Vargas Lleras ----

data.frame(
  "year" = 2018,
  "place" = "4th",
  "second.round" = F,
  "canidate_name" = "German Vargas Lleras",
  "candidate_alias" = "Vargas Lleras",
  "candidate_code" = "CP013",
  "parties" = c("PP_03")),

## 5.5. 2022 elections ----

### 5.5.1. Gustavo Petro----

data.frame(
  "year" = 2022,
  "place" = "1st",
  "second.round" = T,
  "canidate_name" = "Gustavo Petro",
  "candidate_alias" = "Petro",
  "candidate_code" = "CP014",
  "parties" = c("PP_16","PP_15","PP_05","PP_11","PP_08")),

### 5.5.2. Rodolfo Hernández ----

data.frame(
  "year" = 2022,
  "place" = "2nd",
  "second.round" = T,
  "canidate_name" = "Rodolfo Hernández",
  "candidate_alias" = "R. Hernández",
  "candidate_code" = "CP015",
  "parties" = c("")),

### 5.5.3. Federico  Gutiérrez----

data.frame(
  "year" = 2022,
  "place" = "3rd",
  "second.round" = F,
  "canidate_name" = "Federico  Gutiérrez",
  "candidate_alias" = "Fico",
  "candidate_code" = "CP016",
  "parties" = c("PP_04", "PP_13","PP_12","PP_09")),

### 5.4.4. Sergio Fajardo ----

data.frame(
  "year" = 2022,
  "place" = "4th",
  "second.round" = F,
  "canidate_name" = "Sergio Fajardo",
  "candidate_alias" = "Fajardo",
  "candidate_code" = "CP017",
  "parties" = c("PP_01","PP_02"))) # %>% 
  
  ## 5.6.  Ediciones finales ----
  # merge((b1_partidos %>% group_by(
  #   parties = party_or_candidate_code,party_or_candidate_name) %>%  
  #     summarise() %>% as.data.frame()), by = "parties") %>% 
  # arrange(year, place)

 








# 6. Imputaciones ideológicas por candidato ----


b2_imputaciones <- 
  merge(a2_presidenciales, 
        (b1_partidos %>%  select(id_sourvey,parties = party_or_candidate_code, 
                                 idiological_value)),by = "parties") %>% 
  mutate(label = paste0(year, " (",place,")-", candidate_alias)) %>% 
  arrange(label)







 ##### 
{
  
# Comentarios sobre periodos
com1 <- grobTree(
  textGrob("-2022 elecciones-", x=0.85,  y=0.98, hjust=0.5,rot = 0,
           gp=gpar(col="grey30", fontsize=7, fontfamily = "serif")))  
com2 <- grobTree(
  textGrob("-2018 elecciones-", x=0.645,  y=0.98, hjust=0.5,rot = 0,
           gp=gpar(col="grey30", fontsize=7, fontfamily = "serif"))) 
com3 <- grobTree(
  textGrob("-2014 elecciones-", x=0.413,  y=0.98, hjust=0.5,rot = 0,
           gp=gpar(col="grey30", fontsize=7, fontfamily = "serif"))) 
com4 <- grobTree(
  textGrob("-2010 elecciones-", x=0.15,  y=0.98, hjust=0.5,rot = 0,
           gp=gpar(col="grey30", fontsize=7, fontfamily = "serif"))) 


# Comentarios sobre categorias ideológicas

com5 <- grobTree(
  textGrob("Dercha", x=0.96,  y=0.87, hjust=0.5,rot = 90,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic")))  
com6 <- grobTree(
  textGrob("Centro-Dercha", x=0.96,  y=0.62, hjust=0.5,rot = 90,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 
com7 <- grobTree(
  textGrob("Centro-izquierda", x=0.96,  y=0.385, hjust=0.5,rot = 90,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 
com8 <- grobTree(
  textGrob("Izquerda", x=0.96,  y=0.15, hjust=0.5,rot = 90,
           gp=gpar(col="cyan4", fontsize=7, fontfamily = "serif",
                   fontface = "italic"))) 

# Grafica final

b2_imputaciones %>%
  mutate(place2 = ifelse(place == "1st","Ganadores","Perdedores")) %>%
  ggplot(aes(label,idiological_value, color = place2))+
                  
 
  geom_boxplot(outlier.shape = NA, alpha =0.6)+
  annotate("rect",ymin=-2, ymax=10, xmin=-4, xmax=5.5,
           fill = "grey80", alpha = 0.5)+
  annotate("rect",ymin=-2, ymax=10, xmin=9.5, xmax=13.5,
            fill = "grey80", alpha = 0.5)+
  geom_boxplot(outlier.shape = NA, alpha =0.3)+
  scale_color_manual(values = c("brown2", "grey35"))+
  coord_cartesian(ylim = c(1,9), xlim = c(1,17))+
  
  stat_summary(fun.y = mean, geom = "point", size =2)+
  stat_summary(fun.data = n_fun, geom = "text", 
               family = "serif",size =2.5,color = "grey35")+
  
  
  geom_hline(yintercept = c(3,5,7), lty = 2, color ="cyan4", lwd =0.5)+
  
  labs(x ="", y = "Valor ideológico", color = "")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 9))+
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "top") +
  annotation_custom(com1)+annotation_custom(com2)+
  annotation_custom(com3)+annotation_custom(com4)+
  annotation_custom(com5)+annotation_custom(com6)+
  annotation_custom(com7)+annotation_custom(com8)

ggsave("02_figures/02_presential_ideolgy_evolution.png",h = 6,w = 8)
} # Varaciones ideológicas temporaless

# 
  # geom_hline(yintercept = c(3,5,7), lty = 2, color ="cyan4", lwd =0.5)+
  # stat_summary(fun.y = mean, geom = "point", size =2)+
  # 
  # theme_minimal()+
  # theme(text = element_text(family = "serif"),
  #       legend.position = "none")#
  # 
  
  
  
  
  geom_hline(yintercept = c(13.5), color = "grey65", lwd  =1)+
  
  stat_summary(fun.y = mean, geom = "point", size =2)+
  stat_summary(fun.data = n_fun, geom = "text", family = "serif",size =2.5,
               color = "grey35")+
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))+
  
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank())



geomre



n_fun <- function(x){
  return(data.frame(y = mean(x)+0.22, label = paste0((
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



  

b1_partidos %>% ggplot(aes(idiological_value))+
  geom_density(fill = "cyan3",color = "cyan4", alpha = 0.3)+
  geom_vline(xintercept = 
               c(mean(b1_partidos$idiological_value, na.rm = T), 
                 median(b1_partidos$idiological_value, na.rm = T)),
             lty =c(1,2), color = "brown3")+
  labs(title = "Distribution of the idological valuation across parties",
       x = "Ideological value")+
  theme_minimal()+coord_cartesian(ylim = c(0.07,0.16))+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5))
ggsave("02_figures/pruenas2.png")  
  

