# Evolutions
library(dplyr)
library(ggplot2)


pr <- read.csv("00_some_data/02_political_flips_Colombia_2014_2018.csv")



table(pr$flip_2nd_14_18, pr$flip_2nd_18_22)



table(pr$flip_1st_14_18, pr$flip_1st_18_22) %>% as.data.frame() %>% 
  mutate(across(Var1:Var2, ~case_when(
    . == "flip to left"~"Cambio a la izquierda",
    . == "flip to right"~"Cambio a la derecha",
    . == "stable right"~"Derecha estable",
    T~"Izquierda estable")),
    Freq2 =paste0( (round(Freq/1122,3)*100)," %")) %>% 
  
  ggplot(aes(Var1, Var2, fill = Freq))+
  geom_tile()+
  scale_fill_gradient(low = "cyan4",high = "red")+
  theme_minimal()+
  labs(x = "Variación del 2014 al 2018", y = "Variación del 2018 al 2022")+
  geom_text(aes(label = Freq2), family = "serif", color = "white")+
  theme(text = element_text(family = "serif"),
        legend.position = "none")

ggsave("02_figures/03_flips_first_rounds.png",h=6,w=7)
  
table(pr$flip_2nd_14_18, pr$flip_2nd_18_22) %>% as.data.frame() %>% 
  mutate(across(Var1:Var2, ~case_when(
    . == "flip to left"~"Cambio a la izquierda",
    . == "flip to right"~"Cambio a la derecha",
    . == "stable right"~"Derecha estable",
    T~"Izquierda estable")),
    Freq2 =paste0( (round(Freq/1122,3)*100)," %")) %>% 
  
  ggplot(aes(Var1, Var2, fill = Freq))+
  geom_tile()+
  scale_fill_gradient(low = "cyan4",high = "red")+
  theme_minimal()+
  labs(x = "Variación del 2014 al 2018", y = "Variación del 2018 al 2022")+
  geom_text(aes(label = Freq2), family = "serif", color = "white")+
  theme(text = element_text(family = "serif"),
        legend.position = "none")

ggsave("02_figures/04_flips_second_rounds.png",h=6,w=7)
okas
