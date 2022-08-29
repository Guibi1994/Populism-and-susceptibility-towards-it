# Simulacion índice de estabilidad


library(dplyr)
library(ggplot2)
library(ggplot2)
set.seed(20)

i = 100
a0_raw <- data.frame(
  "Candidate.Idiology" = 1:i,
  "vote.share.t1" = sample(c(50:100/100),i, replace = T),
  "vote.share.t2" = sample(c(50:100/100),i, replace = T),
  "ideology.t1" = sample(c(100:900/100),i, replace = T),
  "ideology.t2" = sample(c(100:900/100),i, replace = T)) %>% 
  mutate(vote.dif = (vote.share.t2-vote.share.t1)/vote.share.t1,
         ideology.dif = (ideology.t2-ideology.t1))%>% 
  mutate(change.type =
           case_when(ideology.t1 > 5 & ideology.t2 > 5~"Stable right",
                     ideology.t1 < 5 & ideology.t2 < 5~"Stable left",
                     ideology.t1 < 5 & ideology.t2 > 5~"Flip to right",
                     ideology.t1 > 5 & ideology.t2 < 5~"Flip to left"))

a0_raw %>% 
  ggplot(aes(ideology.dif, vote.dif, color = change.type))+
  geom_point(, size = 3)+
  scale_color_manual(values = c("red","deepskyblue","firebrick4","deepskyblue4"))+
  geom_hline(yintercept = 0, lty = 2)+
  geom_vline(xintercept = 0, lty = 2)+
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))+
  labs(y = "Diference in vote share", x  = "Change in ideology",
       color = "")+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))



