## Setup

library(tidyverse)
library(sf)

options(scipen = 999999)

## Importação da amostra

viagens <- st_read("input", "viagens")

viagens <- viagens %>% 
  select(id, time_acum, spd_kmh, dist)

st_geometry(viagens) <- NULL

## Cálculos

viagens_selecionadas <- viagens %>% 
  group_by(id) %>% 
  summarise(distancia = sum(dist)) %>% 
  filter(distancia >= 3000 & distancia <= 7000) %>% 
  pull(id)

viagens_vel <- viagens %>% 
  filter(id %in% as.vector(viagens_selecionadas)) %>% 
  mutate(tempo = case_when(
         id == lag(id) ~ time_acum - lag(time_acum),
         TRUE ~ 0))

velocidade <- viagens_vel %>%
  drop_na(dist) %>% 
  mutate(vel_ponderada = spd_kmh * dist) %>%
  group_by(id) %>%
  summarise(vel_ponderada = sum(vel_ponderada),
            distancia = sum(dist),
            tempo_min = sum(tempo) / 60) %>%
  mutate(vel_media = vel_ponderada / distancia)

vel_reg <- lm(tempo_min ~ vel_media, data = velocidade)
summary(vel_reg)

## Valores entre 30 km/h e 70 km/h
vel_pred <- seq(30, 70, 1)

tempo_var <- predict(vel_reg, newdata = data.frame(vel_media = vel_pred))

resultados <- tibble(velocidade = vel_pred,
                     tempo_min = tempo_var)

## Velocidade de impacto
vel_imp <- c(5, 15, 25, 35, 45, 55, 65, 75, 85)

## Chance de morte
chance_obito <- c(0, 0.004, 0.026, 0.139, 0.292, 0.309, 0.156, 0.062, 0.012)

## Chance acumulada
acumulado_obito <- cumsum(chance_obito)

## Uniao em tabela
ash <- tibble(vel_imp = vel_imp,
              chance = acumulado_obito)

## Regressão logística
ash_reg <- glm(chance ~ vel_imp, family = "binomial", data = ash)
summary(ash_reg)

obito_var <- predict(ash_reg, newdata = data.frame(vel_imp = vel_pred), 
                     type = "response")

## Resultados

resultados <- tibble(velocidade = vel_pred,
                     tempo = tempo_var,
                     risco = obito_var)


p1 <- resultados %>% 
  ggplot(aes(x = velocidade, y = risco, color = tempo)) +
  geom_point() +
  scale_color_viridis_c() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0,1,0.2), 
                     labels = scales::percent) +
  scale_x_continuous(minor_breaks = NULL) +
  labs(
    x = "Velocidade (km/h)",
    y = "Risco de óbito",
    color = "Tempo de\ndeslocamento\n(minutos):"
  )

ggsave("plot_final.png", p1, device = "png", width = 6, height = 3.5,
       dpi = 300)

resultados %>% 
  filter(velocidade %% 5 == 0) %>% 
  mutate(tempo_ganho = (tempo - lag(tempo))*-1,
         risco = scales::percent(risco),
         tempo_ganho = round(tempo_ganho, 1),
         tempo_perc = (lag(tempo) - tempo) / lag(tempo))

