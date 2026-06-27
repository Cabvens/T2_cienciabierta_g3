# paquetes y librerias ----

options(scipen = 999)
library(dplyr)
library(psych)
library(pacman)
library(lm.beta)

pacman::p_load(tidyverse,
               summarytools,
               sjmisc,
               sjPlot)

# Creacion de objeto ----


datos <- elsoc_wide_2016_2023
rm(elsoc_wide_2016_2023)

# 1. Selección de variables ----

datos <- datos %>% 
  select(s13_06_w06, s19_03_w06, s11_07_w06, m36_w06, m16_w06,
         s11_01_w06, s11_02_w06, s11_03_w06, s11_09_w06,
         m0_edad_w06)

# 2. Renombre de variables ----

datos <- datos %>% 
  rename(depresion         = s19_03_w06,
         sensacion_fracaso = s11_07_w06, 
         estado_civil      = m36_w06,
         despido           = s13_06_w06,
         satis_ingreso     = m16_w06,
         desinteres        = s11_01_w06,
         decaimiento       = s11_02_w06,
         dif_dormir        = s11_03_w06,
         autoflagelo       = s11_09_w06,
         edad              = m0_edad_w06)

# 3. Tratamiento de valores perdidos (ELSOC) ----

datos <- datos %>% 
  mutate(across(everything(),
                ~ ifelse(haven::zap_labels(.) %in% c(-888, -999), NA, .)))

# 4. Filtro por edad (Población objetivo: 30 años en adelante) ----

datos <- datos %>% 
  filter(edad >= 30) %>%
  select(-edad) # Eliminamos la variable tras filtrar según el diseño

# 5. Recodificaciones ---- 

datos <- datos %>%
  mutate(
    
    # Estado civil (0 = Con pareja, 1 = Sin pareja)
    estado_civil = case_when(
      haven::zap_labels(estado_civil) %in% c(1, 2, 3)          ~ 0, 
      haven::zap_labels(estado_civil) %in% c(4, 5, 6, 7, 8, 9) ~ 1, 
      TRUE ~ NA_real_
    ),
    
    # Depresión (1 = Sí, 0 = No)
    depresion = case_when(
      haven::zap_labels(depresion) == 1 ~ 1,
      haven::zap_labels(depresion) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Despido (1 = Sí, 0 = No)
    despido = case_when(
      haven::zap_labels(despido) == 1 ~ 1,
      haven::zap_labels(despido) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Sensación de fracaso (1 = Sí, 0 = No)
    sensacion_fracaso = case_when(
      haven::zap_labels(sensacion_fracaso) %in% c(2, 3, 4, 5) ~ 1, 
      haven::zap_labels(sensacion_fracaso) == 1 ~ 0, 
      TRUE ~ NA_real_
    ),
    
    # Satisfacción de ingreso (0 = Satisfecho, 1 = Insatisfecho)
    satis_ingreso = case_when(
      haven::zap_labels(satis_ingreso) %in% c(4, 5) ~ 0, 
      haven::zap_labels(satis_ingreso) %in% c(1, 2) ~ 1, 
      TRUE ~ NA_real_
    )
  )

# 6. Creación del índice sumativo con Prorrateo (Pairwise) ----

# Rango: 0 a 16 puntos.

datos <- datos %>%
  mutate(
    temp_des = as.numeric(desinteres) - 1,
    temp_dec = as.numeric(decaimiento) - 1,
    temp_dif = as.numeric(dif_dormir) - 1,
    temp_aut = as.numeric(autoflagelo) - 1
  ) %>%
  mutate(
    indice_ideacion_suicida = rowMeans(
      cbind(temp_des, temp_dec, temp_dif, temp_aut), 
      na.rm = TRUE
    ) * 4
  ) %>%
  select(-starts_with("temp_"))

# Modelo de regresion lineal multiple 

modelo <- lm(indice_ideacion_suicida ~ satis_ingreso + despido + sensacion_fracaso + 
              depresion + estado_civil, data = datos)
summary(modelo)

  ## coeficiente estandarizado.
  summary(lm.beta(modelo))