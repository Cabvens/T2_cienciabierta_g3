# Revision de multicolinealidad (matriz de correlacion)
datos_matriz <- datos %>%
  select(depresion, sensacion_fracaso, despido, estado_civil, 
         satis_ingreso, indice_ideacion_suicida) %>%
  mutate(across(everything(), as.numeric))


matriz_cor <- cor(datos_matriz, use = "pairwise.complete.obs")

round(matriz_cor, 2)

# Modelo de regresion lineal multiple 

modelo <- lm(indice_ideacion_suicida ~ satis_ingreso + despido + sensacion_fracaso + 
              depresion + estado_civil, data = datos)
summary(modelo)

  ## coeficiente estandarizado.
  summary(lm.beta(modelo))

  

