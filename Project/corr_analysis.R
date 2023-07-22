# analysis of correlations between explanatory variables
# used to select the variables in the linear model
round(cor(x = datos_filtrados[,7:15], method = "pearson"), 3)
library(corrplot)
corrplot(cor(datos_filtrados[,7:15]), method = "number")

# estos boxplot son para ver como el precio va cambiando segun el valor de la variable cualitativa
ggplot(data = datos_filtrados, mapping=aes(x = zona_tipo, y = price, color=zona_tipo)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
ggplot(data = datos_filtrados, mapping=aes(x = neighbourhood_cleansed, y = price, color=neighbourhood_cleansed)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
ggplot(data = datos_filtrados, mapping=aes(x = host_identity_verified, y = price, color=host_identity_verified)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
ggplot(data = datos_filtrados, mapping=aes(x = host_is_superhost, y = price, color=host_is_superhost)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
ggplot(data = datos_filtrados, mapping=aes(x = instant_bookable, y = price, color=instant_bookable)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")
ggplot(data = datos_filtrados, mapping=aes(x = room_type, y = price, color=room_type)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  theme_bw() + theme(legend.position = "none")

# en estos es explicar la correlacion entre precio y cada variable cuantitativa
# Observamos que la var respuesta no tiene un comportamiento normal, por lo tanto, para el analisis de correlaciones
# optamos por graficar el logaritmo del mismo (explicar) pues muy probablemente se le realice alguna transformacion
library(ggplot2)
ggplot(datos_filtrados, aes(x=antiguedad, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=accommodates, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=bathrooms_text, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=beds, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=bedrooms, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=amenities, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=number_of_reviews, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
ggplot(datos_filtrados, aes(x=review_scores_rating, y=log(price), color=zona_tipo)) + 
  geom_point(alpha=5/10) + theme_light()
