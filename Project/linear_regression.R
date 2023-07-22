model1 <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
            +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
              beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados)
summary(model1)$adj.r.squared

library(MASS)
bc<-boxcox(model1) 
lamda<-bc$x[which.max(bc$y)]
lamda
price_T<-((datos_filtrados$price^lamda)-1)/lamda
model1_T <-lm(price_T ~ host_identity_verified+host_is_superhost+instant_bookable
              +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados)
summary(model1_T)$adj.r.squared
# 0.6314668
ad.test(pnorm(rstandard(model1_T),mean=0,sd=1))
# 0.06132
ncvTest(model1_T) # non constant variance test H_0: los errores tienen varianza consntante
bptest(model1_T)
# NO


cooksDistance = cooks.distance(model1_T)
numOutliersCD = sum(cooksDistance > 4 / length(cooksDistance))
datos_filtrados_2<-datos_filtrados
datos_filtrados_2$cooksDistance <- as.numeric(cooks.distance(model1_T))

criterio_4_over_n<-datos_filtrados_2$cooksDistance[datos_filtrados_2$cooksDistance>4/length(datos_filtrados_2$cooksDistance)]
datos_filtrados_2_no_out <-datos_filtrados_2[!grepl(paste(criterio_4_over_n,collapse='|'),datos_filtrados_2$cooksDistance),]
# una vez filtrados, se calcula el modelo de nuevo con la base actualizada
model1_cooks <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
                  +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                    beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_2_no_out)
summary(model1_cooks)$adj.r.squared
# 0.6301603
# Revisamos de nuevo el supuesto de normalidad
qqPlot(model1_cooks$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Normal Quantiles", side = 1, line = 2.6, cex = 1)
#prueba estadística

ad.test(pnorm(rstandard(model1_cooks),mean=0,sd=1))
# como no se cumple le hacemos su boxplot
bc<-boxcox(model1_cooks) 
lamda<-bc$x[which.max(bc$y)]
lamda
price_T<-((datos_filtrados_2_no_out$price^lamda)-1)/lamda
model1_cooks_T <-lm(price_T ~ host_identity_verified+host_is_superhost+instant_bookable
                    +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                      beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_2_no_out)
summary(model1_cooks_T)$adj.r.squared
# 0.6824103

# REVISANDO LA NORMALIDAD
# Graficamente
qqPlot(model1_cooks_T$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Cuantiles dist normal", side = 1, line = 2.6, cex = 1) # se cumple nomralidad en errores
#prueba estadística
ad.test(pnorm(rstandard(model1_cooks_T),mean=0,sd=1))
# revisar si la media de los residuales es cercana a 0
mean(model1_cooks_T$residuals)

# Homocedasticidad
# graficamente, ambas graficas son para ver la varianza constante
resVarianceDF = data.frame(residuals = model1_cooks_T$residuals, fittedValues = model1_cooks_T$fitted.values)
ggplot(resVarianceDF, aes(x = model1_cooks_T$fitted.values, y = model1_cooks_T$residuals)) + 
  geom_point(alpha = 0.4) + 
  labs(x = 'Residuos', y = 'Ajustados') +
  theme_minimal() +
  theme(text = element_text(size = 13), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))

plot(model1_cooks_T,3) # varianza constante
plot(rstudent(model1_cooks_T)) # varianza conste
# Prueba estadistica
ncvTest(model1_cooks_T) # se cumple varianza constante

# Multicolinealidad
# hacemos la prueba vif pero como hay varios predictores cuantitativos no se da la colinealidad
vif(model1_cooks_T)

# Linealidad
plot(model1_cooks_T,1) 
library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datos_filtrados_2_no_out, aes(antiguedad,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_2_no_out, aes(accommodates,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_2_no_out, aes(bathrooms_text,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_2_no_out, aes(beds,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# explicar porque pareciera que nos e cumple la linealidad con los baños en resumen es por la observacion
# extrema, fuera de esto se cumple la linealidad
plot1 <- ggplot(data = datos_filtrados_2_no_out, aes(bedrooms,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_2_no_out, aes(amenities,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_2_no_out, aes(number_of_reviews,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_2_no_out, aes(review_scores_rating,model1_cooks_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# igual aca es explicar que para bedrooms no pareciera cumplirse por la observacion extrema, fuera de esto
# se cumpliria, al igual que para las demas variables



#PASO3: ANÁLISIS DE PUNTOS INFLUYENTES
#Distancia de Cook
plot(model1_cooks_T, 4)

#Puntos palanca para betas
plot(dfbetas(model1_cooks_T)[,1],type="h", ylab="Beta0")
plot(dfbetas(model1_cooks_T)[,2],type="h", ylab="Beta1")
plot(dfbetas(model1_cooks_T)[,3],type="h", ylab="Beta2")
plot(dfbetas(model1_cooks_T)[,4],type="h", ylab="Beta3")
plot(dfbetas(model1_cooks_T)[,5],type="h", ylab="Beta4")
plot(dfbetas(model1_cooks_T)[,6],type="h", ylab="Beta5")
plot(dfbetas(model1_cooks_T)[,7],type="h", ylab="Beta6")
plot(dfbetas(model1_cooks_T)[,8],type="h", ylab="Beta7")
plot(dfbetas(model1_cooks_T)[,9],type="h", ylab="Beta8")
plot(dfbetas(model1_cooks_T)[,10],type="h", ylab="Beta9")
plot(dfbetas(model1_cooks_T)[,11],type="h", ylab="Beta10")
plot(dfbetas(model1_cooks_T)[,12],type="h", ylab="Beta11")
plot(dfbetas(model1_cooks_T)[,13],type="h", ylab="Beta12")
plot(dfbetas(model1_cooks_T)[,14],type="h", ylab="Beta13")
plot(dfbetas(model1_cooks_T)[,15],type="h", ylab="Beta14")
plot(dfbetas(model1_cooks_T)[,16],type="h", ylab="Beta15")
plot(dfbetas(model1_cooks_T)[,17],type="h", ylab="Beta16")
#Puntos palanca para los datos ajustados
plot(dffits(model1_cooks_T),type="h")

# se denota laobservacion 9692 como influyente para el calculo de betas y valores ajustados
datos_filtrados_3<-datos_filtrados_2_no_out[-9692,]
model2 <-lm(price ~ host_identity_verified+host_is_superhost+instant_bookable
                  +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                    beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_3)
summary(model2)$adj.r.squared
# 0.62907
ad.test(pnorm(rstandard(model2),mean=0,sd=1))
bc<-boxcox(model2) 
lamda<-bc$x[which.max(bc$y)]
lamda
price_T<-((datos_filtrados_3$price^lamda)-1)/lamda
model2_T <-lm(price_T ~ host_identity_verified+host_is_superhost+instant_bookable
                    +zona_tipo+room_type+antiguedad+accommodates+bathrooms_text+
                      beds+bedrooms+amenities+number_of_reviews+review_scores_rating,data=datos_filtrados_3)
summary(model2_T)$adj.r.squared
# 0.6824131

# Analisis de supuestos
# REVISANDO LA NORMALIDAD
# Graficamente
qqPlot(model2_T$residuals, 
       ylab = "", 
       xlab = "")
mtext("Residuals", side = 2, line = 2.6, cex = 1)
mtext("Cuantiles dist normal", side = 1, line = 2.6, cex = 1) # se cumple nomralidad en errores
#prueba estadística
ad.test(pnorm(rstandard(model2_T),mean=0,sd=1))
# revisar si la media de los residuales es cercana a 0
mean(model2_T$residuals)

# Homocedasticidad
# graficamente, ambas graficas son para ver la varianza constante
resVarianceDF = data.frame(residuals = model2_T$residuals, fittedValues = model2_T$fitted.values)
ggplot(resVarianceDF, aes(x = model2_T$fitted.values, y = model2_T$residuals)) + 
  geom_point(alpha = 0.4) + 
  labs(x = 'Residuos', y = 'Ajustados') +
  theme_minimal() +
  theme(text = element_text(size = 13), 
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))

plot(model2_T,3) # varianza constante
plot(rstudent(model2_T)) # varianza conste
# Prueba estadistica
ncvTest(model2_T)# se cumple varianza constante
# Multicolinealidad
# hacemos la prueba vif pero como hay varios predictores cuantitativos no se da la colinealidad
vif(model2_T)

# Linealidad
plot(model2_T,1)
plot1 <- ggplot(data = datos_filtrados_3, aes(antiguedad,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_3, aes(accommodates,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_3, aes(bathrooms_text,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_3, aes(beds,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# explicar porque pareciera que nos e cumple la linealidad con los baños en resumen es por la observacion
# extrema, fuera de esto se cumple la linealidad
plot1 <- ggplot(data = datos_filtrados_3, aes(bedrooms,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos_filtrados_3, aes(amenities,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos_filtrados_3, aes(number_of_reviews,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos_filtrados_3, aes(review_scores_rating,model2_T$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)
# vemos que se cumple ahora si la linealidad para todas las variables al retirar dicho punto influyente


#ANÁLISIS DE PUNTOS INFLUYENTES
#Distancia de Cook
plot(model2_T, 4) # aunque parecieran existir, no son suficiente en cuanto a distancia de cook
# como para ser sobresalientes 
plot(model2_T, 5) # se muestra qui que los puntos palancas ya no generan algun cambio en la estimacion 

# observemos lo siguiente
#EL MODELO model2_T ES NUESTRO MODELO FINAL:
#CUMPLE CON:
#1)Normalidad
#2)Linealidad
#3)Homocedasticidad
#4)No tiene multicolinelaidad
#5)No hay observaciones influyentes
summary(model2_T) 
nom_col<- c( "Miguel Hidalgo",        
             "Cuajimalpa de Morelos", 
             "Cuauhtémoc",            
             "Venustiano Carranza",   
             "La Magdalena Contreras",
             "Benito Juárez",         
             "Álvaro Obregón",        
             "Iztacalco",      
             "Tlalpan",    
             "Gustavo A. Madero   ",  
             "Xochimilco",
             "Iztapalapa",
             "Azcapotzalco ",
             "Tláhuac"               
             ,"Milpa Alta",
             "Coyoacán") 

coeficientes <- model2_T[1]$coefficients

