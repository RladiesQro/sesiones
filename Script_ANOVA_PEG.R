##### Analisis de datos de experimentos: Diseños factoriales######
############## Dra. Patricia E. Garcia ##########################
############## Para R-ladies Qro      #########################
############################################################
### Instalar los paquetes###
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("pwr")
install.packages("rstatix")
install.packages("car")
install.packages("ggpubr")
install.packages("datarium")
install.packages("multcompView")
install.packages("emmeans")


### Cargar los paquetes a usar###

library("ggplot2")
library("tidyverse")
library("pwr")
library("rstatix")
library("car")
library("ggpubr")
library("datarium")
library("multcompView")
library("emmeans")

### Definir el directorio de trabajo###
setwd("C:/Users")###Agregar el directorio (Esta carpeta es comun en el entorno de windows)

###Agregar el directorio

setwd("C:/Laboratorio de Fotobiologia/Curso de R/r-ladies/Anova")

############### Comparación de dos grupos###############
### Peso en aves###
### Colecte el peso de distintas aves y registré si eran machos o hembras
peso_hembras <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
peso_machos <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Create a data frame
data <- data.frame( 
  Grupo = rep(c("Hembras", "Machos"), each = 9),
  Peso = c(peso_hembras,  peso_machos)
)
data
### Verificar los supuestos###
## Normalidad Shapiro o Kolmogorov

ggqqplot(data$Peso)### grafico de los cuantiles

shapiro.test(data$Peso)## Shapiro

ks.test(data$Peso, "pnorm", mean=mean(data$Peso), sd=sd(data$Peso))## Kolmogorov-Smirnov

## Homogeneidad de varianzas

levene_test(data, Peso ~ Grupo, center=mean)### Test de Levene

bartlett.test(Peso ~ Grupo, data) ### Test de Bartlett

### Test de Student o t-test ###
res.1 <- data %>% t_test(Peso ~ Grupo)
res.1

### Graficar con ggpubr

Fig_1 <- ggplot(
  data, aes(Grupo, Peso)) + geom_boxplot(col="deeppink4") + theme_classic2()
 

Fig_1 ## crea la figura 1


Fig_1 + labs(subtitle = get_test_label(res.1, detailed = TRUE), x="", y="Peso (gr)")### Grafico para publicar

#Opción b como barras con la desviación estandard 

Fig_1a <- ggbarplot(
  data, x = "Grupo", y = "Peso", 
  add = c("mean_sd"),color="steelblue", 
  fill= "steelblue",
   ylab=c("Peso (gr)")
)
Fig_1a + labs(subtitle = get_test_label(res.1, detailed = TRUE))

##################################################################
############### Volvemos a la presentación########################



####################################################################
######Prueba no-paramétrica#####
### Ejemplo#####
### Datos del tamaño molecular de la materia orgánica en un experimento###
### datos propios no publicados#####
trat_oscuro <- c( 0.0137, 0.0135, 0.0137)
trat_PARUVR <- c( 0.0159, 0.0159, 0.0157 )

data_1 <- data.frame( 
  Tratamiento = rep(c("Dark", "PARUVR"), each = 3),
  Dato = c(trat_oscuro,  trat_PARUVR)
)
data_1
### Verificar los supuestos###
## Normalidad Shapiro o Kolmogorov
ggqqplot(data_1$Dato)### grafico de los cuantiles

shapiro.test(data_1$Dato)## Shapiro- Normalidad


bartlett.test(Dato~Tratamiento,data_1)## Homogeneidad de varianzas

####Mann-Whitney (Prueba no Parametrica)

stat_test_1<-data_1 %>% wilcox_test(Dato ~ Tratamiento)### Mann-Whitney

stat_test_1
### Realizo el grafico de mis resultados

Fig_2 <- ggbarplot(
  data_1, x = "Tratamiento", y = "Dato", 
  add = c("mean_sd"),color="deeppink4", 
  fill= "pink",
   ylab=c("S275-295"), xlab=c("Treatment")
)
Fig_2 + labs(subtitle = get_test_label(stat_test_1, detailed = TRUE))

##################################################################
############### Volvemos a la presentación ########################



############### Comparación de más de 2 grupos ############### 
#####Anova de una via#####
###ONE-Way ANOVA###

### Importamos los datos ### 
data("PlantGrowth")
d <- PlantGrowth ### peso seco de plantas que fueron sometidas a tres tratamientos
d
summary(d)
d %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")
### Verificación de los supuestos
ggqqplot(d$weight)
shapiro.test(d$weight)## Shapiro para verificar la normalidad
bartlett.test( weight ~ group, d)## Bartlett para verificar la homogeneidad de varianzas

###Anova de una via (ONE-Way ANOVA)
res.aov <- d %>% anova_test(weight ~ group)
res.aov
plot (res.aov)
##################################################################
############### Volvemos a la presentación########################

## Test a posteriori
poshoc <- d %>% emmeans_test(weight~group, p.adjust.method = "bonferroni")## Bonferroni
poshoc

poshoc <- poshoc %>% add_xy_position(x = "group")### para graficar los contrastes del test a posteriori

Fig_3 <- ggplot(d, aes( group,weight))+ geom_boxplot( col="green") + theme_classic2()

Fig_3
Fig_3 + stat_pvalue_manual(poshoc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(poshoc) ## muestra que se uso Bonferroni
  )
### Grafico en barras media y SD

Fig_3a <- ggbarplot(d, x = "group", y = "weight", color= "gold1", fill= "yellow", add = c("mean_sd"), ylab=c("Weight (pounds)"), xlab=c("")) 

Fig_3a + stat_pvalue_manual(poshoc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(poshoc) ## muestra que se uso Bonferroni
  )

Fig_3b <- ggbarplot(d, x = "group", y = "weight", color= "darkgrey", fill= "azure", add = c("mean_sd"), ylab=c("Weight (pounds)"), xlab=c("")) 

Fig_3b + stat_pvalue_manual(poshoc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE)
    
  )

########

rm(list=ls())# comando para limpiar el entorno

#### Analisis de la varianza de una via no-parametrico###
###Kruskal-Wallis ###
### Ejemplo con marketing###
data("marketing")
marketing <- data.frame( 
  type = rep(c("youtube", "facebook", "newspaper" ), each = 200),
  sales = c(marketing$youtube,  marketing$facebook, marketing$newspaper)
)
marketing
### Verificamos los supuestos###

ggqqplot(marketing$sales)
### usamos Kolmogorov porque tenemos muchos datos####

ks.test(marketing$sales, "pnorm", mean=mean(marketing$sales), sd=sd(marketing$sales))
## usamos levene que es menos sensible a la falta de normalidad###
levene_test(marketing, sales~type, center=mean)## Levene para verificar la homogeneidad de varianzas

### Kruskal-Wallis
res.kruskal <- marketing %>% kruskal_test(sales ~ type)###
res.kruskal

### Test a posteriori Tukey###
pkw <- marketing %>% 
  tukey_hsd(sales ~ type)
pkw

## Gráfico o la Figura

pkw <- pkw %>% add_xy_position(x = "type")

Fig_4 <- ggplot(marketing, aes(type,sales)) + geom_boxplot(col= "green", fill="darkolivegreen4") + stat_pvalue_manual(pkw, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pkw)
  ) + theme_classic()
  
Fig_4

Fig_4a <- ggbarplot(
  marketing, x = "type", y = "sales", 
  add = c("mean_sd"),color="aquamarine2", sort.val = c("asc"), 
  fill= "aquamarine", ylab=c("Sales (U$ dollars)"), xlab=c("Marketing type")
) +
  stat_pvalue_manual(pkw, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE)  )

Fig_4a


##################################################################
############### Volvemos a la presentación########################


### Analisis de la Varianza de dos vias o dos factores####
###TWO-Way ANOVA###

data(jobsatisfaction)

jobsatisfaction

### Verificamos los supuestos###
ggqqplot(jobsatisfaction, x="score", color="gender")
ggqqplot(jobsatisfaction, x="score", color="education_level")

jobsatisfaction %>%
  group_by(gender, education_level) %>%
  shapiro_test(score) ### Shapiro para verificar la normalidad (paquete rstatix)

levene_test(jobsatisfaction, score~as.factor(gender)*as.factor(education_level), center=mean)## Levene para verificar la homogeneidad de varianzas

### Analizamos los efectos principales####

res.aov3 <- jobsatisfaction %>% anova_test(score ~ gender + education_level)
res.aov3


### Analizamos la interacción entre los factores####

res.aov3a <- jobsatisfaction %>% anova_test(score ~ gender * education_level)

res.aov3a

plot (res.aov3a)


###### Test a posteriori
pwc <- jobsatisfaction %>% 
  group_by(gender) %>%
  emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 
pwc_1 <- jobsatisfaction %>% 
  group_by(education_level) %>%
  emmeans_test(score ~ gender, p.adjust.method = "bonferroni") 


pwc
pwc_1
###Grafico para publicar###
pwc <- pwc %>% add_xy_position(x = "gender")

Fig_5<- ggboxplot(jobsatisfaction, x = "gender", y = "score",
                  color = "education_level", palette = "Set2"
)

Fig_5 + stat_pvalue_manual(pwc) +
  labs(subtitle = get_test_label(res.aov3a, detailed = TRUE)
  )

##Otra opción usando el nivel de educación en el eje x ###


pwc_a <- pwc %>% add_xy_position(x = "education_level")

Fig_5a<- ggbarplot(jobsatisfaction, x = "education_level", y = "score",
                  fill = "gender", add=c("mean_sd"),  position = position_dodge(0.8))

Fig_5a + stat_pvalue_manual(pwc_a) +
  labs(subtitle = get_test_label(res.aov3a, detailed = TRUE)
  )
######Ejemplo con datos propios #######

###Efectos de la temperatura y de la exposición a la luz en la concentración de Carotenoides en copepodos

#### Garcia et al 2008. doi:10.1093/plankt/fbn041
rm(list=ls())# comando para limpiar el entorno
carotenoids <- data.frame(
  Temp=rep(c(5, 8, 12, 16, 20), each=6),
  Radi=rep(c("PAR+UVR", "Dark", "PAR+UVR","Dark", "PAR+UVR","Dark", "PAR+UVR","Dark", "PAR+UVR","Dark"), each= 3),
  Carot=c(1.39,1.67,1.57,1.79,2.05,1.56,3.83,2.91,3.47,2.30,1.96,2.43,2.67,2.74,2.35,1.12,1.47,1.71,7.71,7.20,7.90,2.96,2.80, 2.98,3.02,3.82,3.49,1.48,1.02,0.91)
)
carotenoids

### Verificamos los supuestos###
ggqqplot(carotenoids$Carot) ## Grafico de los cuantiles
ggqqplot(carotenoids, x="Carot", color="Radi")
carotenoids %>%
  group_by(Temp, Radi) %>%
  shapiro_test(Carot) ### Shapiro para verificar la normalidad (paquete rstatix)

levene_test(carotenoids, Carot~as.factor(Temp)*as.factor(Radi), center=mean)## Levene para verificar la homogeneidad de varianzas


###TWO-Way anova######

res.aov4 <- carotenoids %>% anova_test(Carot~ Temp + Radi)## Mirar los grados de libertad

res.aov4







carotenoids$Temp <- as.factor(carotenoids$Temp) ### Temperatura como factor

res.aov4 <- carotenoids %>% anova_test(Carot~ Temp + Radi)

res.aov4



###### Estudio el efecto interacción###### 

res.aov4a <- carotenoids %>% anova_test(Carot~ Temp * Radi)
res.aov4a
plot (res.aov4a)

###### Test a posteriori###### 

phtukey <- carotenoids %>% 
  group_by(Radi) %>%
  tukey_hsd(Carot ~ Temp) #### Test a posteriori de Tukey

phtukey


#### Grafico de los resultados 

phtukey <- phtukey %>% add_xy_position(x = "Radi")


Fig_6a <- ggbarplot(
  carotenoids, x = "Radi", y = "Carot", 
  fill="Temp",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Carotenoids (mg/DW))"), xlab=c("Radiation treatment")
)
Fig_6a + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )


Fig_6a + stat_pvalue_manual(phtukey) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6a + stat_pvalue_manual(phtukey, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

#####
phtukey <- phtukey %>% add_xy_position(x = "Temp")


Fig_6b <- ggbarplot(
  carotenoids, x = "Temp", y = "Carot", 
  fill="Radi",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Carotenoids (mg/DW))"), xlab=c("Temperature (°C)")
)
Fig_6b + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6b + stat_pvalue_manual(phtukey, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

######Gracias a R-ladies-QRO######

####### GRACIAS  USTED POR PARTICIPAR Y USAR ESTE SCRIPT######
