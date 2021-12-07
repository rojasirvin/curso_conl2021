# SU NOMBRE
# ALGUNOS COMENTARIOS AQUÍ
# Mi primer script

rm(list = ls()) # limpia todo lo que había antes
Sys.setlocale("LC_ALL", "en_US.UTF-8") # indicamos el locale, es decir, la configuración de donde estamos trabajando y que nos permite usar acentos y otros caracteres
options(scipen=999) # evitamos notación científica


#Objetos----
a <- 1
b <- 2
c <- a+b
c





#Ejercicio 1----








#Directorio de trabajo----
#Cambien el directorio a un lugar donde tengan sus archivos de este curso
#Noten que usamos "/" en lugar "\"

setwd("C:/Users/rojas/Dropbox/presentations_sites/curso_conl2021/data")



#Paquetes----
#Si no tienen instalados los paquetes, descomenten
#install.packages("tidiverse")

#Llamar tidiverse
#library(tidyverse)




#El data frame de la evaluación en Marruecos
#Ya deben haber señalado con setwd dónde están su archivo de datos

data.morocco <-read_csv("./marruecos_microfinanzas.csv")





#Ejercicio 2----

#¿Qué hacen las funciones que observa en las diapositivas?
#Por ejemplo, ejectute
head(data.morocco)







#Estadística descriptiva----

#La media de la edad
mean(data.morocco$head_age)

#Reconociendo que hay valores NA
mean(data.morocco$head_age, na.rm=TRUE)

#Ahora por grupos: copie la instrucción de las diapostivas para calcular
#la edad promedio en el grupo tratado y el de control





#Y si queremos más de una estadística
tabla.desc <- data.morocco %>% 
  group_by(treatment) %>%
  summarise(media.edad=mean(head_age, na.rm=T),
            de.edad=sd(head_age, na.rm=T),
            max.edad=max(head_age, na.rm=T),
            min.edad=min(head_age, na.rm=T),
            obs.edad=n()) %>% 
  ungroup()

tabla.desc






#Diferencias de medias----

#Con una prueba t

t.test(head_age ~ treatment,
       data = data.morocco)





#Regresión
#Llamen el archivo salarios.csv creando un objeto llamado datos.salarios

#Use read_csv, adaptando la forma en que leyó los datos de Marruecos

#Una vez que tenga el objeto en el ambiente, con estas indicaciones podrá
#construir el gráfico de dispersión

datos.salarios %>% 
  ggplot(aes(x=educacion, y=salario )) +
  geom_point()+
  labs(x="Educación", "Salario por hora")+
  geom_smooth(method = 'lm', se = F)

#La línea que obtiene en el gráfico es una línea de regresión que se estima
#con las sigueintes indicaciones

lm(salario ~ educacion,
   data=datos.salarios)

#Regularmente guardamos los resultados en un objeto

reg1 <- lm(salario ~ educacion,
           data=datos.salarios)

#Y luego visualizamos un resumen del objeto son summary
summary(reg1)

