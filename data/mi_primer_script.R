# SU NOMBRE
# 8-dic-2021
# Diplomado de Evaluación de Impacto
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

#Un rectánculo tiene una base igual a 10 unidades y una altura igual a 4 unidades. Genere una constante llamada area.rect que indique el área del rectánculo

base <- 10
altura <- 4
area.rect <- base * altura

# ¿Qué pasa si el rectángulo ahora tiene una altura de 5? Genere la nueva área area.rect.2

base1 <- 10
altura1 <- 5
area.rect <- base1 * altura1


#Un círculo tiene radio de 2.6. Genere una constante d.circ que indique el área del círculo y otra p.circ que indique el perímetro

radio <- 2.6
d.circ <- pi * radio^2 


p.circ <- 2.6 * 2 * pi









#Directorio de trabajo----
#Cambien el directorio a un lugar donde tengan sus archivos de este curso
#Noten que usamos "/" en lugar "\"
setwd("C:/Users/rojas/Dropbox/presentations_sites/curso_conl2021/data") #setwd es fijar directorio de trabajo
getwd()

#Paquetes----
#Si no tienen instalados los paquetes, descomenten
#install.packages("tidyverse")

#Llamar tidiverse
library(tidyverse)




#El data frame de la evaluación en Marruecos
#Ya deben haber señalado con setwd dónde están su archivo de datos

data.morocco <-read_csv("./marruecos_microfinanzas.csv")

#Para ver los nombres de lass variables uso colnames()
colnames(data.morocco)


#Ejercicio 2----

#¿Qué hacen las funciones que observa en las diapositivas?
#Por ejemplo, ejectute
head(data.morocco)
head(data.morocco, n=10)


tail(data.morocco)
dim(data.morocco)
nrow(data.morocco)
ncol(data.morocco)
str(data.morocco)




#Estadística descriptiva----

#La media de la edad
mean(data.morocco$head_age)

#Reconociendo que hay valores NA
mean(data.morocco$head_age,
     na.rm=TRUE)

sd.edad <- sd(data.morocco$head_age, na.rm=T)
sd.edad








#Ahora por grupos: copie la instrucción de las diapostivas para calcular
#la edad promedio en el grupo tratado y el de control

data.morocco %>% 
  group_by(treatment) %>%
  summarize(mean=mean(head_age,na.rm=T)) %>% 
  ungroup()


  








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

