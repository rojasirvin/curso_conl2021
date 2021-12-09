# SU NOMBRE
# ALGUNOS COMENTARIOS AQUÍ
# Mi primer script

rm(list = ls()) # limpia todo lo que había antes
Sys.setlocale("LC_ALL", "en_US.UTF-8") # indicamos el locale, es decir, la configuración de donde estamos trabajando y que nos permite usar acentos y otros caracteres
options(scipen=999) # evitamos notación científica

library(tidyverse)
library(MatchIt)
library(Zelig)
library(rdrobust)


#Diseños discontinuos----

setwd("C:/Users/rojas/Dropbox/presentations_sites/curso_conl2021/data")

data.hs <- read_csv("./datos_pobreza.csv")

#Usaremos un paquete llamado rdrobust
#Recuerden que hay que instalarlo y luego llamarlo


#Descripción gráfica
#Sabemos que los municipios que reciben el programa son los que están por
#arriba de 59.1968 puntos de pobreza

x0 <- 59.1968

(rdplot(y = data.hs$mort_age59_related_postHS,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 1))


(rdplot(y = data.hs$mort_age59_related_postHS,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 2))



#Creamos una variable que indica a los municipios pobres
data.hs <- data.hs %>% 
  mutate(ispoor=ifelse(povrate60>=x0,1,0))

#Noten que debemos seleccionar la ventana
b <- 10

summary(lm(mort_age59_related_postHS ~ povrate60 + ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))


#Y ahora un modelo con un polinomio de orden 2

summary(lm(mort_age59_related_postHS ~ povrate60  + I(povrate60^2)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))







#Mostremos cómo las características observables no varían en el corte

#Población: census1960_pop
(rdplot(y = data.hs$census1960_pop,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))

#Población urbana: census1960_pcturban
(rdplot(y = data.hs$census1960_pcturban,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))

#Población de raza negra: census1960_pcturban
(rdplot(y = data.hs$census1960_pctblack,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))





#Ejercicio 1

#Estimemos ahora el modelo con discontinuidades con una ventana de
#15 puntos
#5 puntos






#Ahora agreguemos un polinomio de orden 3 y una ventana de 10 puntos






#Verificar paramétricamente que la población no cambia en el corte




#PSM----

#Leer datos y crear variables
setwd("C:/Users/rojas/Dropbox/presentations_sites/curso_conl2021/data")

data.smoking <- read_csv("./datos_peso.csv") %>% 
  mutate(smoke=ifelse(mbsmoke=="smoker",1,0)) %>% 
  mutate(married=ifelse(mmarried=="married",1,0)) %>% 
  mutate(firstbaby=ifelse(fbaby=="Yes",1,0))

#Instalemos y carguemos los siguientes dos paquetes
#MatchIt
#Zelig
#¿Cómo lo hacemos?


#La función zelig de la librería Zelig nos permite hacer modelos de muchos tipos

#En la clase anterior aprendimos que podemos usar regresión para hacer
#una diferencia de medias

#¿Qué vemos aquí?

zelig(bweight ~ smoke, model="ls", data=data.smoking)

zelig(married ~ smoke, model="ls", data=data.smoking)

zelig(medu ~ smoke, model="ls", data=data.smoking)



#Usamos MatchIt para construir las parejas
m.out <- matchit(formula=smoke ~ married + firstbaby + medu + nprenatal + foreign + mhisp + fage,
                 method = "nearest",
                 ratio = 1,
                 distance= "logit",
                 replace = FALSE,
                 data = data.smoking)

#El objeto m.out contiene información relevante

summary(m.out)


#También tiene información para hacer dos gráficos
plot(m.out, type = "jitter")

plot(m.out, type = "hist")


#Podemos ver la muestra emparejada
m.data <- match.data(m.out)

#Esta matriz nos dice quién es el match de quién
head(m.out$match.matrix)

#Una vez que tenemos la muestra emparejada, ahora sí podemos comparar
#tratados con no tratados

z.out <- zelig(bweight ~ smoke,
               data = m.data,
               model = "ls")

z.out



#Ahora cambiamos el algoritmos de matching a caliper con dos vecinos
m.out <- matchit(formula=smoke ~ married + firstbaby + medu + nprenatal + foreign + mhisp + fage,
                 method = "nearest",
                 distance= "logit",
                 replace = FALSE,
                 ratio = 2,
                 caliper = .1,
                 data = data.smoking)

#¿Qué concluimos?
zelig(bweight~smoke,
      data = match.data(m.out),
      model = "ls")





#Ejercicio 2----
#Estimemos el efecto de tratamiento usando PSM con las siguientes modificaciones

#Incluya el cuadrado de la edad del padre fage y la educación de la madre medu
#Use un ratio de 0.15
#Use tres vecinos dentro del caliper



