# Respuestas al ejercicio en grupos

rm(list = ls()) # limpia todo lo que había antes
Sys.setlocale("LC_ALL", "en_US.UTF-8") # indicamos el locale, es decir, la configuración de donde estamos trabajando y que nos permite usar acentos y otros caracteres
options(scipen=999) # evitamos notación científica


#Llamo los datos
setwd("C:/Users/rojas/Dropbox/presentations_sites/curso_conl2021/data")

data.morocco <-read_csv("./marruecos_microfinanzas.csv")



#Realice una prueba de diferencia de medias para las siguientes variables de
#la línea base. ¿Se concluye que el experimento efectivamente se asignó de 
#forma aleatoria?

#Para cada variable podemos hacer una prueba t

#Miembros del hogar: El valor p es 0.181, no se rechaza la H0 de igualdad de medias
t.test(members_resid ~ treatment,
       data = data.morocco)

#Adultos: p=0.270, no rechazamos H0
t.test(nadults_resid ~ treatment,
       data = data.morocco)

#Edad jefe del hogar: p=0.2068, no rechazamos H0
t.test(head_age ~ treatment,
       data = data.morocco)

t.test(act_livestock ~ treatment,
       data = data.morocco)

t.test(act_business ~ treatment,
       data = data.morocco)

t.test(borrowed_total ~ treatment,
       data = data.morocco)



#También podrian hacer cada una de estas prubeas con una regresión
summary(lm(nadults_resid ~ treatment,
       data = data.morocco))


#La conclusión es que las características están bien balanceadas
#Es decir, hay evidencia de que el experimento ocurrió de acuerdo a lo planeado
#Por tanto, podemos comparar las variables de resutlados post intervención
#y estar seguros que esas comparaciones ya no tienen sesgo de selección






#Estime el impacto del programa en una de las siguientes variables. Para
#hacerlo, usarán una regresión corta y una regresión larga
#(usando las variables de la línea base como controles). ¿Tuvo el programa 
#un impacto en la variable de resultados?

#Tomemos una variable: income_assetsales

#La regresión corta es:

summary(lm(income_assetsales ~ treatment,
           data = data.morocco))
#Noten que t=-2.512 o |t|>2 (nuestra regla de dedo)
#Además, p=0.012
#Hay un efecto estadísticamente significativo del programa sobre la venta
#de activos (algo bueno, creemos)
#La venta de activos se reduce en 628 unidades monetarias en el grupo
#tratado

#La regresión larga es

summary(lm(income_assetsales ~ treatment+
             members_resid +
             nadults_resid +
             head_age +
             act_livestock + 
             act_business +
             borrowed_total,
           data = data.morocco))

#El efecto estimado es de 237, significativo al 10%
#Esto significa que p no es menor que 0.05, pero sí es menor que 0.10


#Este ejercicio debería ser realizado por cada equipo, cambian la variable dependiente

