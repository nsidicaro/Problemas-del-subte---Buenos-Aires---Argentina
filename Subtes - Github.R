#Parte de construccion de Data

#Librerias utilizadas####
library(ggplot2)
library(twitteR)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr) #Sirve para Regex
library(datetime) ###Libreria para trabajar con tiempo

#Para extracci?n de twitter####
consumerKey = "XXXXXX"
consumerSecret = "XXXXXX"
accessToken = "XXXXXX"
accessSecret = "XXXXXX"
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)


#Crear y adecuar el DF####
#Creamos un objeto que guarde los tweets
Metroviastwits<- userTimeline("Metrovias", n = 3360, excludeReplies = T)

#Convertimos el objeto en una base de datos
Metroviastwits_df <- tbl_df(map_df(Metroviastwits, as.data.frame)) #Aca los paquetes que usa son dplyr y purrr
#Guardar los tweets con X mes en un csv porque el paquete solo mira 300 dias para atras
write.csv(Metroviastwits_df,'Tweets.csv')

#Borrar las columnas que no interesan
Metroviastwits_df$favorited <-NULL 
Metroviastwits_df$favoriteCount <-NULL
Metroviastwits_df$replyToSN <-NULL
Metroviastwits_df$replyToSID <-NULL
Metroviastwits_df$replyToUID <-NULL
Metroviastwits_df$truncated <-NULL
Metroviastwits_df$id <-NULL
Metroviastwits_df$statusSource <-NULL
Metroviastwits_df$screenName<-NULL
Metroviastwits_df$retweetCount <-NULL
Metroviastwits_df$longitude <-NULL
Metroviastwits_df$latitude <-NULL
Metroviastwits_df$retweeted <-NULL
Metroviastwits_df$isRetweet <-NULL

#Sacarle las tildes a todas las i para el regex para evitar diferencia dentro de las lineas
Metroviastwits_df$text <- gsub("í","i",Metroviastwits_df$text)

#Hacer una columna que solo contenga la linea que tiene los problemas
Metroviastwits_df$Linea <- str_extract(Metroviastwits_df$text,"#([lL]ínea[A-Za-z]|[lL]inea[A-Za-z]|Premetro|TrenUrquiza)")
#Revisar si solo quedaron esas lineas 
unique(Metroviastwits_df$Linea)
ISNAMensaje <- Metroviastwits_df[is.na(Metroviastwits_df$Linea),]
rm(ISNAMensaje)
#La informacion que no tiene asociada una linea no es util para el analisis, asi que se saca y creo un nuevo DF
Metroviastwits_df.2 <-Metroviastwits_df[!is.na(Metroviastwits_df$Linea),]
#Metroviastwits_df.2 pasa a ser el nuevo DF a utilizar

#Crear una matriz de fecha y hora
Fecha.Hora <- str_split_fixed(Metroviastwits_df.2$created," ",n=2)

#Unir el DF y la matriz fecha/hora
Metroviastwits_df.2 <-cbind(Metroviastwits_df.2,Fecha.Hora)
#Ponerle nombre a la columna de horas y fecha
colnames(Metroviastwits_df.2)[4] <- "Fecha"
colnames(Metroviastwits_df.2)[5] <- "Hora"


#Definicion de fallas####
#Crear columna con tipo de problema
Metroviastwits_df.2$Mensaje <- str_extract(Metroviastwits_df.2$text,"[Ll]imitad[oa]|[Cc]omplet[ao]|[Nn]ormal|[Dd]emora|[Ii]nterrumpid[oa]|[Ff]uerza|ya se detienen|no se detienen|[Nn]o se detiene|[Ff]recuencia|[Rr]establece|[Ee]special|no inicia|[Tt]od[oa]s|[Rr]eanuda|[Ss]in servicio")
#Mecanismo de control#Para ver si es significativo el hecho de que el mensaje sea NA 
ISNAMensaje <- Metroviastwits_df.2 %>%
  filter(is.na(Mensaje)) #En caso de que sea importante, hay que agregar opciones al c?digo anterior
rm(ISNAMensaje)
#Sacar del dataset que utilizo los mensajes inutiles
Metroviastwits_df.2 <- Metroviastwits_df.2[!is.na(Metroviastwits_df.2$Mensaje),]
#Convertir el mensaje todo en minuscula
Metroviastwits_df.2$Mensaje <- tolower(Metroviastwits_df.2$Mensaje)
#Remover la ultima letra del Mensaje para que todo sea neutro
Metroviastwits_df.2$Mensaje <-substr(Metroviastwits_df.2$Mensaje,1,nchar(Metroviastwits_df.2$Mensaje)-1)
unique(Metroviastwits_df.2$Mensaje)

#Definir causales####
#Agregar columna para distinguir los que tienen "por" en el mensaje. Proxy a justificacion 
Metroviastwits_df.2$Justifica <- str_extract(Metroviastwits_df.2$text,'[Pp]or')
Metroviastwits_df.2$Justifica[is.na(Metroviastwits_df.2$Justifica)]<-"No justificado"
Metroviastwits_df.2$Justifica <- tolower(Metroviastwits_df.2$Justifica)
Metroviastwits_df.2$Justifica <- str_replace(Metroviastwits_df.2$Justifica,'por','justificado')
unique(Metroviastwits_df.2$Justifica) #Revisar que no quede nada por fuera de la justificacion. Si hay un NA hay algo mal porque deber?a ser dicotomico

#Crear columna sobre Causas
Metroviastwits_df.2$Causas <- str_extract(Metroviastwits_df.2$text,"[Ff]uerza|[Oo]bra|[Oo]bras|[Ss]eguridad|[Aa]gua|[Mm]édica|[Aa]rrollamiento|[Ee]nerg[íi]a")
#Para saber si hay alguna causa justificada no tenida en cuenta. En caso de haberla, agregar al codigo anterior
Metroviastwits_df.2 %>%
  filter(Justifica=='justificado') %>%
  group_by(Causas) %>%
  summarize(n=n())
#Si las hay: ver cuales
Just.sin.causa <- Metroviastwits_df.2 %>%
  filter(Justifica=='justificado',is.na(Causas)) %>%
  group_by(text, Mensaje) %>%
  summarize(n=n())
rm(Just.sin.causa)

#A las filas sin datos de Causas, poner Metrovias
Metroviastwits_df.2$Causas[is.na(Metroviastwits_df.2$Causas)]<-"Metrovias"

#Normalizar datos####
#PAra ver si hay mensajes que tienen que ver entre si y unificarlos
unique(Metroviastwits_df.2$Mensaje)
####Unificar criterios de Metrovias para graficar
#No se detiene a Limitado
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"no se detiene","limitad")
#Complet a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"complet","norma")
#Reanuda a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"reanud","norma")
#Ya se detiene y complet a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"ya se detiene","norma")
#frecuenci a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"frecuenci","norma")
#Restablec a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"restablec","norma")
#toda a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"toda","norma")
#todo a norma
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"todo","norma")
#No se detiene a Limitado
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"especia","limitad")
#No inici a Interrumpido
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"no inici","interrumpid")
#No inici a Interrumpido
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"fuerz","interrumpid")
#sin servici a interrumpid
Metroviastwits_df.2$Mensaje <- str_replace(Metroviastwits_df.2$Mensaje,"sin servici","interrumpid")

unique(Metroviastwits_df.2$Mensaje)
#Sacar los mensajes de servicio regularizado normalizado####
#Armar un data set que no tenga los normales
Metroviastwits_df.2.sin.norma <-Metroviastwits_df.2[!Metroviastwits_df.2$Mensaje == "norma",]
#Metroviastwits_df.2.sin.norma pasa a ser el nuevo DF sobre el que trabajar


#Trabajar con horas y fechas####
#Darle formato de Horas a las horas
strptime(Metroviastwits_df.2.sin.norma$Hora, format="%H:%M:%S")
Metroviastwits_df.2.sin.norma$Hora <- format(strptime(Metroviastwits_df.2.sin.norma$Hora, format="%H:%M:%S"), format = "%H:%M:%S")

#Crear nueva columna de horas = 3:00:00
Metroviastwits_df.2.sin.norma$lastreshoras <- "03:00:00"
Metroviastwits_df.2.sin.norma$lastreshoras <- format(strptime(Metroviastwits_df.2.sin.norma$lastreshoras, format="%H:%M:%S"), format = "%H:%M:%S")

#Averiguar horario de falla. Hora de mensaje menos 3 horas, ya que el paquete le pone hora de creacion respecto a la hora 00:00
#########Transformar en horas:minutos los datos. Es un formato llamado as.time
Metroviastwits_df.2.sin.norma$Hora <- as.time(Metroviastwits_df.2.sin.norma$Hora) 
Metroviastwits_df.2.sin.norma$lastreshoras <- as.time(Metroviastwits_df.2.sin.norma$lastreshoras,format="%H:%M")  
####Resta de hora mensaje con 3hs para saber la hora de la falla comunicada en el twitter dado que la falla ocurre 3 horas antes por el uso horario de BSAS
Metroviastwits_df.2.sin.norma$Momento.Falla <- Metroviastwits_df.2.sin.norma$Hora - as.hour(3)

###Extraer la hora en la que paso. Exacta
Metroviastwits_df.2.sin.norma$Hora.sola <- format(Metroviastwits_df.2.sin.norma$Momento.Falla,"%H")

#Meses y año de cada mensaje
Metroviastwits_df.2.sin.norma$Mes <- format(as.Date(Metroviastwits_df.2.sin.norma$Fecha),"%Y-%m")



#Armar intervalos de horas####
Metroviastwits_df.2.sin.norma$Momento.dia <- Metroviastwits_df.2.sin.norma$Hora.sola
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "00","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "01","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "02","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "03","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "04","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "05","Madrugada")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "06","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "07","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "08","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "09","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "10","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "11","Mañana")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "12","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "13","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "14","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "15","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "16","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "17","Tarde")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "18","Noche")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "19","Noche")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "20","Noche")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "21","Noche")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "22","Noche")
Metroviastwits_df.2.sin.norma$Momento.dia <- str_replace(Metroviastwits_df.2.sin.norma$Momento.dia,
                                                         "23","Noche")
unique(Metroviastwits_df.2.sin.norma$Momento.dia)
#Datos de 2018 y mes final####
#Armar un data set que solo tenga los datos de 2018
Metroviastwits_df.2.sin.norma.2018<-Metroviastwits_df.2.sin.norma[str_detect(Metroviastwits_df.2.sin.norma$Mes,"2018"),]
unique(Metroviastwits_df.2.sin.norma.2018$Mes)
#Sacarle Noviembre porque faltan datos
Datos.nov.2018<-Metroviastwits_df.2.sin.norma.2018[str_detect(Metroviastwits_df.2.sin.norma$Mes,"2018-11"),]

#Filtrar por subte unicamente####
#Sacarle Premetro y Urquiza
Metr<-Datos.nov.2018[!Datos.nov.2018$Linea=="#TrenUrquiza",]
Datos.nov.2018<-Datos.nov.2018[!Datos.nov.2018$Linea=="#Premetro",]
unique(Datos.nov.2018$Linea)
#Control de fechas y subir csv####
min(Datos.nov.2018$created)
max(Datos.nov.2018$created)
write.csv(Datos.nov.2018,'Datos de noviembre.csv')


