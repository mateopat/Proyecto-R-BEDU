
# tidyverse incluye:
#   - dplyr
#   - ggplot2
#   - tidyr
#   - readr
#   - tibble
# Y otros paquetes útiles.

library(fbRanks)
library(tidyverse)

wd <- "/home/nicky/WorkingDirectory/Proyecto-R-BEDU/"                           # Directorio de Trabajo de Nicky. Recuerda modificarlo!

setwd(wd)

################################################################################
#
#                            Descarga de archivos
#                 https://www.football-data.co.uk/spainm.php
#
################################################################################

# Creamos un nuevo directorio donde guardar los documentos

rawData.dir <- "./raw_data"                                                     # variable que almacena el directorio donde van a ir los .csv descargados

if(!dir.exists(rawData.dir)){                                                   # si no existe la carpeta rawData.dir en el working directory
  dir.create(rawData.dir)                                                       # entonces hay que crearla
}

##############################################################################
#
# Generamos los URL donde están los .csv de forma programática y vamos 
# descargando los archivos, siguiendo el patrón de nombramiento, en el 
# directorio de trabajo.
#
##############################################################################
                                                                                

setwd(rawData.dir)                                                              # cambiamos temporalmente el working directory para descargar en rawData.dir
                                                                                # los archivos de forma sencilla
for (i in 0:9) {
  
  current.url <- paste(                                                         # Obtenemos las URL una por una de forma programática siguiendo el patrón que tienen los
    "https://www.football-data.co.uk/mmz4281/1",i,11+i,"/SP1.csv",              # archivos que nos interesan en www.football-data.co.uk
    sep = ""
  )
  
  temp.destfile <- paste("SP1-1",i,11+i,".csv", sep = "")                       # variable que almacena el nombre del archivo que vamos a descargar.
  
  if(!file.exists(temp.destfile)){                                              # si el archivo no está en el directorio de trabajo lo descargamos
    
    download.file(                                                              # de la URL almacenada en la variable current.url al rawData.dir
      url = current.url, 
      destfile = temp.destfile,
      mode = "wb"
    )
    
  }
  
}

################################################################################
#
#                              Lectura de datos
#
################################################################################

files <- lapply(list.files(path = "."), read.csv)                               # del directorio actual, guardamos los archivos csv en una lista.

setwd("./..")                                                                   # regresamos al wd original

################################################################################
#
#                            Procesamiento de datos
#
################################################################################

sfiles <- list()                                                                # inicializamos una lista vacía que se encargará de almacenar
                                                                                # los data.frame después de aplicarles un select.
for (i in 1:length(files)){
  
  temp.file <- files[[i]]                                                       # guarda temporalmente el archivo i-ésimo de files en temp.file
  
  temp.names <- names(temp.file)                                                # obtén las columnas del archivo que guardaste en temp.file
  
  if ("BbMx.2.5" %in% temp.names && "BbAv.2.5.1" %in% temp.names){              # si el archivo tiene las variables "BbMx.2.5" y "BbAv.2.5.1"
    
    temp.file <- select(temp.file, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
    
    # temp.file <- mutate(temp.file, Date = as.Date(Date, format = "%d/%m/%y"))
    
    # sfiles <- append(sfiles, list(temp.file))
    
  }else{                                                                        # si el archivo no tiene las variables "BbMx.2.5" y "BbAv.2.5.1"
    
    temp.file <- select(temp.file, Date:FTAG, Max.2.5:Avg.2.5.1)                # obten los valores de las columnas Date:FTAG y Max.2.5:Avg.2.5.1
    
    temp.file <- select(temp.file, -Time)                                       # a los valores seleccionados réstales la columna Time
    
    # temp.file <- mutate(temp.file, Date = as.Date(Date, format = "%d/%m/%y"))
    
    # sfiles <- append(sfiles, list(d1920 = temp.file))
    
  }
  
  temp.file <- mutate(temp.file, Date = as.Date(Date, format = "%d/%m/%y"))     # corregimos el formato de la fecha en el data.frame temp.file
  
  sfiles <- append(sfiles, list(temp.file))                                     # agrega en una lista el data.frame procesado, temp.file.
  
}

# Unimos de d1415S a d1819S

# Esto es O(n²). Es muy lento para n grande. Hay que buscar una mejor solución
# Para cada x en sfiles, vamos preguntando si para cada nombre en temp.names
# está la variable "BbMx.2.5

# Propongo que en el primer for de la sección de procesamiento de datos,
# busquemos una forma de discriminar "desde el primer nivel de iteración"
# al df que tiene la variable BbMX.2.5 como lo hace el código comentado en
# esa parte del script.

d1019S <- data.frame()
d1920S <- data.frame()

for (i in 1:length(sfiles)){
  
  temp.file <- sfiles[[i]]                                                      # guarda temporalmente el df i-ésimo de sfiles en temp.file
  
  temp.names <- names(temp.file)                                                # obtén las columnas del df que guardaste en temp.file
  
  if(!("BbMx.2.5" %in% temp.names)){                                            # Si BbMx.2.5 no es una variable de temp.file
    
    d1920S <- temp.file                                                         # La marcamos. Sabemos que sólo uno de los df la tiene, por lo que
                                                                                # hacer esto es suficiente.
  }else{
    
    d1019S <- rbind(temp.file, d1019S)                                          # Si BbMx.2.5 es una variable de temp.file
                                                                                # vamos juntando las columnas de los df que ya juntamos con el
                                                                                # df en temp.file
  }
}

# Renombrar columnas

d1019S <- rename(d1019S,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1920S <- rename(d1920S,  Max.2.5.O = Max.2.5, 
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

# Ordenamos las columnas

d1019S <- select(d1019S, colnames(d1920S))

# Volvemos a unir

d1020S <- rbind(d1019S, d1920S)

# Renombramos

d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

# Ordenamos columnas

data <- select(d1020S, date, home.team, home.score, away.team, away.score:Avg.2.5.U) # Este data frame contiene todos los datos necesarios

head(data, n = 2L); tail(data, n = 2L)

# Data frames de partidos y equipos

md <- data %>% select(date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba

f <- scores$date # Fechas de partidos
fu <- unique(f) # Fechas sin repetición
Ym <- format(fu, "%Y-%m") # Meses y años
Ym <- unique(Ym) # Meses y años sin repetir
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 

train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción

pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

# Continuar ajustando y prediciendo

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}

# Eliminamos NA's

buenos <- !(is.na(phs) | is.na(pas)) # 
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales

mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
# probabilidad condicional estimada de ganar en under 2.5

# Juegos con momios máximos

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p
