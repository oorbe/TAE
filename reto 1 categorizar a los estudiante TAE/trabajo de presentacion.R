getwd()
setwd('C:/Users/Oliver/Documents/9/TAE')
txt <- readLines('hilo tae2.txt', encoding = "UTF-8")

#install.packages('stringr')
library(stringr)

#Data Science 4 All.

#así soluciono  el problema de los que escribieron separado por \n
ali <- paste(cbind(txt[seq(5,10)])  , collapse = " | ") 
nico <- paste(cbind(txt[seq(64,69)])  , collapse = " | ") 

txt_final <- c(txt[-c(seq(4,11), seq(64,70))],ali, "",nico)

#lo que me interesa tiene tamaÃ±o mayor aprox a 50
vec <- c()
for (i in txt_final) {
  if (nchar(i) >50 ) {
    vec <- c(vec, i)
  }
}

#Para normalizar primero paso a minusculas
vec <- str_to_lower(vec)
vec
# Les quito las tildes
library(stringi)
vec <- stri_trans_general(vec,'Latin-ASCII')

# Hay unos errores usando como separaciÃ³n 1 y I en vez de |
length(str_which(vec, ' \\| ')) # vec  y la condiciÃ³n no son del mismo tamaÃ±o entonces
                                 # voy a encontrar los raritos
arreglos <- vec[str_which(vec, ' \\| ', T)] # los encontrÃ©

# los siguientes son los arreglos correspondientes
arreglos[1] <- str_replace_all(arreglos[1],',',' |')
arreglos[2] <- str_replace_all(arreglos[2], ' l ', ' | ')
arreglos[2] <- str_replace_all(arreglos[2], '[:alpha:]{1}[:digit:]{2}[:alpha:]{1}', '| 21 |')
arreglos[3] <- str_replace_all(arreglos[3], ',', ' | ')
arreglos[3] <- str_replace_all(arreglos[3], 'hola \\|  soy ', '') # me doy cuenta de que ya esta

# aqui concateno los buenos con los arreglos
vec3 <- c(vec[str_which(vec, ' \\| ')], arreglos[c(1,2)])

# elimino puntaucion y simbolos
vec3 <- str_replace_all(vec3,'[:punct:]','')
# aqui los separo por |
vec4 <- str_split(vec3, '\\|') # me resulto mas facil separarlo sin tener en cuenta los espacios

# decubro que el 61 tiene problemas de "" y debo borrar para que 
# se extraigan en el mism orden que lo necesit y hago lo siguiente:
vec4 <- c(vec4[-61],list(vec4[[61]][c(2,3,4,5,6,7)]))

# aqui selecciono los que tienen 6 elementos que es lo que nos piden
# y saco los que no cumplen para revisar
vec_raros <- c()
vec5 <- c()
for (i in vec4) {
  if (length(i) == 6 ){
    vec5 <- c(vec5,list(i))
  }else{vec_raros <- c(vec_raros,list(i))}
  print(i)
}

vec5
vec_raros # decido excluirlos por el momennto para luego de tener los demas datos proceder como indico
falto_trabajo <- vec_raros[c(1,2,5,8,11)] # se le asignara un 'no'
falto_semestre <- vec_raros[c(3,4,7,10)] #  se le asignra promedio
falto_mucho <- vec_raros[c(6)]  # semestre promedio no labora y tendra pasatiempo leer
separar_ultima_entrada <- vec_raros[c(9)]



# creo los vectores y voy borrando espacios en blanco incio y final
nombres <- trimws(sapply(vec5, '[', 1))
edad <- trimws(sapply(vec5, '[', 2))
programa0 <- trimws(sapply(vec5, '[', 3))
semestre0 <- trimws(sapply(vec5, '[', 4))
trabaja <- trimws(sapply(vec5, '[', 5))
pasatiempo <- trimws(sapply(vec5, '[', 6))

# VOy a añadir los raritos con lo dicho:
ft <- trimws(sapply(falto_trabajo, '[', c(1:5)))
fs <- trimws(sapply(falto_semestre, '[', c(1:5)))
fm <- trimws(sapply(falto_mucho, '[', c(1:3)))
sue <- trimws(sapply(separar_ultima_entrada, '[', c(1:5)))
falto_mucho 
separar_ultima_entrada


nombres <- c(nombres,ft[1,],fs[1,],fm[1,],sue[1,])
edad <- c(edad,ft[2,],fs[2,],fm[2,],sue[2,])
programa0 <- c(programa0,ft[3,],fs[3,],fm[3,],sue[3,])
# semestre luego lo anadimos cuando podamos sacar el promedio
trabaja <- c(trabaja,rep('no',5),fs[4,],'no','desarrollo series')
pasatiempo <- c(pasatiempo,ft[5,],fs[5,],'leer','deporte')

#para la edad quito las letras y espacios en blanco y lo convierto a numerico:
edad <- as.numeric(str_replace_all(edad,'[:alpha:]|[:space:]',''))
# normalizo más los de sistemas llevandolos solo 'sitemas':
programa <- c()
for (i in programa0) {
  if (str_detect(i,'sistemas') == TRUE){
    programa <- c(programa,'sistemas')
  }else{
    programa <- c(programa,i)
  }
  
}
# quito las palabras ingenieria y 'de '
programa <- str_replace_all(programa, 'ingenieria |ingenieria de |ing ','')
programa <- str_replace_all(programa, 'de ','')

#Semestre es un desastre. vamos despacio, primero elimimo la palabra semestre.
semestre <- c()
for (i in semestre0) {
  if (str_detect(i,'semestre') == TRUE){
    semestre <- c(semestre,str_replace_all(i,'semestre',''))
  }else{    semestre <- c(semestre,i)
  }
}

#Luego identifico los patrones indeceados y los modifico correctamente
semestre <- str_replace_all(semestre,'octavo','8')
semestre <- str_replace_all(semestre,'xviii','16')
semestre <- str_replace_all(semestre,'septimo','7')
semestre <- str_replace_all(semestre,'ultimo','10')
semestre <- str_replace_all(semestre,'ix','9')
semestre <- str_replace_all(semestre,'3er ','3')
semestre[46] <- '12'
semestre <- as.numeric(semestre); semestre #buala

#añado los que faltaban del semestre:
round(mean(semestre),0) #la media es 8 para los que no pusieron el semestre
semestre <- c(semestre,ft[4,],rep(8,4),8,sue[4,])
# para trabajo reemplazo algunas cadenas que representan que no trabaja:
trabaja <- str_replace_all(trabaja,'no [:alpha:]*.|na|ninguna|desempleado','no')
trabaja[48] <- 'no'# no me quiso reemplazar, por eso lo hago mas manual
# un 'no' para los campos vacios
trabaja1 <- c()
for (i in trabaja) {
  if (nchar(i) == 0 ) {
    trabaja1 <- c(trabaja1, 'no')
  }else{trabaja1 <- c(trabaja1, i)}
}

trabaja1

#Creamos el DF:
df <- data.frame(nombres, edad, programa, semestre, trabaja1, pasatiempo )
ifelse(test = df$trabaja1 == 'no', 'no','si')

# Para Buscar el sexo extraemos los primeros nombres:
name_1 <- sapply(str_split(df[,1], ' '), '[', 1)

# trabajo manual de separar hombre y mujer si repetir
n_h <- c("juan","santiago","sebastian","jean","daniel","julian","carlos","alejandro","arley","david","ismael",
  "mateo","jhonier",  "eider" ,"esteban", "miguel","diego" ,"james","hans","jelssin","edwar","nicolas",
  "esteban","oliver","edhy" )
n_m <- c("carolina", "laura","cristina", "jennifer","stephany","catherine","zuleima","estefania","juliana","allison" 
  ,"sara"  ,    "yuliza","salome", "ana","vanessa", "isabela")

# este loop arrojara los generos
sexo <- c()
for (i in name_1){
  if (i %in% n_h == T){
    sexo <- c(sexo,'H')
  }
  if (i %in% n_m == T) {
    sexo <- c(sexo,'M')
  }
}
sexo
# anado la columna al DF:
df['sexo'] <- sexo

dat <- df
