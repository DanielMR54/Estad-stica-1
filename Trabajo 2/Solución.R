uno <- read.table(file.choose(), header=T)
genera <- function(cedula){
  set.seed(cedula)
  data <- uno[sample(1:2000,120),]
  data
}

datos <- genera(1000098328)
datos[,4] <- as.factor(datos[,4])
datos[,5] <- as.factor(datos[,5])
 

#Primer punto.
estatura <- datos[,2]
shapiro.test(estatura)
  
summary(estatura)
sd(estatura)

 pt(-0.26,119)
 
#Segundo punto.
masaG <- datos[,1]

 
datosH <- datos[datos[,4]== "Hombre",]
datosM <- datos[datos[,4]== "Mujer",] 

masaH <- datosH[,1]
summary(masaH)
sd(masaH)

masaM <- datosM[,1]
summary(masaM)
sd(masaM)

pnorm(0.86, lower.tail = FALSE)


shapiro.test(masaG)
shapiro.test(masaH)
shapiro.test(masaM)

#Tercer punto.
length(datosH[,4])
pnorm(-1.53, lower.tail = FALSE)

#Cuarto punto.
ciudad <- datos[,5]
summary(ciudad)

pt(0.70, 5, lower.tail = FALSE)
