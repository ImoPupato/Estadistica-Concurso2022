#Set Working Directory and load data
setwd("G:/Mi unidad/Git/Estadistica-Concurso/Estadistica-Ejemplo-1")
data <- read.csv("G:/Mi unidad/Git/Estadistica-Concurso/Estadistica-Ejemplo-1/data.txt", sep="")
summary(data)

#Carga de librerías
library(car)

#Gráficos exploratorios
qqPlot(data$Conc., pch=19,
       main='QQplot para la Concentración de Cinc en muestras fisiológicas',
       xlab='Cuantiles teóricos',
       ylab='Cuantiles muestrales')

hist(data$Conc., freq=TRUE,
     main='Histograma de la Concentración de Cinc en muestras fisiológicas',
     xlab='Cc (ppm)',
     ylab='Frecuencia')

shapiro.test(data$Conc.) # Realizamos el ensayo de normalidad

#Creación del IC: desvio = 5 ppm, conf. = 0.95
media<-mean(data$Conc.)
n<-length(data$Conc.)
desvio<-5
errorst<-desvio/sqrt(n)
Z_0.975<-1.96 # Recordar que alpha=0.05, 1-alpha/2 = 0.975

lim_inf<-media-(Z_0.975*errorst)
lim_sup<-media+(Z_0.975*errorst)

interval_m<-data.frame(n, media, desvio, lim_inf, lim_sup)

interval_m



#En caso de desconocer la variancia
t.test(x=data$Conc, conf.level=0.90, var.equal = )$conf.int

#Generación de data
x <- rnorm(n=100, mean=150, sd=5)
y<-seq(1,100,1)
x<-as.data.frame(x)
y<-as.data.frame(y)
y$Conc<-x$x
colnames(y)<-c("Muestra", "Conc.")
write.table(data,"data.txt")
