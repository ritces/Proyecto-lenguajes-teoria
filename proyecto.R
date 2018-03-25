#Proyecto Lenguajes
# libraries:
library(plotrix)
setwd("C:/Users/YezaRincon/Desktop/ProyectoLenguajes")

#Datos 1 y datos 2 creo q son los mismos

# Lectura de los archivos
datos1=read.table("notas1.txt", header=T)
datos2=read.table("notas2.txt", header=T)
datos3=read.table("notas3.txt", header=T)
datos4=read.table("notas4.txt", header=T)
datos5=read.table("notas5.txt", header=T)

# PARTE 1
#Analisi descriptivo de todos los datos con enfoque en los histogramas de las
#frecuencias de las notas de en cada examen de cada trimestre, además de la data
#que provee el comando summary
colors = c("yellow", "cyan", "violet", "green", "pink", "orange", "red")
hist(datos1$Examen1, main = "Histograma del Examen1 trimestre1", ylab = "Frecuencias", xlab= "datos", col="cyan")
hist(datos1$Examen2, main = "Histograma del Examen2 trimestre1", ylab = "Frecuencias", xlab= "datos", col="yellow")
hist(datos1$Examen3, main = "Histograma del Examen3 trimestre1", ylab = "Frecuencias", xlab= "datos", col="violet")
summary(datos1)

hist(datos2$Examen1, main = "Histograma del Examen1 trimestre2", ylab = "Frecuencias", xlab= "datos", col="cyan")
hist(datos2$Examen2, main = "Histograma del Examen2 trimestre2", ylab = "Frecuencias", xlab= "datos", col="yellow")
hist(datos2$Examen3, main = "Histograma del Examen3 trimestre2", ylab = "Frecuencias", xlab= "datos", col="violet")
summary(datos2)

hist(datos3$Examen1, main = "Histograma del Examen1 trimestre3", ylab = "Frecuencias", xlab= "datos", col="cyan")
hist(datos3$Examen2, main = "Histograma del Examen2 trimestre3", ylab = "Frecuencias", xlab= "datos", col="yellow")
hist(datos3$Examen3, main = "Histograma del Examen3 trimestre3", ylab = "Frecuencias", xlab= "datos", col="violet")
summary(datos3)

hist(datos4$Examen1, main = "Histograma del Examen1 trimestre4", ylab = "Frecuencias", xlab= "datos", col="cyan")
hist(datos4$Examen2, main = "Histograma del Examen2 trimestre4", ylab = "Frecuencias", xlab= "datos", col="yellow")
hist(datos4$Examen3, main = "Histograma del Examen3 trimestre4", ylab = "Frecuencias", xlab= "datos", col="violet")
summary(datos4)

hist(datos5$Examen1, main = "Histograma del Examen1 trimestre5", ylab = "Frecuencias", xlab= "datos", col="cyan")
hist(datos5$Examen2, main = "Histograma del Examen2 trimestre5", ylab = "Frecuencias", xlab= "datos", col="yellow")
hist(datos5$Examen3, main = "Histograma del Examen3 trimestre5", ylab = "Frecuencias", xlab= "datos", col="violet")
summary(datos5)


# PARTE 2
#Para cada trimestre, ¿El promedio de la nota definitiva (en base a 5) es menor que 3?

# Hipotesis nula: El promedio de las notas definitivas es igual a 3
# Hipotesis alternativa: El promedio de la nota definitiva es menor a 3
datosDef1 = datos1[,4]
t.test( datosDef1, alternative = "less", mu = 3, conf.level = 0.99 )
# p-valor= 0.05167 que es mayor a 0.01, por tanto NO se rechaza la hipotesis nula,
# en consecuencia, se acepta la hipotesis nula que afirma que en promedio la nota
# definitiva no fue menor que 3

# Hipotesis nula: El promedio de las notas definitivas es igual a 3
# Hipotesis alternativa: El promedio de la nota definitiva es menor a 3
datosDef2 = datos2[,4]
t.test( datosDef2, alternative = "less", mu = 3, conf.level = 0.99 )
# p-valor= 0.0008054 que es menor que 0.01, por tanto se rechaza la hipotesis nula,
# en consecuencia, se acepta la hipotesis alternativa que afirma que en promedio la nota
# definitiva es menor que 3

# Hipotesis nula: El promedio de las notas definitivas es igual a 3
# Hipotesis alternativa: El promedio de la nota definitiva es menor a 3
datosDef3 = datos3[,4]
t.test( datosDef3, alternative = "less", mu = 3, conf.level = 0.99 )
# p-valor= 4.461e-05 que es menor que 0.01, por tanto se rechaza la hipotesis nula,
# en consecuencia, se acepta la hipotesis alternativa que afirma que en promedio la nota
# definitiva es menor que 3

# Hipotesis nula: El promedio de las notas definitivas es igual a 3
# Hipotesis alternativa: El promedio de la nota definitiva es menor a 3
datosDef4 = datos4[,4]
t.test( datosDef4, alternative = "less", mu = 3, conf.level = 0.99 )
# p-valor= 0.4478 que es mayor que 0.01, por tanto NO se rechaza la hipotesis nula,
# en consecuencia, se acepta la hipotesis nula que afirma que en promedio la nota
# definitiva es 3

# Hipotesis nula: El promedio de las notas definitivas es igual a 3
# Hipotesis alternativa: El promedio de la nota definitiva es menor a 3
datosDef5 = datos5[,4]
t.test( datosDef5, alternative = "less", mu = 3, conf.level = 0.99 )
# p-valor= 0.1486 que es mayor que 0.01, por tanto NO se rechaza la hipotesis nula,
# en consecuencia, se acepta la hipotesis nula que afirma que en promedio la nota
# definitiva es 3

############################################################################
# PARTE 3
# Prueba de hipótesis de si en promedio el tercer examen es reprobado por
# los estudiantes
# Trimestre 1
# Un alumno que ya curso la materia afirma que en más del 60% de los casos, las
# personas reprueban el tercer examen.Si 17 de los 28 datos presentan esta 
# característica, ¿se respaldaría esta afirmación? Con los datos disponibles del 
# trimestre 1, realizar el contraste de hipótesis adecuado. Realice esta prueba
# a nivel alfa = 0.1.
binom.test(17, 28, p = 0.6, alternative = c("greater"), conf.level = 0.99 )
# Resultado:
# H0: media > 60%
# Ha: media < 60%
# Entonces, como el p-valor es 0.551 mayor que 0.01, entonces NO se rechaza la hipotesis
# nula, por tanto se puede afirmar que la mayoria de los estudiantes del examen#3
# del trimestre 1 aprobaron la materia.

# Trimestre 2
# Un alumno que ya curso la materia afirma que en más del 60% de los casos, las
# personas reprueban el tercer examen.Si 15 de los 28 datos presentan esta 
# característica, ¿se respaldaría esta afirmación? Con los datos disponibles del 
# trimestre 2, realizar el contraste de hipótesis adecuado. Realice esta prueba
# a nivel alfa = 0.1.
binom.test(15, 28, p = 0.6, alternative = c("greater"), conf.level = 0.99 )
# Resultado:
# H0: media > 60%
# Ha: media < 60%
# Entonces, como el p-valor es 0.8132 mayor que 0.01, entonces NO se rechaza la hipotesis
# nula, por tanto se puede afirmar que la mayoria de los estudiantes del examen#3
# del trimestre 2 aprobaron la materia.

# Trimestre 3
# Un alumno que ya curso la materia afirma que en más del 60% de los casos, las
# personas reprueban el tercer examen.Si 15 de los 28 datos presentan esta 
# característica, ¿se respaldaría esta afirmación? Con los datos disponibles del 
# trimestre 3, realizar el contraste de hipótesis adecuado. Realice esta prueba
# a nivel alfa = 0.1.
binom.test(13, 28, p = 0.6, alternative = c("greater"), conf.level = 0.99 )
# Resultado:
# H0: media > 60%
# Ha: media < 60%
# Entonces, como el p-valor es 0.9501 mayor que 0.01, entonces NO se rechaza la hipotesis
# nula, por tanto se puede afirmar que la mayoria de los estudiantes del examen#3
# del trimestre 3 aprobaron la materia.

# Trimestre 4
# Un alumno que ya curso la materia afirma que en más del 60% de los casos, las
# personas reprueban el tercer examen.Si 15 de los 28 datos presentan esta 
# característica, ¿se respaldaría esta afirmación? Con los datos disponibles del 
# trimestre 4, realizar el contraste de hipótesis adecuado. Realice esta prueba
# a nivel alfa = 0.1.
binom.test(19, 28, p = 0.6, alternative = c("greater"), conf.level = 0.99 )
# Resultado:
# H0: media > 60%
# Ha: media < 60%
# Entonces, como el p-valor es 0.2588 mayor que 0.01, entonces NO se rechaza la hipotesis
# nula, por tanto se puede afirmar que la mayoria de los estudiantes del examen#3
# del trimestre 4 aprobaron la materia.

# Trimestre 5
# Un alumno que ya curso la materia afirma que en más del 60% de los casos, las
# personas reprueban el tercer examen.Si 15 de los 28 datos presentan esta 
# característica, ¿se respaldaría esta afirmación? Con los datos disponibles del 
# trimestre 5, realizar el contraste de hipótesis adecuado. Realice esta prueba
# a nivel alfa = 0.1.
binom.test(17, 28, p = 0.6, alternative = c("greater"), conf.level = 0.99 )
# Resultado:
# H0: media > 60%
# Ha: media < 60%
# Entonces, como el p-valor es 0.551 mayor que 0.01, entonces NO se rechaza la hipotesis
# nula, por tanto se puede afirmar que la mayoria de los estudiantes del examen#3
# del trimestre 5 aprobaron la materia.

##############################################################################
# PARTE 4
# A traves de graficos Pie Chart represente los alumnos aprobados vs reprobados 
# de cada trimestre dado
aprobados1 = c()
aprobados1 = subset(datos1, Definitiva==3 | Definitiva==4 | Definitiva==5)
reprobados1 = c()
reprobados1 = subset(datos1, Definitiva==2 | Definitiva==1)

aprobados2 = c()
aprobados2 = subset(datos2, Definitiva==3 | Definitiva==4 | Definitiva==5)
reprobados2 = c()
reprobados2 = subset(datos2, Definitiva==2 | Definitiva==1)

aprobados3 = c()
aprobados3 = subset(datos3, Definitiva==3 | Definitiva==4 | Definitiva==5)
reprobados3 = c()
reprobados3 = subset(datos3, Definitiva==2 | Definitiva==1)

aprobados4 = c()
aprobados4 = subset(datos4, Definitiva==3 | Definitiva==4 | Definitiva==5)
reprobados4 = c()
reprobados4 = subset(datos4, Definitiva==2 | Definitiva==1)

aprobados5 = c()
aprobados5 = subset(datos5, Definitiva==3 | Definitiva==4 | Definitiva==5)
reprobados5 = c()
reprobados5 = subset(datos5, Definitiva==2 | Definitiva==1)

suma1=sum(aprobados1$Definitiva)
suma2=sum(aprobados2$Definitiva)
suma3=sum(aprobados3$Definitiva)
suma4=sum(aprobados4$Definitiva)
suma5=sum(aprobados5$Definitiva)

sumr1=sum(reprobados1$Definitiva)
sumr2=sum(reprobados2$Definitiva)
sumr3=sum(reprobados3$Definitiva)
sumr4=sum(reprobados4$Definitiva)
sumr5=sum(reprobados5$Definitiva)

colors2 = c("yellow", "cyan")

partes1 <- c(suma1,sumr1) 
etiquetas <- c("Aprobados", "Reprobados")
pie3D(partes1,labels=etiquetas,explode=0.1, main="Trim1. Aprobados vs Reprobados", col = colors2)

partes2 <- c(suma2,sumr2) 
pie3D(partes2,labels=etiquetas,explode=0.1, main="Trim2. Aprobados vs Reprobados", col =colors2)

partes3 <- c(suma3,sumr3) 
pie3D(partes3,labels=etiquetas,explode=0.1, main="Trim3. Aprobados vs Reprobados", col =colors2)

partes4 <- c(suma4,sumr4) 
pie3D(partes4,labels=etiquetas,explode=0.1, main="Trim4. Aprobados vs Reprobados", col =colors2)

partes5 <- c(suma5,sumr5) 
pie3D(partes5,labels=etiquetas,explode=0.1, main="Trim5. Aprobados vs Reprobados", col =colors2)

#A partir de los resultados gráficos se puede observar que en el trimestre 3 hubo mayor cercania
#entre la cantidad de aprobados vs reprobados.