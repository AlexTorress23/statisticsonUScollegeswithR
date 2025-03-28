
texto <- "\n
Data visualization contest with R: fourth edition

 Alejandro Torres García
 iskandertorres23@gmail.com

El grupo de usuarios de R de Asturias organiza un concurso
de visualización con R de la base de datos school.
https://github.com/grupoRasturias/dataviz-contest-2025/blob/main/bases_es.md

Se busca que el material creado (web, imagen o vídeo):
- Sea fácilmente interpretable.
- El proyecto es original.
- Se adjunta el código empleado debidamente anotado.

Analizo la base de dato school y resumo la información más relevante.
Presento dicha información mediante una animación creada en R.

Esta animación forma parte de un trabajo de investigación
sobre animaciones en analítica deportiva en el que estoy colaborando.
Los primeros resultados se presentan en este congreso
Orden no admisible de escritura aplicado a la docencia en el Grado en Ciencias de la Actividad Física y del Deporte
A. Torres García, S. Díaz Vázquez, E. Torres Manzanera, S. Montes
XLI Congreso Nacional de Estadística e Investigación Operativa,
Lleida, del 10 al 13 de Junio de 2025

Las animaciones buscan reflejar el orden con que el ser humano escribe o dibuja.
Desde un punto de vista matemático, esto es un orden, pues ordena las letras y palabras,
total, pues los órdenes de todas las palabras o letras se pueden comparar unos con otros,
y no admisible, es decir, no sigue un orden Lattice, especialmente cuando
se escriben o dibujas cosas complejas como fórmulas matemáticas o dibujos.

Se ha implementado una relación de orden no admisible en una paquete de R
que permite automatizar la generación de animaciones de diapositivas más o menos
sencillas, como las que se utilizan en la docencia de analítica deportiva.

Los programas informáticos necesarios son:
- ffmpeg: Genera el vídeo.
  Para su instalación, véase https://cran.r-project.org/web/packages/act/vignettes/install_ffmpeg.html
- Paquetes de R: png, remotes, collegeScorecard, visualizacion.

Los pasos seguidos son:
- Análisis de la base de datos.
- Creación de la imagen a animar.
- Ordenación de los pixels de la imagen.
- Creación de los frames del vídeo.
- Uso de ffmpeg para generar el vídeo.

El vídeo editado se puede ver en:
https://www.youtube.com/@alex_torress23

Este código está en
https://github.com/alex_torress23/visualizacion

"
cat(texto,"\n")



texto <- "\n
Instalo los paquetes en caso necesario.

- collegeScorecard: base de datos.
- png: para leer y escribir png.
- visualizacion: para ordenar los pixels.

"
cat(texto,"\n")

if(!require(collegeScorecard)){
    install.packages('collegeScorecard')
    library(collegeScorecard)
}
if(!require(png)){
    install.packages("png")
    library(png)
}
if(!require(remotes)) {
    install.packages("remotes")
}
if(!require(visualizacion)){
    remotes::install_github("emiliotorres/visualizacion")
    library(visualizacion)
}

texto <- "\n
Analizo la base datos school.
Leo la documentación:
- Son 11,300 escuelas.
- Variables identificativas de las escuelas
(id,name,city,state, latitude, longitude, url)
- Resto de variables que analizan los colegios.

¿Cuál es la variable más relevante?

Sigo un criterio muy sencillo. La variable más informativa,
la más relevante, la que más atención se ha dedicado
 para recabar la información, es la que menos datos perdidos tiene.
En este caso resulta la variable 'control', que identifica la naturaleza
del centro. Me informo sobre ella:

‘control’ [factor] Control of institution (IPEDS). (‘CONTROL’)

Y encuentro que para analizar
 Integrated Postsecondary Education Data System (IPEDS)
se afirma que
«This classification ('control') is foundational
 for understanding the structure and dynamics of U.S. higher education.
It categorizes US colleges and universities based on whether they are
 public, private non-profit, or private for-profit,
according to the definitions established by the IPEDS system.
 This factor is essential for understanding
various aspects of higher education, including funding, tuition, governance,
and institutional mission.
When you see this label in a dataset,
you know you're dealing with a crucial piece of information
for analyzing and comparing colleges and universities.»
(según https://lmarena.ai/)

Por lo tanto, esta variable es un factor fundamental en el
análisis de las escuelas universitarias.
"

?school
summary(school)
tt <- table(school[,"control"])
p <- round( 100*table(school[,"control"])/sum( table(school[,"control"])),1)
p


texto <- "\n
La base de datos collegeScorecard::collegeScorecard resumida en una línea.
Se ha creado esta imagen mediante LaTeX con el siguiente texto.
(se puede usar tools::texi2pdf, pero requiere instalar LaTeX en el ordenador.)
"
cat(texto,"\n\n")
latex_vector <- c(
    "\\documentclass[hmargin=0.01cm,vmargin=0cm,head=0.05cm,headsep=0pt,foot=0.05cm,margin=0.01cm,scaleratio=169]{beamer}",
    "\\usepackage[utf8]{inputenc}",
    "\\usecolortheme{cormorant}",
    "\\beamertemplatenavigationsymbolsempty",
    "\\begin{document}",
    "\\begin{frame}",
    "\\[
11,\\,300 \\text{ U.S. Colleges }
\\begin{cases}
  23.3\\% & \\text{Public},\\\\
  24.4\\% & \\text{Nonprofit},\\\\
  52.3\\% & \\text{For-profit}.
\\end{cases}
\\]",
"\\end{frame}",
"\\end{document}\n\n"
)
cat(paste(latex_vector,"\n"),"\n")
content <- readPNG(system.file("images", "prueba50.png", package = "visualizacion"))
plot_array(content) # Un resumen preciso y conciso de la base de datos.



texto <- "\n
Como se trata sobre Escuelas Universitarias,
en todas las aulas hay un profesor y una pizarra.

Creo un decorado basado en esta idea.
Empleo https://lmarena.ai/.\n
"
cat(texto,"\n")
blackboard <- readPNG(system.file("images", "pizarra-paisano-250308.png", package = "visualizacion"))
plot_array(blackboard)

texto <- "\n
Añado la firma.\n
"
cat(texto,"\n")
author <- readPNG(system.file("images", "prueba51.png", package = "visualizacion"))
plot_array(author)





texto <- "\n
Inserto la información dentro de la pizarra.\n
"
cat(texto,"\n")
texto <- "\n
Recortamos la línea.\n
"
cat(texto,"\n")
img <- content
mboundingbox <- obtenerboundingbox(img)
f <- mboundingbox[,"segmento"]>0
ytop <- max(min(mboundingbox[f,"ytop"]) -20,0)
ybottom <- min(max(mboundingbox[f,"ybottom"]) +20,dim(img)[1])
xleft <- max(min(mboundingbox[f,"xleft"]) -20,0)
xright <- min(max(mboundingbox[f,"xright"]) + 20, dim(img)[2])
img <- img[ytop:ybottom,xleft:xright, ]
plot_array(img)
content <- img



texto <- "\n\n
Inserto la información dentro de la pizarra.\n
"
cat(texto,"\n")
segmentos <- obtenersegmentos(blackboard)
n <- prod(dim(blackboard)[1:2])
scale <- dim(content)[1]/dim(content)[2]
imgmodificada <- blackboard
newcols <- c(400,1608)
newrows <- 250 + as.integer(c(0,(newcols[2]-newcols[1] +1) *scale))
f <- segmentos[,"irow"]  %in% c( newrows[1]:newrows[2]) & segmentos[,"jcol"]  %in% c(newcols[1]:newcols[2])
formula1 <- mrescale(content[,,1], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
formula2 <- mrescale(content[,,2], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
formula3 <- mrescale(content[,,3], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
sum(f)-prod(dim(formula1))
pos <- c(1:n)[f]
imgmodificada[pos]  <- formula1
imgmodificada[pos+n]  <- formula2
imgmodificada[pos+2*n]  <- formula3
plot_array(imgmodificada)
img2 <- imgmodificada

texto <- "\n
Hago lo mismo para la firma
"
cat(texto,"\n")
imgmodificada <- img2
content <- author
img <- content
mboundingbox <- obtenerboundingbox(img)
f <- mboundingbox[,"segmento"]>0
ytop <- max(min(mboundingbox[f,"ytop"]) -20,0)
ybottom <- min(max(mboundingbox[f,"ybottom"]) +20,dim(img)[1])
xleft <- max(min(mboundingbox[f,"xleft"]) -20,0)
xright <- min(max(mboundingbox[f,"xright"]) + 20, dim(img)[2])
img <- img[ytop:ybottom,xleft:xright, ]
plot_array(img)
content <- img
segmentos <- obtenersegmentos(imgmodificada)
head(segmentos)
n <- prod(dim(blackboard)[1:2])
scale <- dim(content)[1]/dim(content)[2]
newcols <- c(1400,1650)
newrows <- 775 + as.integer(c(0,(newcols[2]-newcols[1] +1) *scale))
f <- segmentos[,"irow"]  %in% c( newrows[1]:newrows[2]) & segmentos[,"jcol"]  %in% c(newcols[1]:newcols[2])
formula1 <- mrescale(content[,,1], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
formula2 <- mrescale(content[,,2], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
formula3 <- mrescale(content[,,3], nrow= newrows[2]-newrows[1]+1,ncol=newcols[2]-newcols[1]+1)
sum(f)-prod(dim(formula1))
pos <- c(1:n)[f]
imgmodificada[pos]  <- formula1
imgmodificada[pos+n]  <- formula2
imgmodificada[pos+2*n]  <- formula3
plot_array(imgmodificada)



pos3 <- c(1:length(blackboard))[blackboard > 0.10]
imgmodificada[pos3] <- blackboard[pos3]
plot_array(imgmodificada)
img <- imgmodificada


texto <- "\n\n
Ahora viene la parte difícil:
ordenar los pixels.
- Primero ordeno los segmentos.
- Luego ordeno los pixels.
Chequeo que el algoritmo sigue el orden que yo espero.
Para eso,
- dibujo el número de cada segmento.
- compruebo que el algortimo los ordena según yo quiero.
Para un usuario final, esta información es irrelevante.
"
cat(texto,"\n")
mboundingbox <- obtenerboundingbox(img)
f <- mboundingbox[,"pixels"] >0
plot_array(img)
rect(xleft=mboundingbox[f,"xleft"],
     ybottom=dim(img)[1]- mboundingbox[f,"ybottom"],
     xright=mboundingbox[f,"xright"],
     ytop=dim(img)[1]-mboundingbox[f,"ytop"],border="red",lwd=2)
text((mboundingbox[f,"xleft"]), dim(img)[1]- mboundingbox[f,"ybottom"],
     label=mboundingbox[f,"segmento"],cex=1.5,col="yellow")

m <- ordenarpixels(img)

head(m)

texto <- "\n
La parte más difícil ya está hecha.
Ahora toca crear un vídeo.
Un vídeo se compone de diapositivas (frames).
Asigno tiempos:
- 10 segundos para la pizarra.
- 12 segundos para el contenido.
- 13 segundos imagen estática.

En total, 45 segundos.
Menos tiempo no da para quedarse con la información.
Más tiempo genera un vídeo demasiado pesado para enviar
por correo electrónico al concurso.

El vídeo editado se puede ver en
https://www.youtube.com/@alex_torress23
Creo un directorio 'slides50' con todos los fotogramas  (20*25 = 500).
"
cat(texto,"\n")
slide <- 50L
directoriodiapos = "slides"
precision <- 1000L
colortexto=c(898898898,890890890)
colorfondo=98098098
framerate <- 25L
f <- m[,"color"] == colorfondo
m[f,"frame"]  <- 1L
## Pizarra
nseconds <- 10L
f <- m[,"color"]   != colorfondo &  m[,"segmento"] ==1L # color de la pizarra
npixelstext  <-  sum(f)
ntotalframes  <-  nseconds*framerate
sumpixelstext  <- cumsum(f)
textpixelsperframe = (npixelstext/(ntotalframes-1)) ## La primera debería ser negra
m[f,"frame"]  <- 2L +  as.integer(sumpixelstext[f]/(textpixelsperframe))
framesconsumidas <- max(m[,"frame"])
framesconsumidas

## Contenido. 1 frase
nseconds <- 30L
f <- m[,"color"]   != colorfondo &  m[,"segmento"] >1L
npixelstext  <-  sum(f)
ntotalframes  <-  nseconds*framerate
sumpixelstext  <- cumsum(f)
textpixelsperframe = (npixelstext/(ntotalframes-1)) ## La primera debería ser negra
m[f,"frame"]  <- framesconsumidas +  as.integer(sumpixelstext[f]/(textpixelsperframe))

framesconsumidas <- max(m[,"frame"])
## Guardamos cada frame en el directorio
directoriodiaposlide <- paste0(directoriodiapos,slide)
unlink(directoriodiaposlide,recursive=TRUE)
dir.create(directoriodiaposlide)
dims <- c(dim(img)[1],dim(img)[2])
npoints <- prod(dims)
r <- trunc(colorfondo/precision^2)
g <- trunc((colorfondo - r*precision^2)/precision)
b <- (colorfondo- r*precision^2 - g*precision)
mmask <- c(rep(r/precision,npoints),
           rep(g/precision,npoints),
           rep(b/precision,npoints)
           )
dim(mmask) <- dim(img)
f <- m[,"color"] != colorfondo
mfinal <- m[f,c("pos","frame"),drop=FALSE]
## For each frame
maxslide <- max(mfinal[,"frame"])
for(i in 1L:maxslide){
    if(!(i %% 50)) cat(paste0("Slide ", slide, ": faltan ", maxslide-i," fotogramas.\n"))
    filename <- sprintf(file.path(directoriodiaposlide,"/data%04d.png"), i)
    ##print(filename)
    posf <- mfinal[ mfinal[,"frame"] == i, "pos"]
    posred <- posf
    posgreen <- npoints + posf
    posblue <- 2*npoints + posf
    pos <- c(posred,posgreen,posblue)
    mmask[pos]  <- img[pos]
    writePNG(mmask,target=filename)
}




## Añadimos 5 segundos de imagen estática
nseconds <- 5L
for(i in (maxslide + c(1:(framerate*nseconds)))) {
    if(!(i %% 50))cat(paste("Slide", slide, " faltan ",framerate*nseconds+ maxslide-i," fotogramas.\n"))
    filename <- sprintf(file.path(directoriodiaposlide,"/data%04d.png"), i)
    writePNG(img,target=filename)
}


texto <- "
Ya tenemos los 500 fotogramas en el directorio 'slides50'.
Empleamos el programa ffmpeg para crear el vídeo.
Si no está instalado, sale un mensaje de error.
Si está instalado, creo el vídeo y borro los fotogramas.
"
cat(texto,"\n")

if (nzchar(Sys.which("ffmpeg")) == 0) {
    stop("ffmpeg is not installed. Try in your system 'sudo apt install ffmpeg' in Ubuntu, or in MacOs or Windows, see https://cran.r-project.org/web/packages/act/vignettes/install_ffmpeg.html")
}

ficherovideo <- paste0("video-",slide,"-a.mp4")
if(file.exists(ficherovideo))file.remove(ficherovideo)

instruction <- paste0("ffmpeg -f image2  -r ",framerate," -i ",file.path(directoriodiaposlide,"data%04d.png"),"   -vf \"pad=ceil(iw/2)*2:ceil(ih/2)*2\" -vcodec libx264 -crf 18  -pix_fmt yuv420p ",ficherovideo)
cat(instruction,"\n")
system(instruction)

if(!file.exists(ficherovideo)) {
    stop("No encuentro el fichero ",ficherovideo)
} else{
    unlink(directoriodiaposlide,recursive=TRUE)
}

texto <- "\n
Debería existir un vídeo video-50-a.mp4 en este directorio.
A disfrutar de él.
"
cat(texto,"\n")
