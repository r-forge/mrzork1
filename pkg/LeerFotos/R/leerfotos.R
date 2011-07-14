leerfotos <-
function(){
require(tcltk)
carpeta <- tclvalue(tclfile.dir(tkgetOpenFile(message("seleccione cualquier archivo del directorio")))) # Very simple, isn't it?
if (!nchar(carpeta)) {
    tkmessageBox(message = "No ha seleccionado nada")
} else {
    tkmessageBox(message = paste("La Carpeta selecionada fue", carpeta))
}
setwd(carpeta)
archivos<-list.files(path = ".", pattern = ".JPG", all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = TRUE)
##
datos2 <- data.frame()

for(i in 1:length(archivos)){
datos<-0
datos<-cbind(archivos[i],eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:ExposureTime] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:ApertureValue] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:FocalLength] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:ShutterSpeedValue] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:ISOSpeedRatings] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:Flash] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))),eval(parse(text=paste(system(paste(Sys.getenv("ImageMagick"),  "identify -format %[EXIF:FNumber] \"", archivos[i], "\"", sep = ""),  intern = TRUE)
))))
if(datos[7]==16){datos[7]<-"Flash did not fire, compulsory flash mode"}
if(datos[7]==9){datos[7]<-"Flash fired, compulsory flash mode"}
datos2<-rbind(datos2,datos)
}
colnames(datos2)<-c("Nombre Imagen","Tiempo de Exposici<U+00F3>n","Apertura de Diafragma","Distancia Focal","Velocidad Obturador","Iso","Flash","EV")
write.csv ( x =  datos2 , file =  "data.csv" , append =  TRUE  ,quote = TRUE ,  sep = ';' , eol = "\n" , na = "NA" ,  dec = ',' , row.names =  TRUE ,  col.names =  TRUE , qmethod=  'escape' ) 
}

