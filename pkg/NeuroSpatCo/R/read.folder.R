read.folder<-function () 
{
    leercarpeta <- function() {
        a <- ("1")
        Carpeta1 <- new.env(hash=FALSE)
        require(tcltk)
        ReturnVal <- tkmessageBox(title = "Seleccione Carpeta", 
            message = "Busque y seleccione un archivo cualquiera dentro de la carpeta que contiene todos los archivos a explorar", 
            icon = "info", type = "ok")
        require(tcltk)
        carpeta <- tclvalue(tclfile.dir(tkgetOpenFile(message("Seleccione cualquier archivo del directorio a escoger"))))
#          Carpeta1$carpeta <- carpeta
        if (!nchar(carpeta)) {
            tkmessageBox(message = "No ha seleccionado nada")
        }
        else {
            tkmessageBox(message = paste("La carpeta selecionada fue", 
                carpeta))
        }
        setwd(carpeta)
        archivos <- list.files(path = ".", pattern = ".txt", 
            all.files = FALSE, full.names = FALSE, recursive = FALSE, 
            ignore.case = TRUE)
#          Carpeta1$archivos <- archivos
        k <- a:length(archivos)
        d <- 0
        name <- 0
        for (i in 1:length(k)) {
            d[i] <- paste(carpeta, "/", archivos[i], sep = "")
            name[i] <- c(paste("dat", k[i], sep = ""))
            name[i] <- c(paste(archivos[i], sep = ""))
        }
        dates = 0
        tama <- 1
        data <- 0
        datos <- list()
        for (i in 1:length(k)) {
            assign(name[i], read.table(d[i], header = FALSE, 
                sep = "", na.strings = "NA", dec = ".", strip.white = TRUE, 
                col.names = c("centroidex", "centroidey", "area", 
                  "orientacion", "largo", "ancho")), env = Carpeta1,inherits=FALSE)
        }
	Carpeta1$carpeta <- carpeta
	Carpeta1$archivos <- archivos
        rm(name, datos, dates, data, d, a, i, k, tama)
        return(Carpeta1)
    }

    Carpeta1 <- leercarpeta()
    SALIDA <- list(Carpeta1 = as.list(Carpeta1))
    i = 1
    w <- 0
    while (i <= 9) {
        i = i + 1
        require(tcltk)
        ReturnVal <- tkmessageBox(message = "Quieres leer otra carpeta?", 
            icon = "question", type = "yesnocancel", default = "yes")
        if (as.character(ReturnVal) == "no") {
            esc <- 0
        }
        if (as.character(ReturnVal) == "yes") {
            esc <- 1
        }
        if (as.character(ReturnVal) == "cancel") {
            esc <- 2
        }
        if (esc == 1) {
            if (i == 2) {
                Carpeta2 <- leercarpeta()
                w <- i
            }
            if (i == 3) {
                Carpeta3 <- leercarpeta()
                w <- i
            }
            if (i == 4) {
                Carpeta4 <- leercarpeta()
                w <- i
            }
            if (i == 5) {
                Carpeta5 <- leercarpeta()
                w <- i
            }
            if (i == 6) {
                Carpeta6 <- leercarpeta()
                w <- i
            }
            if (i == 7) {
                Carpeta7 <- leercarpeta()
                w <- i
            }
            if (i == 8) {
                Carpeta8 <- leercarpeta()
                w <- i
            }
        }
        if (esc == 0) {
            require(tcltk)
            ReturnVal <- tkmessageBox(title = "FIN", message = "Se ha terminado la lectura de carpetas", 
                icon = "info", type = "ok")
            i = 10
        }
        if (esc == 2) {
            require(tcltk)
            ReturnVal <- tkmessageBox(title = "FIN", message = "Se cancelo el proceso", 
                icon = "info", type = "ok")
            i = 10
        }
    }
    if (w == 2) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2))
    }
    if (w == 3) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3))
    }
    if (w == 4) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3), Carpeta4 = as.list(Carpeta4))
    }
    if (w == 5) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3), Carpeta4 = as.list(Carpeta4), 
            Carpeta5 = as.list(Carpeta5))
    }
    if (w == 6) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3), Carpeta4 = as.list(Carpeta4), 
            Carpeta5 = as.list(Carpeta5), Carpeta6 = as.list(Carpeta6))
    }
    if (w == 7) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3), Carpeta4 = as.list(Carpeta4), 
            Carpeta5 = as.list(Carpeta5), Carpeta6 = as.list(Carpeta6), 
            Carpeta7 = as.list(Carpeta7))
    }
    if (w == 8) {
        SALIDA <- list(Carpeta1 = as.list(Carpeta1), Carpeta2 = as.list(Carpeta2), 
            Carpeta3 = as.list(Carpeta3), Carpeta4 = as.list(Carpeta4), 
            Carpeta5 = as.list(Carpeta5), Carpeta6 = as.list(Carpeta6), 
            Carpeta7 = as.list(Carpeta7), Carpeta8 = as.list(Carpeta8))
    }
    rm(i, ReturnVal, esc, leercarpeta)
    class(SALIDA) <- "NeuroSpatCo"
    return(SALIDA)
}
