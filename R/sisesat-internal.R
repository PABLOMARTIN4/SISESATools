# CREAR CARPETAS
createFolder <- function(directorio, nombre,...){
  
  for(i in 1:length(nombre)){
    folder <- nombre[i]
    subDir <- paste("dat", folder, sep = "")
    dir.create(file.path(directorio, subDir))
  }
  return(invisible(NULL))
}

# falta el .getData como insumo de baseBoat01
# separa los viajes 2000-2015 insumo de baseBoat02
.getData2 <- function(data){
  
  #data <- listaBarco[[1]]
  #data$DATACION2      <- data$DATACION # uniformiza fecha
  data$DATACION2      <- strptime(as.character(data$DATACION),format = "%d/%m/%Y %H:%M")
  #print(data$DATACION2)
  data                <- data[order(data$DATACION2),] # ordenamos la data
  
  distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)# calculamos la distancia a puerto        
  distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
  distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
  distancia0 <- as.vector(distancia0)
  
  distancia <- rep(distancia0,rep(length(data$CODIGO),length(distancia0)))
  dim(distancia) <- c(length(data$CODIGO),dim(caletas)[1]*2)
  
  distanciaTotal <- NULL
  
  for(lonp in seq(1,nrow(caletas)*2,by=2)){
    distanciaPuerto <-  distORTODROMICA(data$X,data$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
    distanciaTotal  <- cbind(distanciaTotal,distanciaPuerto)        
  }
  
  distanciaTotal        <- data.frame(distanciaTotal)
  data$puerto           <- apply(distanciaTotal,1,which.min)
  data$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
  
  data$horaEmision      <- NA
  data$distanciaEmision <- NA 
  data$velocidadEmision <- NA
  data$diferenciaRUMBO  <- NA
  data$compVelocidad    <- NA
  
  data$horaEmision[2:(length(data[,1]))] <- (julian(data$DATACION2[2:(length(data[,1]))])-julian(data$DATACION2[1:(length(data[,1])-1)]))*24
  data$distanciaEmision[2:(length(data[,1]))] <- distORTODROMICA(data$X[1:(length(data[,1])-1)],data$Y[1:(length(data[,1])-1)],data$X[2:length(data[,1])],data$Y[2:length(data[,1])])    
  data$velocidadEmision <- data$distanciaEmision/data$horaEmision
  data$diferenciaRUMBO  <- calcularRumbo(data$X,data$Y)#$vectorRUMBO
  data$cambioRUMBO      <- apply(matrix(data$diferenciaRUMBO),1,modificarRumbo)  
  data$compVelocidad    <- data$VELOCIDAD - round(data$velocidadEmision,1)   
  #}    
  
  return(data)
}
# falta modificar esta parte
# baseboatbarco 2000-2009 ---------------------------------------------

baseBoat <- function(directorio, year, caletas, ... ){#caletas
  
  for(i in unique(year)){
    
    archivos <- NULL
    datYear <-NULL
    
    totalFantasmas <- NULL
    totalEmisiones <- NULL
    distanciaTotal <- NULL
    dat <- NULL
    
    setwd(file.path(directorio,i)) ##direciona a la carpeta anho
    archivos <- dir()
#    setwd("E:/vms_prueba_tesis/2000dat/")
#    strsplit(archivos[1], ".csv", fixed=TRUE)
    
    for(j in 1:length(archivos)){
      
      dat <- read.dbf(archivos[j]) ##lee cada archivo dbf   
      #fprint(dat)
      datYear <- rbind(datYear,dat)     
    }  
    
    emb=lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2 <- unlist(emb) 
    
    for(barco in unique(datYear$EMBARCACIO2)){
      
      datBarco <- datYear[datYear$EMBARCACIO2 == barco,]
      datBarco$DATACION2 <- modTime(datBarco$DATACION) 
      datBarco <- datBarco[order(datBarco$DATACION2),]
      
      barcoFantasma <- table(datBarco$NUMERO_EMB) 
      barcoBueno <- dim(datBarco)[1]
      
      if(length(barcoFantasma) > 1){ ## quitamos los barcos fantasmas
        datBarco <-  datBarco[datBarco$NUMERO_EMB == as.numeric(names(which.max(barcoFantasma))),]                        
      }        
      #datBarco <- data[,1:12]
      #datBarco$DATACION2 <- modTime(datBarco$DATACION)#strptime(as.character(data$DATACION),format = "%d/%m/%Y %H:%M")
      if(barcoBueno > 10){
        
        distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)        
        distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
        distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
        distancia0 <- as.vector(distancia0)
        
        distancia      <- rep(distancia0,rep(length(datBarco$NUMERO_EMB),length(distancia0)))
        dim(distancia) <- c(length(datBarco$NUMERO_EMB),dim(caletas)[1]*2)
        
        distanciaTotal <- NULL
        
        for(lonp in seq(1,nrow(caletas)*2,by=2)){
          distanciaPuerto         <-  distORTODROMICA(datBarco$X,datBarco$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
          distanciaTotal          <- cbind(distanciaTotal,distanciaPuerto)        
        }
        
        distanciaTotal            <- data.frame(distanciaTotal)
        datBarco$puerto           <- apply(distanciaTotal,1,which.min)
        datBarco$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
        
        datBarco$horaEmision      <- NA
        datBarco$horaEmision[2:(length(datBarco[,1]))] <- (julian(datBarco$DATACION2[1:(length(datBarco[,1])-1)])-julian(datBarco$DATACION2[2:(length(datBarco[,1]))]))*24*(-1)
        
        datBarco$distanciaEmision <- NA 
        datBarco$distanciaEmision[2:(length(datBarco[,1]))] <- distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])])
        
        datBarco$velocidadEmision <- NA
        datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
        
        datBarco$change.speed.1  <- NA
        datBarco$change.speed.2  <- NA
        datBarco$change.speed.1[-1]  <- diff(datBarco$velocidadEmision)
        datBarco$change.speed.2[-length(datBarco$change.speed.1)]  <- diff(datBarco$velocidadEmision)
        #datBarco$change.speed.2[-1]  <- rev(diff(rev(datBarco$velocidadEmision)))
                
        datBarco$diferenciaRUMBO  <- NA
        datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
        datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
        
        datBarco$compVelocidad        <- NA
        datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
        
        datBarco$angle  <- NA
        datBarco$angle[2:(length(datBarco$angle)-1)]  <- estimateAngle(datBarco$X, datBarco$Y)
        
        datBarco$cambio.angle.1      <- NA
        datBarco$cambio.angle.2      <- NA
        datBarco$cambio.angle.1[2:length(datBarco$angle)]     <- rev(diff(rev(datBarco$angle)))
        datBarco$cambio.angle.2[1:(length(datBarco$angle)-1)] <- rev(diff(rev(datBarco$angle)))
        
        carpeta <- paste("dat",substring(datBarco$DATACION[1], 7, 10), sep = "")
        # print(carpeta)
        # setwd(paste(directorio,"/",carpeta,sep = ""))
        #write.csv(datBarco, paste(datBarco$CODIGO[1],".csv",sep = ""))
        #write.csv(datBarco, file = paste0(directorio,"/",carpeta,"/",barco,".csv")) 
        write.csv(datBarco, paste(barco, ".csv", sep = "")) 
        #write.csv(datBarco, file = paste(directorio,"/",carpeta,"/",barco,".csv", sep = ""))
      }
      
    }   
    #write.csv(datBarco, paste(barco,".csv", sep = ""))      
  }  
  return(invisible(NULL))
}
#########################################

# baseBoat2 <- function(directorio, year, caletas, ... ){#caletas
#   
#   for(i in unique(year)){
#     
#     archivos <- NULL
#     datYear <-NULL
#     
#     totalFantasmas <- NULL
#     totalEmisiones <- NULL
#     distanciaTotal <- NULL
#     dat <- NULL
#     
#     setwd(file.path(directorio,i)) ##direciona a la carpeta anho
#     archivos <- dir()
#     #    setwd("E:/vms_prueba_tesis/2000dat/")
#     #    strsplit(archivos[1], ".csv", fixed=TRUE)
#     
#     for(j in 1:length(archivos)){
#       
#       dat <- read.csv(archivos[j]) ##lee cada archivo dbf   
#       #fprint(dat)
#       datYear <- rbind(datYear,dat)     
#     }  
#     
#     emb=lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
#     datYear$EMBARCACIO2 <- unlist(emb) 
#     
#     for(barco in unique(datYear$EMBARCACIO2)){
#       
#       datBarco <- datYear[datYear$EMBARCACIO2 == barco,]
#       datBarco$DATACION2 <- modTime(datBarco$DATACION) 
#       datBarco <- datBarco[order(datBarco$DATACION2),]
#       
#       barcoFantasma <- table(datBarco$NUMERO_EMB) 
#       barcoBueno <- dim(datBarco)[1]
#       
#       if(length(barcoFantasma) > 1){ ## quitamos los barcos fantasmas
#         datBarco <-  datBarco[datBarco$NUMERO_EMB == as.numeric(names(which.max(barcoFantasma))),]                        
#       }        
#       #datBarco <- data[,1:12]
#       #datBarco$DATACION2 <- modTime(datBarco$DATACION)#strptime(as.character(data$DATACION),format = "%d/%m/%Y %H:%M")
#       if(barcoBueno > 10){
#         
#         distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)        
#         distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
#         distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
#         distancia0 <- as.vector(distancia0)
#         
#         distancia      <- rep(distancia0,rep(length(datBarco$NUMERO_EMB),length(distancia0)))
#         dim(distancia) <- c(length(datBarco$NUMERO_EMB),dim(caletas)[1]*2)
#         
#         distanciaTotal <- NULL
#         
#         for(lonp in seq(1,nrow(caletas)*2,by=2)){
#           distanciaPuerto         <-  distORTODROMICA(datBarco$X,datBarco$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
#           distanciaTotal          <- cbind(distanciaTotal,distanciaPuerto)        
#         }
#         
#         distanciaTotal            <- data.frame(distanciaTotal)
#         datBarco$puerto           <- apply(distanciaTotal,1,which.min)
#         datBarco$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
#         
#         datBarco$horaEmision      <- NA
#         datBarco$horaEmision[2:(length(datBarco[,1]))] <- (julian(datBarco$DATACION2[1:(length(datBarco[,1])-1)])-julian(datBarco$DATACION2[2:(length(datBarco[,1]))]))*24*(-1)
#         
#         datBarco$distanciaEmision <- NA 
#         datBarco$distanciaEmision[2:(length(datBarco[,1]))] <- distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])])
#         
#         datBarco$velocidadEmision <- NA
#         datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
#         
#         datBarco$change.speed.1  <- NA
#         datBarco$change.speed.2  <- NA
#         datBarco$change.speed.1[-1]  <- diff(datBarco$velocidadEmision)
#         datBarco$change.speed.2[-length(datBarco$change.speed.1)]  <- diff(datBarco$velocidadEmision)
#         #datBarco$change.speed.2[-1]  <- rev(diff(rev(datBarco$velocidadEmision)))
#         
#         datBarco$diferenciaRUMBO  <- NA
#         datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
#         datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
#         
#         datBarco$compVelocidad        <- NA
#         datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
#         
#         datBarco$angle  <- NA
#         datBarco$angle[2:(length(datBarco$angle)-1)]  <- estimateAngle(datBarco$X, datBarco$Y)
#         
#         datBarco$cambio.angle.1      <- NA
#         datBarco$cambio.angle.2      <- NA
#         datBarco$cambio.angle.1[2:length(datBarco$angle)]     <- rev(diff(rev(datBarco$angle)))
#         datBarco$cambio.angle.2[1:(length(datBarco$angle)-1)] <- rev(diff(rev(datBarco$angle)))
#         
#         carpeta <- paste("dat",substring(datBarco$DATACION[1], 7, 10), sep = "")
#         # print(carpeta)
#         # setwd(paste(directorio,"/",carpeta,sep = ""))
#         #write.csv(datBarco, paste(datBarco$CODIGO[1],".csv",sep = ""))
#         #write.csv(datBarco, file = paste0(directorio,"/",carpeta,"/",barco,".csv")) 
#         write.csv(datBarco, paste(barco, ".csv", sep = "")) 
#         #write.csv(datBarco, file = paste(directorio,"/",carpeta,"/",barco,".csv", sep = ""))
#       }
#       
#     }   
#     #write.csv(datBarco, paste(barco,".csv", sep = ""))      
#   }  
#   return(invisible(NULL))
# }

####################################################

# baseboatmatricula 2010-2015 ---------------------------------------------
baseBoat2 <- function(directorio, year, caletas, ... ){#caletas
  for(i in unique(year)){
    carpeta = paste("")
    archivos       <- NULL
    datYear        <- NULL
    totalFantasmas <- NULL
    totalEmisiones <- NULL
    distanciaTotal <- NULL
    dat            <- NULL
    setwd(paste(directorio, "/", i, sep = "")) ##direciona a la carpeta anho
    archivos <- dir()
    if(length(archivos)>1){
      for(j in 1:length(archivos)){   
        dat <- read.csv(archivos[j], stringsAsFactors = F,encoding = "utf-8") ##lee cada archivo dbf   
        dat <- dat[dat$X < 0,]
        datYear <- rbind(datYear,dat)
      }  
    } else {
      datYear <- read.csv(archivos)
    }
    emb=lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2<- unlist(emb) 
    MATRICULA <- as.data.frame(datYear$MATRICULA)
    CODIGO <- apply(MATRICULA, 1, extrae.numero)
    CODIGO <- as.numeric(CODIGO)
    datYear$CODIGO <- CODIGO
    
    for(barco in unique(datYear$CODIGO)){
      #barco <- 21260 
      datBarco           <- datYear[datYear$CODIGO == barco,]
      datBarco$DATACION2 <- modTime(datBarco$DATACION) 
      datBarco           <- datBarco[order(datBarco$DATACION2),]
      
      #barcoFantasma <- table(datBarco$barc) 
      barcoBueno <- dim(datBarco)[1]      
      #     if(length(barcoFantasma) > 1){ ## quitamos los barcos fantasmas
      #       datBarco <-  datBarco[datBarco$NUMERO_EMB == as.numeric(names(which.max(barcoFantasma))),]                        
      #     }        
      if(barcoBueno > 10){
        
        distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)        
        distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
        distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
        distancia0 <- as.vector(distancia0)
        
        distancia      <- rep(distancia0,rep(length(datBarco[,1]),length(distancia0)))
        dim(distancia) <- c(length(datBarco[,1]),dim(caletas)[1]*2)
        
        distanciaTotal <- NULL
        
        for(lonp in seq(1,nrow(caletas)*2,by=2)){
          distanciaPuerto <-  distORTODROMICA(datBarco$X,datBarco$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
          distanciaTotal  <- cbind(distanciaTotal,distanciaPuerto)        
        } 
        distanciaTotal            <- data.frame(distanciaTotal)
        datBarco$puerto           <- apply(distanciaTotal,1,which.min)
        datBarco$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
        
        datBarco$horaEmision      <- NA
        datBarco$horaEmision[2:(length(datBarco[,1]))] <- (julian(datBarco$DATACION2[1:(length(datBarco[,1])-1)])-julian(datBarco$DATACION2[2:(length(datBarco[,1]))]))*24*(-1)
        
        datBarco$distanciaEmision <- NA 
        datBarco$distanciaEmision[2:(length(datBarco[,1]))] <- distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])])
        
        datBarco$velocidadEmision <- NA
        datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
        
        datBarco$change.speed.1  <- NA
        datBarco$change.speed.2  <- NA
        datBarco$change.speed.1[-1]  <- diff(datBarco$velocidadEmision)
        datBarco$change.speed.2[-length(datBarco$change.speed.1)]  <- diff(datBarco$velocidadEmision)
        #datBarco$change.speed.2[-1]  <- rev(diff(rev(datBarco$velocidadEmision)))
        
        datBarco$diferenciaRUMBO  <- NA
        datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
        datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
        
        datBarco$compVelocidad        <- NA
        datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
        
        datBarco$angle  <- NA
        datBarco$angle[2:(length(datBarco$angle)-1)]  <- estimateAngle(datBarco$X, datBarco$Y)
        
        datBarco$cambio.angle.1      <- NA
        datBarco$cambio.angle.2      <- NA
        datBarco$cambio.angle.1[2:length(datBarco$angle)]     <- rev(diff(rev(datBarco$angle)))
        datBarco$cambio.angle.2[1:(length(datBarco$angle)-1)] <- rev(diff(rev(datBarco$angle)))
        
        carpeta <- paste("dat",substring(datBarco$DATACION[1], 7, 10), sep = "")
        # print(carpeta)
        # setwd(paste(directorio,"/",carpeta,sep = ""))
        #write.csv(datBarco, paste(datBarco$CODIGO[1],".csv",sep = ""))
        write.csv(datBarco, file = paste0(directorio,"/",carpeta,"/",barco,".csv")) 
        #write.csv(datBarco, paste(barco, ".csv", sep = "")) 
        #write.csv(datBarco, file = paste(directorio,"/",carpeta,"/",barco,".csv", sep = ""))
      }
    }        
  }  
  return(invisible(NULL))
}

# IDENTIFICA LOS VIAJES
calcularViaje <- function(directorio, year, ...){
  
  archivos <- NULL
  data <- NULL
  
  for(i in unique(year)){
    setwd(paste(directorio, "/","dat",i, sep = "")) ##direciona a la carpeta anho    
    archivos <- dir()  
    
    for(j in 1:length(archivos)){
      data <- read.csv(archivos[j])
      data <- data[order(data$DATACION2),] # mod 15052015
      data_new <- .idViaje2(data)   
      carpeta <- paste("viaje",i,"dat",sep = "")
      write.csv(data_new, file = paste0(directorio,"/",carpeta,"/",archivos[j]))
    }
  }
  return(invisible(NULL))
}


# IDENTIFICA LOS VIAJES
# calcularViaje2016 <- function(directorio, year, ...){
#   
#   archivos <- NULL
#   data <- NULL
#   
#   for(i in unique(year)){
#     setwd(paste(directorio, "/","dat",i, sep = "")) ##direciona a la carpeta anho    
#     archivos <- dir()  
#     
#     for(j in 1:length(archivos)){
#       data <- read.csv(archivos[j])
#       data <- data[order(data$DATACION2),] # mod 15052015
#       data_new <- .identificarViajes(data)   
#       carpeta <- paste("viaje",i,"dat",sep = "")
#       write.csv(data_new, file = paste0(directorio,"/",carpeta,"/",archivos[j]))
#     }
#   }
#   return(invisible(NULL))
# }

##
separaBarcos2016 = function(directorio, year, caletas, ... ){#caletas
  for(i in unique(year)){
    
    archivos       <- NULL
    datYear        <- NULL
    totalFantasmas <- NULL
    totalEmisiones <- NULL
    distanciaTotal <- NULL
    dat            <- NULL
    directorio2 = file.path(directorio,i)
    setwd(directorio2) ##direciona a la carpeta anho
    archivos <- dir()
    if(length(archivos)>1){
      for(j in 1:length(archivos)){   
        dat <- read.csv(archivos[j], stringsAsFactors = F,encoding = "utf-8") ##lee cada archivo dbf   
        dat <- dat[dat$X < 0,]
        datYear <- rbind(datYear,dat)
      }  
    } else {
      datYear <- read.csv(archivos)
    }
    #
    datYear = read.csv("data.csv")
    emb=lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2 <- unlist(emb) 
    MATRICULA <- as.data.frame(datYear$MATRICULA)
    CODIGO <- apply(MATRICULA, 1, extrae.numero)
    CODIGO <- as.numeric(CODIGO)
    datYear$CODIGO <- CODIGO
    #emisionCODIGO <- table(datYear$CODIGO)
    
    #write.csv(emisionCODIGO, file.path(directorio,i,"estadisticas","emisiones.csv"))
    
    for(barco in unique(datYear$CODIGO)){
      #barco <- 21260 
      print(barco)
      datBarco           <- datYear[datYear$CODIGO == barco,]
      datBarco$DATACION2 <- as.POSIXct(strptime(as.character(datBarco$DATACION), format = "%Y-%m-%d %H:%M"))
      #datBarco$DATACION2 <- modTime(datBarco$DATACION) 
      datBarco           <- datBarco[order(datBarco$DATACION2),]
      
      #barcoFantasma <- table(datBarco$barc) 
      barcoBueno <- dim(datBarco)[1]      
      #     if(length(barcoFantasma) > 1){ ## quitamos los barcos fantasmas
      #       datBarco <-  datBarco[datBarco$NUMERO_EMB == as.numeric(names(which.max(barcoFantasma))),]                        
      #     }        
      if(barcoBueno > 10){
        
        distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)        
        distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
        distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
        distancia0 <- as.vector(distancia0)
        
        distancia      <- rep(distancia0,rep(length(datBarco[,1]),length(distancia0)))
        dim(distancia) <- c(length(datBarco[,1]),dim(caletas)[1]*2)
        
        distanciaTotal <- NULL
        
        for(lonp in seq(1,nrow(caletas)*2,by=2)){
          distanciaPuerto <-  distORTODROMICA(datBarco$X,datBarco$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
          distanciaTotal  <- cbind(distanciaTotal,distanciaPuerto)        
        } 
        distanciaTotal            <- data.frame(distanciaTotal)
        datBarco$puerto           <- apply(distanciaTotal,1,which.min)
        datBarco$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
        
        datBarco$horaEmision      <- NA
        datBarco$horaEmision[2:(length(datBarco[,1]))] <- (julian(datBarco$DATACION2[1:(length(datBarco[,1])-1)])-julian(datBarco$DATACION2[2:(length(datBarco[,1]))]))*24*(-1)
        
        datBarco$distanciaEmision <- NA 
        datBarco$distanciaEmision[2:(length(datBarco[,1]))] <- distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])])
        
        datBarco$velocidadEmision <- NA
        datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
        
        datBarco$change.speed.1  <- NA
        datBarco$change.speed.2  <- NA
        datBarco$change.speed.1[-1]  <- diff(datBarco$velocidadEmision)
        datBarco$change.speed.2[-length(datBarco$change.speed.1)]  <- diff(datBarco$velocidadEmision)
        #datBarco$change.speed.2[-1]  <- rev(diff(rev(datBarco$velocidadEmision)))
        
        datBarco$diferenciaRUMBO  <- NA
        datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
        datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
        
        datBarco$compVelocidad        <- NA
        datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
        
        datBarco$angle  <- NA
        datBarco$angle[2:(length(datBarco$angle)-1)]  <- estimateAngle(datBarco$X, datBarco$Y)
        
        datBarco$cambio.angle.1      <- NA
        datBarco$cambio.angle.2      <- NA
        datBarco$cambio.angle.1[2:length(datBarco$angle)]     <- rev(diff(rev(datBarco$angle)))
        datBarco$cambio.angle.2[1:(length(datBarco$angle)-1)] <- rev(diff(rev(datBarco$angle)))
        
        carpeta <- paste("dat",i, sep = "")
        # print(carpeta)
        # setwd(paste(directorio,"/",carpeta,sep = ""))
        #write.csv(datBarco, paste(datBarco$CODIGO[1],".csv",sep = ""))
        #write.csv(datBarco, file = paste0(directorio,"/",carpeta,"/",barco,".csv")) 
        write.csv(datBarco, file.path(directorio,carpeta,paste(barco,".csv", sep =""))) 
        #write.csv(datBarco, file = paste(directorio,"/",carpeta,"/",barco,".csv", sep = ""))
      }
    }        
  }  
  return(invisible(NULL))
}






## realizado 04/08/2015
calcularViaje2 <- function(directorio, year, ...){
  
  archivos <- NULL
  data <- NULL
  
  for(i in unique(year)){
    setwd(paste(directorio, "/","dat",i, sep = "")) ##direciona a la carpeta anho    
    archivos <- dir()  
    
    for(j in 1:length(archivos)){
      data <- read.csv(archivos[j])
      data <- data[order(data$DATACION2),] # mod 15052015
      data_new <- .newidViaje2(data)   # se genero .newidViaje
      carpeta <- paste("viaje",i,"dat",sep = "")
      write.csv(data_new, file = paste0(directorio,"/",carpeta,"/",archivos[j]))
    }
  }
  return(invisible(NULL))
}

# IDENTIFICA EL VIAJE E INCORPORA EL MULTINUCLEO
calcularViajeMN <- function(directorio, year, nodes = 6, ...){
  archivos <- NULL
  data     <- NULL
    
  for(y in unique(year)){
    setwd(paste(directorio, "/","dat",y, sep = "")) ##direciona a la carpeta anho    
    archivos      <- dir()  
    listaArchivos <- NULL
    carpeta       <-  paste("viaje",y,"dat",sep = "")  
  
    for(i in seq_along(archivos)){
      data               <- read.csv(archivos[i])
      data2              <- data[order(data$DATACION2),]
      listaArchivos[[i]] <- data2
    }
    
    #system.time({
    cl = makeCluster(nodes, type = "SOCK")
    registerDoSNOW(cl)
    
    cluster  = foreach(l = seq_along(listaArchivos), .inorder = FALSE) %dopar% {    
      .idViaje2(listaArchivos[[l]])        
    }  
    stopCluster(cl)
    # })  
    
    for(w in seq_along(cluster)){     
      write.csv(cluster[[w]], file = paste0(directorio,"/",carpeta,"/",paste(cluster[[w]]$EMBARCACIO2[1], ".csv", sep = ""))) 
    }  
  }
  
  return(invisible(NULL))
}

## CALCULAR VIAJE 2010-2015

calcularViajeMN2010_2015 <- function(directorio, year, nodes = 6, ...){
  archivos <- NULL
  data     <- NULL
  
  for(y in unique(year)){
    setwd(paste(directorio, "/","dat",y, sep = "")) ##direciona a la carpeta anho    
    archivos      <- dir()  
    listaArchivos <- NULL
    carpeta       <-  paste("viaje",y,"dat",sep = "")  
    
    for(i in seq_along(archivos)){
      data               <- read.csv(archivos[i])
      data2              <- data[order(data$DATACION2),]
      listaArchivos[[i]] <- data2
    }
    
    #system.time({
    cl = makeCluster(nodes, type = "SOCK")
    registerDoSNOW(cl)
    
    cluster  = foreach(l = seq_along(listaArchivos), .inorder = FALSE) %dopar% {    
      .idViaje2(listaArchivos[[l]])        
    }  
    stopCluster(cl)
    # })  
    
    for(w in seq_along(cluster)){     
      write.csv(cluster[[w]], file = paste0(directorio,"/",carpeta,"/",paste(cluster[[w]]$CODIGO[1], ".csv", sep = ""))) 
    }  
  }  
  return(invisible(NULL))
}

# 19/01/2016 MODICACION DE LA FUNCION CALCULAR VIAJE PARA 2010-2016 -------
# calcularViajeULT <- function(directorio, year, nodes = 6, ...){
#   
#   archivos <- NULL
#   data     <- NULL
#   
#   for(y in unique(year)){
#     setwd(paste(directorio, "/","dat",y, sep = "")) ##direciona a la carpeta anho    
#     archivos      <- dir()  
#     listaArchivos <- NULL
#     carpeta       <-  paste("viaje",y,"dat",sep = "")  
#     
#     for(i in seq_along(archivos)){
#       data               <- read.csv(archivos[i])
#       data2              <- data[order(data$DATACION2),]
#       listaArchivos[[i]] <- data2
#     }
#     
#     #system.time({
#     cl = makeCluster(nodes, type = "SOCK")
#     registerDoSNOW(cl)
#     
#     cluster  = foreach(l = seq_along(listaArchivos), .inorder = FALSE) %dopar% {    
#       .identificarViajes(listaArchivos[[l]])        
#     }  
#     stopCluster(cl)
#     # })  
#     
#     for(w in seq_along(cluster)){     
#       write.csv(cluster[[w]], file = paste0(directorio,"/",carpeta,"/",paste(cluster[[w]]$EMBARCACIO2[1], ".csv", sep = ""))) 
#     }  
#   }  
#   return(invisible(NULL))
# }

## carga las base por barcos y limpia los viaje no identificados -
datviaje <- function(directorio, year, ...){  
  for(y in unique(year)){
    archivos <- NULL
    datviaje <-NULL
    
    setwd(paste(directorio, "/", "viaje", y,"dat", sep = "")) ##direciona a la carpeta anho
    archivos <- dir()
    archivos <- archivos[1]
    
    for(barco in 1:length(archivos)){
      #for(barco in sample(1:length(archivos), 20)){    
      dat <- read.csv(archivos[barco])
      datviaje <- rbind(datviaje,dat)     
    }
    
  }
  datviaje <- datviaje[!is.na(datviaje$viaje) & datviaje$tipo_viaje == 1,]
  return(datviaje)
}

# falta el baseBoat01
baseBoat01 <- function(directorio, year, caletas, nodes, ... ){
}
# baseBoat02 separa las bases de cada barco 2010-2015 ---------------------

baseBoat02 <- function(directorio, year, caletas, nodes, ... ){#caletas
#   # formato rumbo
#   newRumbo <- function(x){
#     x[!is.na(x) & x>180] <- x[!is.na(x) & x>180]-360 
#     return(x)
#   }
#   
#   modificarRumbo <- function(x){
#     out <- min(x,360-x)
#     out[is.infinite(out)] <- NA
#     return(out)
#   }
#   
#   calcularRumbo <- function(X,Y){
#     vectorRumbo <- NULL 
#     vectorRumbo[1] <- NA
#     for(i in 2:length(X)){
#       x2 <-  X[i]    
#       y2 <-  Y[i]  
#       x1 <-  X[i-1]  
#       y1 <-  Y[i-1]  
#       y   <- sin((x2-x1)*pi/180) * cos(y2*pi/180)
#       x   <- cos(y1*pi/180) * sin(y2*pi/180) - sin(y1*pi/180) * cos(y2*pi/180) * cos((x2-x1)*pi/180)
#       Rumbo <- atan2(y,x)*180/pi
#       Rumbo <- (Rumbo + 360)%%360
#       vectorRumbo <- rbind(vectorRumbo,Rumbo)
#     }
#     vectorRumbo <- as.vector(vectorRumbo)
#     return(vectorRumbo)#, cambioRumbo = cambioRumbo))
#   } 
#   
#   distORTODROMICA <- function (x1,y1,x2,y2) 
#   {
#     lon.mn1 = -x1 * 60 * cos(-y1 * pi/180)
#     lon.mn2 = -x2 * 60 * cos(-y2 * pi/180)
#     lat.mn1 = -y1 * 60
#     lat.mn2 = -y2 * 60
#     out = sqrt((lon.mn1 - lon.mn2)^2 + (lat.mn1 - lat.mn2)^2)
#     return(out)
#   }
#   .getData2 <- function(data){
#     
#     #data <- listaBarco[[1]]
#     #data$DATACION2      <- data$DATACION # uniformiza fecha
#     data$DATACION2      <- strptime(as.character(data$DATACION),format = "%d/%m/%Y %H:%M")
#     #print(data$DATACION2)
#     data                <- data[order(data$DATACION2),] # ordenamos la data
#     
#     distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)# calculamos la distancia a puerto        
#     distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
#     distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
#     distancia0 <- as.vector(distancia0)
#     
#     distancia <- rep(distancia0,rep(length(data$CODIGO),length(distancia0)))
#     dim(distancia) <- c(length(data$CODIGO),dim(caletas)[1]*2)
#     
#     distanciaTotal <- NULL
#     
#     for(lonp in seq(1,nrow(caletas)*2,by=2)){
#       distanciaPuerto <-  distORTODROMICA(data$X,data$Y,distancia[,lonp],distancia[,lonp+1])  # Ortodromica ??
#       distanciaTotal  <- cbind(distanciaTotal,distanciaPuerto)        
#     }
#     
#     distanciaTotal        <- data.frame(distanciaTotal)
#     data$puerto           <- apply(distanciaTotal,1,which.min)
#     data$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen
#     
#     data$horaEmision      <- NA
#     data$distanciaEmision <- NA 
#     data$velocidadEmision <- NA
#     data$diferenciaRUMBO  <- NA
#     data$compVelocidad    <- NA
#     
#     data$horaEmision[2:(length(data[,1]))] <- (julian(data$DATACION2[2:(length(data[,1]))])-julian(data$DATACION2[1:(length(data[,1])-1)]))*24
#     data$distanciaEmision[2:(length(data[,1]))] <- distORTODROMICA(data$X[1:(length(data[,1])-1)],data$Y[1:(length(data[,1])-1)],data$X[2:length(data[,1])],data$Y[2:length(data[,1])])    
#     data$velocidadEmision <- data$distanciaEmision/data$horaEmision
#     data$diferenciaRUMBO  <- calcularRumbo(data$X,data$Y)#$vectorRUMBO
#     data$cambioRUMBO      <- apply(matrix(data$diferenciaRUMBO),1,modificarRumbo)  
#     data$compVelocidad    <- data$VELOCIDAD - round(data$velocidadEmision,1)   
#     #}    
#     
#     return(data)
#   }  # for de matricula y convertirlo a lista
  for(i in seq_along(year)){
    
    listaBarco <- NULL
    datYear    <- NULL
    carpeta    <- paste("dat", year[i], sep = "")          
    setwd(file.path(directorio, year[i])) ##direciona a la carpeta anho
    archivos   <- dir()
    
    for(j in seq_along(archivos)){      
      dat <- read.csv(archivos[j]) ##lee cada archivo csv
      datYear <- rbind(datYear, dat)
    } 
    
    emb=lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2 <- unlist(emb) 
    MATRICULA           <- as.data.frame(datYear$MATRICULA)
    CODIGO              <- apply(MATRICULA, 1, extrae.numero)
    datYear$CODIGO      <- as.numeric(CODIGO)
    
    tabCodigo <-  table(datYear$CODIGO)   
    barco     <- sort(as.numeric(names(tabCodigo[tabCodigo>10])))
    #barco <- sort(unique(datYear$CODIGO))    
    #print(barco)
    
    for(m in seq_along(barco)){
      listaBarco[[m]] <- datYear[datYear$CODIGO == barco[m],]  
    }
    #     for(ll in seq_along(listaBarco)){
    #       print(dim(listaBarco[[ll]]))  
    #     }
    
    #print(listaBarco[[m]])
    cl = makeCluster(nodes, type = "SOCK")
    registerDoSNOW(cl)
    
    cluster  = foreach(l = seq_along(listaBarco), .inorder = FALSE) %dopar% {    
      .getData2(listaBarco[[l]])        
    }
    stopCluster(cl)
    
    for(w in seq_along(cluster)){     
      write.csv(cluster[[w]], file = paste0(directorio,"/",carpeta,"/",paste(cluster[[w]]$CODIGO[1], ".csv", sep = ""))) 
    }
  }
  return(invisible(NULL))
}  
