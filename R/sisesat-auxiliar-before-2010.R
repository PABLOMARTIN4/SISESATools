# PARA 2000-2009
readVMSvesselBefore <- function(directorio, year, caletas, ... ){#caletas
  require(foreign)
  
  for(i in unique(year)){
    
    archivos       <- NULL
    datYear        <- NULL
    totalFantasmas <- NULL
    totalEmisiones <- NULL
    distanciaTotal <- NULL
    dat            <- NULL
    
    # LUEGO CAMBIAR EL DIRECTORIO
    setwd(file.path(directorio, i)) ##direciona a la carpeta anho
    #
    archivos <- dir()
    if(length(archivos)>1){
      for(j in 1:length(archivos)){   
        dat <- read.dbf(archivos[j]) ##lee cada archivo dbf     
        datYear <- rbind(datYear,dat)
      }  
    } else {
      datYear <- read.csv(archivos)
    }
        
    emb = lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2 = unlist(emb) 
    
    for(barco in sort(unique(datYear$NUMERO_EMB))){
      datBarco           <- datYear[datYear$NUMERO_EMB == barco,]
      datBarco$DATACION2 <- modTime(datBarco$DATACION) 
      datBarco           <- datBarco[order(datBarco$DATACION2),]
      
      barcoBueno <- dim(datBarco)[1]      
      if(barcoBueno > 10){
        row.repite = sameRow(datBarco$DATACION2) #QUITAMOS LAS EMISIONES REPETIDAS
        if(length(row.repite) != 0){
          datBarco = datBarco[-row.repite,]
        }
        
        distancia0 <- matrix(0,nrow = 1, ncol = nrow(caletas)*2)        
        distancia0[,seq(1,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,2]))),nrow = 1)
        distancia0[,seq(2,nrow(caletas)*2,by=2)] <- matrix(as.numeric(t(as.matrix(caletas[,3]))),nrow = 1)
        distancia0 <- as.vector(distancia0)
        distancia      <- rep(distancia0,rep(length(datBarco[,1]),length(distancia0)))
        dim(distancia) <- c(length(datBarco[,1]),dim(caletas)[1]*2)
        
        distanciaTotal <- NULL
        for(lonp in seq(1,nrow(caletas)*2,by=2)){
          distanciaPuerto <-  distORTODROMICA(datBarco$X,datBarco$Y,distancia[,lonp],distancia[,lonp+1])  
          distanciaTotal  <- cbind(distanciaTotal,distanciaPuerto)        
        } 
        distanciaTotal            <- data.frame(distanciaTotal)
        datBarco$puerto           <- apply(distanciaTotal,1,which.min)
        datBarco$distanciaPuerto  <- apply(distanciaTotal,1,min) ## distancia al puerto de origen    
        datBarco$horaEmision      <- c(NA, (julian(datBarco$DATACION2[1:(length(datBarco[,1])-1)])-julian(datBarco$DATACION2[2:(length(datBarco[,1]))]))*24*(-1))
        datBarco$distanciaEmision <- c(NA, distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])]))
        datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
        datBarco$change.speed.1   <- c(NA, diff(datBarco$velocidadEmision))
        datBarco$change.speed.2   <- c(diff(datBarco$velocidadEmision), NA)
        datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
        #datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
        datBarco$cambioRUMBO      <- c(NA,abs(diff(datBarco$diferenciaRUMBO)))
        datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
        datBarco$angle            <- c(NA, estimateAngle(datBarco$X, datBarco$Y), NA)
        datBarco$cambio.angle.1   <- c(NA, rev(diff(rev(datBarco$angle))))
        datBarco$cambio.angle.2   <- c(rev(diff(rev(datBarco$angle))), NA)
        
        carpeta <- paste("dat",i, sep = "")
        write.csv(datBarco, file = file.path(directorio,carpeta,paste(barco,".csv", sep = ""))) 
      }
    }        
  }  
  return(invisible(NULL))
}
