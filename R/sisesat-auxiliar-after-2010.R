# QUITAR LAS EMISIONES REPETIDAS
sameRow <- function(x){ 
  resta     <- difftime(x[-1],x[-length(x)])
  idrow <-  which(resta == 0)+1
  return(idrow)
}

#baseBoat2 AHORA ES readVMSVESSEL
readVMSvesselafter <- function(directorio, year, caletas, ... ){#caletas
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
        dat <- read.csv(archivos[j], stringsAsFactors = F,encoding = "utf-8") ##lee cada archivo dbf   
        #dat <- dat[dat$X < 0,]
        datYear <- rbind(datYear,dat)
      }  
    } else {
      datYear <- read.csv(archivos)
    }
    
    datYear = datYear[,-which(colnames(datYear) == "X.1")]      
    emb = lapply(strsplit(as.character(datYear$EMBARCACIO), split = "/"), function(xvect) return(xvect[1]))## extrae el ex-nombre del vector EMBARCACIO
    datYear$EMBARCACIO2 = unlist(emb) 
    datYear$CODIGO = as.numeric(apply(as.data.frame(datYear$MATRICULA), 1, extrae.numero))  
    #barco = 10061
    for(barco in unique(datYear$CODIGO)){
      datBarco           <- datYear[datYear$CODIGO == barco,]
      datBarco$DATACION2 <- modTime(datBarco$DATACION) 
      datBarco           <- datBarco[order(datBarco$DATACION2),]
      
      row.repite = sameRow(datBarco$DATACION2) #QUITAMOS LAS EMISIONES REPETIDAS
      if(length(row.repite) > 0){
        datBarco = datBarco[-row.repite,]
      }
      
      barcoBueno <- dim(datBarco)[1]
      if(barcoBueno > 20){
        
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
      datBarco$horaEmision      <- c(NA,difftime(datBarco$DATACION2[-1],datBarco$DATACION2[-length(datBarco$DATACION2)], units = "hours"))  
      datBarco$distanciaEmision <- c(NA, distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])]))
      datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision

      # removemos Inf, los cuales se producen cuando hay saltos en la fecha 

      whichInf = which(is.infinite(datBarco$velocidadEmision))
      removeRow = whichInf[diff(whichInf) != 1]
      
      if(length(removeRow) > 0){
        datBarco = datBarco[-removeRow,]        
        datBarco$horaEmision      <- c(NA,difftime(datBarco$DATACION2[-1],datBarco$DATACION2[-length(datBarco$DATACION2)], units = "hours"))  
        datBarco$distanciaEmision <- c(NA, distORTODROMICA(datBarco$X[1:(length(datBarco[,1])-1)],datBarco$Y[1:(length(datBarco[,1])-1)],datBarco$X[2:length(datBarco[,1])],datBarco$Y[2:length(datBarco[,1])]))
        datBarco$velocidadEmision <- datBarco$distanciaEmision/datBarco$horaEmision
      }
      
      datBarco$change.speed.1   <- c(NA, diff(datBarco$velocidadEmision))
      datBarco$change.speed.2   <- c(diff(datBarco$velocidadEmision), NA)
      datBarco$diferenciaRUMBO  <- calcularRumbo(datBarco$X,datBarco$Y)#$vectorRUMBO
      #datBarco$cambioRUMBO      <- apply(matrix(datBarco$diferenciaRUMBO),1,modificarRumbo)  
      datBarco$cambioRUMBO      <- c(NA,abs(diff(datBarco$diferenciaRUMBO)))
      datBarco$compVelocidad    <- datBarco$VELOCIDAD-round(datBarco$velocidadEmision,1)
      datBarco$angle            <- c(NA, estimateAngle(datBarco$X, datBarco$Y), NA)
      datBarco$cambio.angle.1   <- c(NA, rev(diff(rev(datBarco$angle))))
      datBarco$cambio.angle.2   <- c(rev(diff(rev(datBarco$angle))), NA)
        
      carpeta <- paste("dat",substring(datBarco$DATACION[1], 7, 10), sep = "")
      write.csv(datBarco, file = file.path(directorio,carpeta,paste(barco,".csv", sep = ""))) 
      }
    }        
  }  
  return(invisible(NULL))
}

# IDENTIFICAR VIAJES
#data = read.csv("E:/vms_prueba_tesis/data_vms/dat2010/11433.csv")
# dim(data)<
# identificar viajes
# vmax = maxima velocidad
# hmax = hora maxima
# dmin = minima distancia de la emision a puerto a puerto durante el viaje
# out = idTrips(data)

# out = VMSkit:::.idTrips(data, distPUERTO = 2, velPUERTO = 2, see = TRUE,  
#                             vmax = 15, dmin = 10 )

.idTrips <- function(data = data, distPUERTO = 2, velPUERTO = 5, minEmiMar = 4,
                    vmax = 15, hmax = 2.3, dmin = 5, see = FALSE){  
  #print(data$CODIGO[1])
  dpuerto  <- data[["distanciaPuerto"]]  # distancia a puerto
  vemision <- data[["velocidadEmision"]] # velocidad de emision  
  #demision <- data[["distanciaEmision"]] # distancia entre emisiones
  
  data$errores <- 0
  # 0 sin errores
  # 1 velocidad 0 cuando el barco esta en mar
  # 2 el intervalo de horas es mayor a 2
  # 3 la hora en inf y se le pone 0  
  vemision[1][is.na(vemision[1])] <- 0
  #demision[1][is.na(demision[1])] <- 0
  
  vemision[is.na(vemision)] <- 20 # para se eliminado despues
  data$errores[dpuerto > distPUERTO & is.na(vemision)] <- 1 # 2
  
  #UBICAR LOS PUNTOS EN TIERRA Y MAR
  n  <- length(dpuerto)
  id <- rep(0, n)
  for (i in 1:n){
    if(dpuerto[i] < distPUERTO){ # 2
      if(vemision[i] < velPUERTO){ # 2
 #       if(demision[i] < emiPUERTO){ # 1
        id[i] <- 1
      }else {
        id[i] <- 0 
       }
 #    } 
    }
  }
  data$id <- id
  #UBICAR LOS VIAJES
  data$errores[is.infinite(data$horaEmision) & is.na((data$horaEmision))] <- 3
  data$horaEmision[is.infinite(data$horaEmision) & is.na(data$horaEmision)] <- 3
  data$errores[data$horaEmision > hmax] <- 2
  data$viaje = NA
 
  idpUERTO <- which(data$id %in% 1) 
  Location        = data.frame(rowi = idpUERTO)
  if(dim(Location)[1] == 0){
     Location = data.frame(rowi =c(1,1))
  }
     Location$length = c(diff(Location$rowi),1) 
 
 if(max(Location$length) > minEmiMar){
   Location        = Location[Location$length > minEmiMar,] # 4
   Location$rowf   = Location$rowi + Location$length 
   Location$same   = c(NA,Location$rowf[-length(Location$rowi)] - Location$rowi[-1]) 
   if(length(which(Location$same == 0))>0){
     Location[which(Location$same == 0),"rowi"] = Location[which(Location$same == 0),"rowi"] + 1  
   }
   
   Location$length = Location$rowf - Location$rowi 
   viaje <- rep(1:length(Location$length),Location$length+1)
   
   emisionesViaje <- NULL
   for(u in 1:length(Location$length)){
     emisiones      <- Location$rowi[u]:Location$rowf[u]
     emisionesViaje <- c(emisionesViaje, emisiones)
   }    
   dataOUT       = data[emisionesViaje,]
   dataOUT$viaje = viaje    
   #LIMPIAR VIAJES ( quitar la primera fila de la horaEmision)
   dataOUT2 = dataOUT[-Location$rowi,]
   omitviaje = unique(dataOUT2[dataOUT2$velocidadEmision > vmax # 15 
                               & !is.na(dataOUT2$velocidadEmision), "viaje"])
   dataOUT   = dataOUT[!dataOUT$viaje %in% omitviaje, ]
   omitviaje = unique(dataOUT2[dataOUT2$horaEmision > hmax # 2.3
                               & !is.na(dataOUT2$horaEmision), "viaje"])
   dataOUT   = dataOUT[!dataOUT$viaje %in% omitviaje, ]
   omitDpuerto = tapply(dataOUT$distanciaPuerto, dataOUT$viaje, max)
   omitDpuerto = as.numeric(names(omitDpuerto[omitDpuerto < dmin])) # 5
   dataOUT   = dataOUT[!dataOUT$viaje %in% omitDpuerto, ]
   
   if(dim(dataOUT)[1] == 0){
     dataOUT = data
   }
   if(dim(dataOUT)[1] > 0){
     if(isTRUE(see)){
       print(length(unique(dataOUT$viaje)))  
     }#else{
     #         print("0 viajes")
     #       }
   }
 }
 
else{
    dataOUT = data
  }  
 return(dataOUT)
}

##
fishingTrips <- function(directorio, year, distPUERTO, velPUERTO, 
                         emiPUERTO, minEmiMar, vmax, hmax, dmin, see,...){  
  for(i in unique(year)){
    setwd(file.path(directorio,paste("dat",i, sep =""))) ##direciona a la carpeta anho    
    archivos <- dir()      
    for(j in 1:length(archivos)){
      data <- read.csv(archivos[j])
      if(dim(data)[1] > 10){
        data <- data[order(data$DATACION2),] 
        data_new <- .idTrips(data)
        print(data_new$CODIGO[1])
        carpeta  <- paste("viaje",i,"dat",sep = "")
        write.csv(data_new, file = paste0(directorio,"/",carpeta,"/",archivos[j]))        
      }
    }
  }
  return(invisible(NULL))
}


# plot(data$X[8:26], data$Y[8:26], type = "l", col = 1, ylim = c(-18.3,-16), xlim = c(-72.5, -70.5))
# lines(data$X[31:84], data$Y[31:84], type = "l", col = 2)
# lines(data$X[85:124], data$Y[85:124], col = 3)
# lines(data$X[125:150], data$Y[125:150], col = 4)
# lines(shoreline)

# i = 6
# mapVMS(dataOUT[dataOUT$viaje == i, "X"], dataOUT[dataOUT$viaje == i, "Y"], dataOUT[dataOUT$viaje == i, "velocidadEmision"])    

