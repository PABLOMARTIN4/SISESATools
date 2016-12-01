interpolatevms <- function(data, interval = 60, proj4 = NULL, area = NULL){

  if (!require(sp)) 
    stop("package sp is required to run interpolatevms()")
  
  interpL <- lapply(split(data, data$Cod.Viaje.VMS, drop = TRUE),function(x){
    
    if(is.null(proj4)) proj4 = "+proj=tmerc +lat_0=-6 +lon_0=-80.5 +k=0.99983008 +x_0=222000 +y_0=1426834.743 +ellps=intl +towgs84=-288,175,-376,0,0,0,0 +units=m +no_defs"
    if(is.null(area)) area = "+init=epsg:24891"
  
    print(x$Cod.Viaje.VMS[1])  
    
    t2 <- seq(from=floor_date(x$DATE[1],unit="hour"), ceiling_date(x$DATE[length(x$DATE)],unit="hour"),by=60*interval) # that's because I'm doing it by hour
    ini = which.min(t2 - x$DATE[1] < -0.000000001) - 1
    fin = which.max(t2 - x$DATE[length(x$DATE)] > -0.000000001)
    t2  = t2[ini:fin]  
    

    interp <- lapply(t2[2:(length(t2)-1)],function(tempo){ 
      
      dif.tiempo <- difftime(x$DATE,tempo)
      ind1 <- which(dif.tiempo<0)[sum(dif.tiempo<0)]
      ind3 <- ind1+1
      lon2 <- (x$LONGITUDE_M[ind3] - x$LONGITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LONGITUDE_M[ind1]
      lat2 <- (x$LATITUDE_M[ind3] - x$LATITUDE_M[ind1])*as.numeric(difftime(tempo,x$DATE[ind1],units = "hour"))/as.numeric(difftime(x$DATE[ind3],x$DATE[ind1],units="hours")) + x$LATITUDE_M[ind1]
      ind.comp <- which.min(abs(difftime(x$DATE,tempo,units = "mins"))) 
      
      cbind.data.frame(lon2,lat2,Cod.Barco=x$Cod.Barco[ind.comp], Vel.VMS=x$Vel.VMS[ind.comp], Rumbo.VMS=x$Rumbo.VMS[ind.comp], Puerto.0.Mar.1=x$Puerto.0.Mar.1[ind.comp], 
                       Dist.Puerto=x$Dist.Puerto[ind.comp],	Dif.Tiempo=x$Dif.Tiempo[ind.comp], Dist.Emisiones=x$Dist.Emisiones[ind.comp], Vel.Cal=x$Vel.Cal[ind.comp], Cambio.Rumbo.Calc=x$Cambio.Rumbo.Calc[ind.comp], 
                       Cod.Viaje.VMS=x$Cod.Viaje.VMS[ind.comp], Move=x$Move[ind.comp], Dist.Costa=x$Dist.Costa[ind.comp], Flota=x$Flota[ind.comp],	Dur.Viaje=x$Dur.Viaje[ind.comp], month=x$month[ind.comp], ind.t = as.numeric(abs(as.numeric(difftime(tempo,x$DATE[ind.comp],units="hour")))>1),
                       DATE.INTERP=tempo)
      
    })
    matriz.int <- do.call(rbind.data.frame,interp)
    
    matriz.int.i = x[1,] 
    matriz.int.i$lon2 = matriz.int.i$LONGITUDE_M; matriz.int.i$lat2 = matriz.int.i$LATITUDE_M
    matriz.int.i$DATE.INTERP = t2[1]
    matriz.int.i$ind.t = matriz.int$ind.t[1]

    matriz.int.f = x[length(x[,1]),]
    matriz.int.f$lon2 = matriz.int.f$LONGITUDE_M; matriz.int.f$lat2 = matriz.int.f$LATITUDE_M
    matriz.int.f$DATE.INTERP = t2[length(t2)]
    matriz.int.f$ind.t = matriz.int$ind.t[1]

    matriz.int = rbind(matriz.int.i[,names(matriz.int)], matriz.int, matriz.int.f[,names(matriz.int)]) 

  })
  interpoL <- do.call(rbind.data.frame,interpL)
  vms.interpol <- unprojet(interpoL)
  #write.csv(vms.interpol , file=paste0('sisesat_',year,'-',interval,"_min",'.csv'))
  #save(vms.interpol,file=paste0('sisesat_',year,'.RData'))
  return(vms.interpol)
}


projet <- function(data){
  require(sp)
  xy <- data[c("Lon","Lat")]
  data[c("LONGITUDE_M","LATITUDE_M")] <- coordinates(spTransform(SpatialPointsDataFrame(xy, data, 
                                                                                        proj4string = CRS(proj4),
                                                                                        match.ID = TRUE),CRS(area)))
  return(data)
}

unprojet <- function(data){
  require(sp)
  xy <- data[c("lon2","lat2")]
  data[c("LONGITUDE","LATITUDE")] <- coordinates(spTransform(SpatialPointsDataFrame(xy,data,
                                                                                    proj4string = CRS(area)),
                                                             CRS(proj4)))
  return(data)
}