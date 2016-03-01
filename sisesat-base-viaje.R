# GENERA LA MATRIX SISESAT POR VIAJE
require(fenix)
baseViajeVMS <- function(directory, directory2, year, ...){
  

  base_viaje_barco  <- NULL
  base_viaje_y      <- NULL
  
  for(y in year){
    setwd(directory)
    archivos  <- dir()
    length(archivos)

    for(i in 1:length(archivos)){
      data <- read.csv(file.path(directory, archivos[i]))
      if(dim(data)[1] > 0) {
        data$dc <- estima.dc(data$X, data$Y)       
        data$dc[is.null(data$dc)]     <- NA
        data$dc[is.infinite(data$dc)] <- NA
               
        for(v in unique(data$viaje)){  
          base   <- data[data$viaje == v,]
          n      <- 1:length(base[,1])
          inicio <- min(n) 
          final  <- max(n)  
          #base$DATACION <- as.POSIXct(strptime(base$DATACION, format = "%Y-%m-%d %H:%M:%S"))        
          base$DATACION <- modTime(base$DATACION)
          # VARIABLES
          if(!is.null(length(base$CODIGO))){
          CODIGO        <- as.numeric(base$CODIGO[inicio])} else { CODIGO = NA }
          
          Barco         <- as.character(base$EMBARCACIO2[inicio])
          viaje         <- base$viaje[inicio]
          dv            <- as.numeric((julian(base$DATACION[final]) - julian(base$DATACION[inicio])))*24
          fechaZarpe    <- base$DATACION[inicio]
          fechaArribo   <- base$DATACION[final]
          puertoZarpe   <- base$puerto[inicio]
          puertoArribo  <- base$puerto[final]
          velocidadMax  <- max(base$velocidadEmision, na.rm  = TRUE); velocidadMax[is.infinite(velocidadMax)] <- NA  
          velocidadMin  <- min(base$velocidadEmision, na.rm  = TRUE); velocidadMin[is.infinite(velocidadMin)] <- NA
          velocidadMean <- mean(base$velocidadEmision, na.rm = TRUE); velocidadMean[is.infinite(velocidadMean)] <- NA
          velocidadsd   <- sd(base$velocidadEmision, na.rm   = TRUE); velocidadsd[is.infinite(velocidadsd)] <- NA
          velocidadvar  <- var(base$velocidadEmision, na.rm  = TRUE); velocidadvar[is.infinite(velocidadvar)] <- NA
          distCostaMean <- mean(base$dc, na.rm = TRUE)
          distCostaSd   <- sd(base$dc, na.rm   = TRUE)
          distCostaVar  <- as.numeric(var(base$dc, na.rm  = TRUE))
          lat.max       <- max(base$Y, na.rm   = TRUE)
          lat.min       <- min(base$Y, na.rm   = TRUE)
          lon.max       <- max(base$X, na.rm   = TRUE)
          lon.min       <- min(base$X, na.rm   = TRUE)
          recorrido     <- sum(base$distanciaEmision, na.rm = T)
          longitud      <- distORTODROMICA(base$X[inicio], base$Y[inicio],base$X[final], base$Y[final])
          sinuosidad10  <- sinuosidad10(base$angle,base$distanciaEmision)
          #sinuosidad08  <- sinuosidad08(base$angle,base$distanciaEmision)
          sinuosidad01  <- sinuosidad01(recorrido, longitud)        
          # incorporar la variable de emision corregido.
          
          base2 <- data.frame(Codigo, Barco, viaje,dv, fechaZarpe, 
                              fechaArribo, puertoZarpe, puertoArribo, 
                              velocidadMax, velocidadMean, velocidadsd, 
                              velocidadvar, velocidadMin, lat.max, lat.min, 
                              lon.max, lon.min, recorrido, sinuosidad10, 
                              sinuosidad01, distCostaMean, distCostaSd, distCostaVar)
          base_viaje_barco <- rbind(base_viaje_barco, base2)      
          
          }
      }
    }     
    write.csv(base_viaje_barco, file=paste0(directory2,paste("base_viaje_vms_"),year,".csv"))    
  }
}  

f.ang.rel <- function(df){
  ang1 <- df$abs.angle[-nrow(df)]
  ang2 <- df$abs.angle[-1]
  #slsp <- match.arg(slsp)
  #if (slspi == "remove") {
  dist <- c(sqrt((df[-nrow(df), "x"] - df[-1, "x"])^2 + 
                   (df[-nrow(df), "y"] - df[-1, "y"])^2), NA)
  wh.na <- which(dist < 1e-07)
  if (length(wh.na) > 0) {
    no.na <- (1:length(ang1))[!(1:length(ang1)) %in% 
                                wh.na]
    for (i in wh.na) {
      indx <- no.na[no.na < i]
      ang1[i] <- ifelse(length(indx) == 0, NA, ang1[max(indx)])
    }
  }
  #}
  res <- ang2 - ang1
  res <- ifelse(res <= (-pi), 2 * pi + res, res)
  res <- ifelse(res > pi, res - 2 * pi, res)
  return(c(NA, res))
}

###
foo <- function(x) {
  x1 <- x[-1, ]
  x2 <- x[-nrow(x), ]
  dist <- c(sqrt((x1$X - x2$X)^2 + (x1$Y - x2$Y)^2), NA)
  R2n <- (x$X - x$X[1])^2 + (x$Y - x$Y[1])^2
  dt <- c(unclass(x1$DATACION2) - unclass(x2$DATACION2), NA)
  dx <- c(x1$X - x2$X, NA)
  dy <- c(x1$Y - x2$Y, NA)
  abs.angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx))
  
  so <- cbind.data.frame(dx = dx, dy = dy, dist = dist, 
                         dt = dt, R2n = R2n, abs.angle = abs.angle)
  ang.rel <- f.ang.rel(so)
  so2 <- cbind.data.frame(so, ang.rel)
  return(so2)
}

