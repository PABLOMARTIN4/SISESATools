# Distancia entre dos puntos
distXY <- function(lonA,latA,lonB,latB){
  distXY <- sqrt((lonA-lonB)^2 + (latA-latB)^2)*60
  return(distXY)
}
# Distancia entre dos puntos considerando la curvatura de la tierra
distORTODROMICA <- function (x1,y1,x2,y2) 
{
  lon.mn1 = -x1 * 60 * cos(-y1 * pi/180)
  lon.mn2 = -x2 * 60 * cos(-y2 * pi/180)
  lat.mn1 = -y1 * 60
  lat.mn2 = -y2 * 60
  out = sqrt((lon.mn1 - lon.mn2)^2 + (lat.mn1 - lat.mn2)^2)
  return(out)
}
#
equationAngle <- function(x,y){
  dot.prod <- x%*%y
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))*180/pi
  return(theta)
}
#
estimateAngle <- function(x,y){
  vec_angle <- NULL
  for(z in 2:(length(x)-1)){
    A <- matrix(NA,nrow = 2)
    B <- matrix(NA,nrow = 2)
    X1 <- as.matrix(x[c(z-1,z)])
    Y1 <- as.matrix(y[c(z-1,z)])
    X2 <- as.matrix(x[c(z,z+1)])
    Y2 <- as.matrix(y[c(z,z+1)])
    A[1] <- X1[1]-X1[2]
    A[2] <- Y1[1]-Y1[2]
    B[1] <- X2[2]-X2[1]
    B[2] <- Y2[2]-Y2[1]
    angle <- equationAngle(t(A),B)
    vec_angle <- rbind(vec_angle,angle)
  }
  return(vec_angle)
}
# uniformiza el tiempo
modTime <- function(x){
  xDate <- strsplit(as.character(x), split = " ")  
  fecha <- unlist(lapply(xDate,function(xvect) return(xvect[1])))
  hora <- unlist(lapply(xDate,function(xvect) return(xvect[2])))
  AMPM <- unlist(lapply(xDate,function(xvect) return(xvect[3])))
  
  hora[is.na(AMPM)] <- "00:00:00"
  
  H <- substr(hora,1,2)
  
  H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H != "12")] <- as.character(as.numeric(H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H != "12")])+12)
  #H[which(AMPM %in% c("PM","P.M.","p.m.","pm") & H == "12")] <- "00"
  horacorr <- paste(H,substring(hora,3,8),sep = "")
  
  #horacorr[horacorr > 24] <- paste("00",substring(horacorr[horacorr > 24],3,9),sep = "")
  
  xDatecorr <- as.POSIXct(strptime(paste(fecha,horacorr,sep=" "), format = "%d/%m/%Y %H:%M:%S"))
  return(xDatecorr)
}
# formato rumbo rocio
newRumbo <- function(x){
  x[!is.na(x) & x>180] <- x[!is.na(x) & x>180]-360 
  return(x)
}
# mofica el rumbo 
modificarRumbo <- function(x){
  out <- min(x,360-x)
  out[is.infinite(out)] <- NA
  return(out)
}
# calcula el rumbo
calcularRumbo <- function(X,Y){
  vectorRumbo <- NULL 
  vectorRumbo[1] <- NA
  for(i in 2:length(X)){
    x2 <-  X[i]    
    y2 <-  Y[i]  
    x1 <-  X[i-1]  
    y1 <-  Y[i-1]  
    y   <- sin((x2-x1)*pi/180) * cos(y2*pi/180)
    x   <- cos(y1*pi/180) * sin(y2*pi/180) - sin(y1*pi/180) * cos(y2*pi/180) * cos((x2-x1)*pi/180)
    Rumbo <- atan2(y,x)*180/pi
    Rumbo <- (Rumbo + 360)%%360
    vectorRumbo <- rbind(vectorRumbo,Rumbo)
  }
  vectorRumbo <- as.vector(vectorRumbo)
  return(vectorRumbo)#, cambioRumbo = cambioRumbo))
} 
# calcula el puerto
idPuerto <- function(x){
  port2 <- NULL
  
  for(i in 1:length(x)){
    port <- as.character(puertos[puertos$numero == x[i], "puerto2"])
    port2 <- rbind(port, port2)
  }
  port2 <- as.character(port2)
  return(port2)
}  
## EXTRAE EL NUMERO DE LA MATRICULA DEL BARCO
extrae.numero <- function(x)
{
  n <- nchar(x)
  ext_num <- substring(x, 1:n, 1:n)
  numbers <- 0:9
  num <- NULL
  for (i in ext_num){
    if (i %in% numbers){
      num <- paste(num,i,sep="")
    }
  }
  num <- as.numeric(num)
  return(num)
}

# TORTUOCIDAD -------------------------------------------------------------
# paper How to reliably estimate the tortuosity of an animals path
# Simon Benhamou 2004

# S <- sinuosidad
# p <-  esperanza de la longitud de los pasos
# c <-  coseno promedio
# s <-  seno promedio
# b <-  coeficiente de variacion de la longitud de los pasos
# las ecuaciones de sinuocidad se simon no pueden aplicarse a los viajes de pesca
# dado que estas trayectorias no son homogeneas. pero si puede usarse la ecuacion
# de l/D

##
sinuosidad10 <- function(tetha, distanciaEmision){
  s <- mean(sin(tetha), na.rm = TRUE)
  c <- mean(cos(tetha), na.rm = TRUE)
  p <- mean(distanciaEmision, na.rm = TRUE)
  b <- sd(distanciaEmision, na.rm = TRUE)
  
  out <- 2*(p*((1-(c^2)-(s^2))/((1-c^2)+s^2)+b^2))^(-0.5) 
  return(out)
}

##
sinuosidad08 <- function(tetha, distanciaEmision){
  s <- mean(sin(tetha), na.rm = TRUE)
  c <- mean(cos(tetha), na.rm = TRUE)
  p <- mean(distanciaEmision, na.rm = TRUE)
  b <- sd(distanciaEmision, na.rm = TRUE)
  
  out <- 2(p*((1+c)/(1-c))+b^2)^-0.5
  return(out)
}

##
sinuosidad01 <- function(recorrido, longitud){
  rectitud <- longitud/recorrido
  return(rectitud)
}

## MAPAS
velCol <- function(x){
  xcol = x
  xcol[x >= 8] <- 3
  xcol[x >= 5 & x < 8] <- 5
  xcol[x > 2 & x < 5] <- 7
  xcol[x <= 2] <- 2
  return(xcol)
}


# Pega data.frame con las mismas columnas ---------------------------------

rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}


make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}
