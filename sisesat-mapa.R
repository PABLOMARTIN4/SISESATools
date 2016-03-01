# funcion para realizar el mapa
# quita y pone el punto de puerto en el mapa
 

.pointZarpe <- function(x,y){
  x2 <- x[1] + 0.01
  y2 <- y[1]
  x2 <- c(x2,x)
  y2 <- c(y2,y)
  return(list(x2 = x2, y2 = y2))
}
# x = base$X
# y = base$Y
# velocidad = base$velocidadEmision
# realiza un mapa de la trayectoria del viaje

mapVMS <- function(x = x, y = y, velocidad = velocidad, texto = NULL,...){
  require(shape)
  require(fenix)
  xlim = c(min(x),max(x))
  ylim = c(min(y),max(y))  
  newPoint <- .pointZarpe(x, y)
  x0 <- newPoint$x2
  y0 <- newPoint$y2
  x1 <- rev(rev(x0)[-1])
  y1 <- rev(rev(y0)[-1])
  x2 <- x0[-1]
  y2 <- y0[-1]

  plot(y0 ~ x0,  type = "l", xlim = xlim, ylim = ylim, ylab = "Latitud", xlab = "Longitud")
  lines(shoreline)
  Arrows(x1, y1, x2, y2, arr.type = "curved", code = 2,lty = 1,
         arr.length = 0.2, arr.adj = 1, col = velCol(velocidad))
  
  if(isTRUE(texto)){
    text(x, y, texto, pos = 4, cex = 0.6)  
  }  
  return(invisible)
}

# genera un pdf por cada barco por year    

allmapVMS <- function(year, dirmap1, dirmap2){
  
  setwd(file.path(dirmap1,paste("viaje",year,"dat", sep = "")))
  archivos  <- dir()
  for(i in 1:length(archivos)){
    data <- read.csv(archivos[i])
    pdf(file.path(dirmap2,year,paste(archivos[i],".pdf", sep = ""))) 
    for(fio in sort(unique(data$viaje))){
      base <- data[data$viaje == fio,]  
      mapVMS(x = base$X, y = base$Y, velocidad = base$velocidadEmision)
    }
    
    dev.off()  
  }
}
#incluir la mejor version del mapa
