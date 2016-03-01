.idViaje2 <- function(data){
  
  dpuerto  <- data[["distanciaPuerto"]]  # distancia a puerto
  vemision <- data[["velocidadEmision"]] # velocidad de emision
  
  vemision[is.na(vemision)] <- 0 # al calcular la velociadad aparecen NAs, estos se convierten a cero
  
  id <- NULL
  n  <- length(dpuerto)
  id <- rep(NA, n)
  for (i in 1:n){
    if(dpuerto[i] < 2 && vemision[i] < 2){ # el punto en tierra es cuando la distancia a puerto y la velocidad son menores a 2
      
      id[i] <- 1 
    }  else {
      id[i] <- 0 
    }
  }
  # corrige el error tipo 1: 1,0,1 | 1,0,0,1 | 1,0,0,0,1 y los convierte a 1 todos los valores
  # patron1 
  error <- rep(0, n) #  LA VARIABLE ERROR DETECTA LA EMISIONES CORREGIDAS
  
  for(t in 1:(n-2)){
    codom = c(t, t+1, t+2)
    
    if(sum(abs(id[codom] - c(1,0,1))) == 0){
      id[codom] = c(1,1,1)
      error[codom] = 1
    }  
  }
  
  for(t in 1:(n-3)){
    codom = c(t, t+1, t+2, t+3)
    if(sum(abs(id[codom] - c(1,0,0,1))) == 0){
      id[codom] = c(1,1,1,1)
      error[codom] = 1
    }
  }
  
  for(t in 1:(n-4)){
    codom = c(t, t+1, t+2, t+3, +t+4)
    if(sum(abs(id[codom] - c(1,0,0,0,1))) == 0){
      id[codom] = c(1,1,1,1,1)
      error[codom] = 1
    }
  }
  
  for(t in 1:(n-2)){
    codom = c(t, t+1, t+2)
    if(sum(abs(id[codom] - c(0,1,0))) == 0){
      id[codom] = c(0,0,0)
      error[codom] = 1
    }  
  }
  # ERROR = 1 EMISIONES CORREGIDAS; ERROR = 0 EMISIONES NORMALES 
  data$id <- id
  data$error <- error
  data$horaEmision[is.infinite(data$horaEmision)] <- 0
  
  if(sum(data$id) > 1 # exista por lo menos dos emisiones en tierra
     & length(data$id[data$id==0]) > 4  # si la base viaje tiene por lo menos 4 emisiones en mar 
     & length(diff(which(data$id==1))) != sum(diff(which(data$id==1)))) # no me acuerdo 
  { 
    
    # IDENTIFICA EL VIAJE
    
    posicion1 <- which(data$id %in% 1) 
    pos2 <- NULL
    
    for(i in 2:(length(posicion1)-1)){
      if((posicion1[i+1] - posicion1[i] > 1) == TRUE){
        
        posi <- sum(diff(posicion1[1:i]))
        posf <- sum(diff(posicion1[1:(i+1)]))
        pos  <- c(posi+1, posf+1)
        pos2 <- rbind(pos2, pos)    
      }
    }
    
    vector_viaje <- NULL
    nviaje       <- NULL
    
    for(i in 1:length(pos2[,1])){
      
      vector      <- pos2[i,1]:pos2[i,2]
      eviaje      <- length(vector)
      
      vector_viaje <- c(vector_viaje, vector)
      nviaje       <- c(nviaje, eviaje)
      
    }
    viaje <- rep(1:length(pos2[,1]),nviaje)
    
    data_new <- data[vector_viaje,]
    data_new$viaje <- viaje
    data_new$tipo_viaje <- NA
    
    # CALIDAD DEL VIAJE------------------------------------------------------------
    
    data_new2  <- NULL
    # 
    for(v in unique(data_new$viaje)){  
      base <- data_new[data_new$viaje == v,]
      
      if(max(base$distanciaEmision) < 30  && sum(base$id) == 2 && max(base$horaEmision, na.rm = TRUE)  < 2 
         && max(base$velocidadEmision, na.rm = T) < 20 && mean(base$distanciaPuerto, na.rm = T) > 2){
        base$tipo_viaje <- 1 # buen viaje  
      } else {
        base$tipo_viaje <- 0 # mal viaje    
      }
      data_new2 <- rbind(data_new2, base)
    }
  }
  
  else {
    data_new2            <- data
    data_new2$viaje      <- NA 
    data_new2$tipo_viaje <- NA
  }   
  data_new2 <- data_new2[data_new2$tipo_viaje == 1 & !is.na(data_new2$tipo_viaje),]
  
  return(data_new2)
}
## se genero .newidViaje
.newidViaje2 <- function(data){
  
  dpuerto  <- data[["distanciaPuerto"]]  # distancia a puerto
  vemision <- data[["velocidadEmision"]] # velocidad de emision
  
  vemision[is.na(vemision)] <- 0 # al calcular la velociadad aparecen NAs, estos se convierten a cero
  
  id <- NULL
  n  <- length(dpuerto)
  id <- rep(NA, n)
  for (i in 1:n){
    if(dpuerto[i] < 2 && vemision[i] < 2){ # el punto en tierra es cuando la distancia a puerto y la velocidad son menores a 2
      
      id[i] <- 1 
    }  else {
      id[i] <- 0 
    }
  }
  # corrige el error tipo 1: 1,0,1 | 1,0,0,1 | 1,0,0,0,1 y los convierte a 1 todos los valores
  # patron1 
#   error <- rep(0, n) #  LA VARIABLE ERROR DETECTA LA EMISIONES CORREGIDAS
#   
#   for(t in 1:(n-2)){
#     codom = c(t, t+1, t+2)
#     
#     if(sum(abs(id[codom] - c(1,0,1))) == 0){
#       id[codom] = c(1,1,1)
#       error[codom] = 1
#     }  
#   }
#   
#   for(t in 1:(n-3)){
#     codom = c(t, t+1, t+2, t+3)
#     if(sum(abs(id[codom] - c(1,0,0,1))) == 0){
#       id[codom] = c(1,1,1,1)
#       error[codom] = 1
#     }
#   }
#   
#   for(t in 1:(n-4)){
#     codom = c(t, t+1, t+2, t+3, +t+4)
#     if(sum(abs(id[codom] - c(1,0,0,0,1))) == 0){
#       id[codom] = c(1,1,1,1,1)
#       error[codom] = 1
#     }
#   }
#   
#   for(t in 1:(n-2)){
#     codom = c(t, t+1, t+2)
#     if(sum(abs(id[codom] - c(0,1,0))) == 0){
#       id[codom] = c(0,0,0)
#       error[codom] = 1
#     }  
#   }
  # ERROR = 1 EMISIONES CORREGIDAS; ERROR = 0 EMISIONES NORMALES 
  data$id <- id
#  data$error <- error
  data$horaEmision[is.infinite(data$horaEmision)] <- 0
  
  if(sum(data$id) > 1 # exista por lo menos dos emisiones en tierra
     & length(data$id[data$id==0]) > 4  # si la base viaje tiene por lo menos 4 emisiones en mar 
     & length(diff(which(data$id==1))) != sum(diff(which(data$id==1)))) # no me acuerdo 
  { 
    
    # IDENTIFICA EL VIAJE
    
    posicion1 <- which(data$id %in% 1) 
    pos2 <- NULL
    
    for(i in 2:(length(posicion1)-1)){
      if((posicion1[i+1] - posicion1[i] > 1) == TRUE){
        
        posi <- sum(diff(posicion1[1:i]))
        posf <- sum(diff(posicion1[1:(i+1)]))
        pos  <- c(posi+1, posf+1)
        pos2 <- rbind(pos2, pos)    
      }
    }
    
    vector_viaje <- NULL
    nviaje       <- NULL
    
    for(i in 1:length(pos2[,1])){
      
      vector      <- pos2[i,1]:pos2[i,2]
      eviaje      <- length(vector)
      
      vector_viaje <- c(vector_viaje, vector)
      nviaje       <- c(nviaje, eviaje)
      
    }
    viaje <- rep(1:length(pos2[,1]),nviaje)
    
    data_new <- data[vector_viaje,]
    data_new$viaje <- viaje
    data_new$tipo_viaje <- NA
    
    # CALIDAD DEL VIAJE------------------------------------------------------------
    
    data_new2  <- NULL
    # 
    for(v in unique(data_new$viaje)){  
      base <- data_new[data_new$viaje == v,]
      
      if(max(base$distanciaEmision) < 30  && sum(base$id) == 2 && max(base$horaEmision, na.rm = TRUE)  < 2 
         && max(base$velocidadEmision, na.rm = T) < 20 && mean(base$distanciaPuerto, na.rm = T) > 2){
        base$tipo_viaje <- 1 # buen viaje  
      } else {
        base$tipo_viaje <- 0 # mal viaje    
      }
      data_new2 <- rbind(data_new2, base)
    }
  }
  
  else {
    data_new2            <- data
    data_new2$viaje      <- NA 
    data_new2$tipo_viaje <- NA
  }   
  data_new2 <- data_new2[data_new2$tipo_viaje == 1 & !is.na(data_new2$tipo_viaje),]
  
  return(data_new2)
}



# 19/01/2016 CORRECCION PARA IDENTIFICAR LOS VIAJES DE PESCA --------------

.identificarViajes <- function(data){  
  dpuerto  <- data[["distanciaPuerto"]]  # distancia a puerto
  vemision <- data[["velocidadEmision"]] # velocidad de emision
  
  vemision[is.na(vemision)] <- 0 # al calcular la velociadad aparecen NAs, estos se convierten a cero
  
  n  <- length(dpuerto)
  id <- rep(0, n)
  for (i in 1:n){
    #    if(dpuerto[i] < 2 & vemision[i] < 2){ # el punto en tierra es cuando la distancia a puerto y la velocidad son menores a 2
    if(dpuerto[i] < 2){
      if(vemision[i] < 2){
        id[i] <- 1
      }else {
        id[i] <- 0 
      }
    } 
  }

  data$id <- id
  #  data$error <- error
  data$horaEmision[is.infinite(data$horaEmision)] <- 0
  
  if(sum(data$id) > 2.01){
    if(length(data$id[data$id==0]) > 4){
      if(length(diff(which(data$id==1)))!= sum(diff(which(data$id==1)))){
        posicion1 <- which(data$id %in% 1) 
        pos2 <- NULL
        
        for(i in 2:(length(posicion1)-1)){
          #if(!is.na(posicion1[i+1] - posicion1[i])){
          if((posicion1[i+1] - posicion1[i] > 1) == TRUE){
            
            posi <- sum(diff(posicion1[1:i]))
            posf <- sum(diff(posicion1[1:(i+1)]))
            pos  <- c(posi+1, posf+1)
            pos2 <- rbind(pos2, pos)    
          } 
          #}
        }
        vector_viaje <- NULL
        nviaje       <- NULL
        
        for(i in 1:length(pos2[,1])){
          vector      <- pos2[i,1]:pos2[i,2]
          eviaje      <- length(vector)
          vector_viaje <- c(vector_viaje, vector)
          nviaje       <- c(nviaje, eviaje)      
        }
        viaje <- rep(1:length(pos2[,1]),nviaje)
        
        data_new <- data[vector_viaje,]
        data_new$viaje <- viaje
        data_new$tipo_viaje <- 0
      }
      none_viaje = table(data_new$viaje)
      remove_viaje = as.numeric(names(none_viaje[none_viaje < 7]))
      data_new = data_new[!data_new$viaje %in% remove_viaje,]
      ##
      data_new2 <- NULL
      for(v in unique(data_new$viaje)){  
        base = data_new[data_new$viaje == v,]
        nn   = length(base[,1])
        base$horaEmision[1] = NA
        if(max(base$horaEmision[-1]) == base$horaEmision[nn]){
          base          = base[-nn,]
          base$id[nn-1] = 1
        }
        ## mejorar el if
        if(max(base$horaEmision[-1],na.rm = T) <= 2.01){
          if(max(base$velocidadEmision[-1], na.rm = T) <= 16){ #velocidad maxima 16 mn/hora
            if(sum(base$id) == 2){
              if(mean(base$distanciaPuerto, na.rm = T) > 2){
                base$tipo_viaje <- 1 # buen viaje  
              } 
            }    
          }
        }else{
          base$tipo_viaje <- 0 # mal viaje    
        }  
        data_new2 <- rbind(data_new2, base)
      }
    }
  }else{
    data_new2 <- NULL
  }
  return(data_new2) 
}

