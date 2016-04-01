
# Cruce de la informacion SISESAT - SEGUIMIENTO ---------------------------
readDataSeg <- function(data, ...){
  # incluir fecha, Fecha.Fin y Codigo.Fecha 
}

.removeSameRow = function(x){ 
  resta   = difftime(x[-1],x[-length(x)])
  idrow   = c(which(resta == 0), which(resta == 0)+1)
  idrow   = unique(idrow)
  return(idrow)
}

removeEqualTrip <- function(dataSeg){
  seguimiento = dataSeg
  seguimiento.corr = NULL

  for(ii in unique(seguimiento$cod)){
    seguimientoCod = seguimiento[seguimiento$cod == ii,]
    seguimientoCod = seguimientoCod[order(seguimientoCod$Fecha.Fin),]
    rRow = .removeSameRow(seguimientoCod$Fecha.Fin)

    if(length(rRow) > 0) {
      seguimientoCod = seguimientoCod[-rRow,]
    }
    seguimiento.corr = rbind(seguimiento.corr, seguimientoCod)  

    return(seguimiento.corr)
  }
}
 
linkSatSeg <- function(dataSat, dataSeg, ...){
  
  sisesat      = dataSat
  seguimiento  = dataSeg
  sisesat$tipo = NA; sisesat$cb = NA; sisesat$captura = NA
  
  for(ii in uniqueCodigo){
    isat = which(sisesat$Cod.Barco == ii)
    iseg = which(seguimiento$cod == ii)
    seguimientoCod = seguimiento[iseg,]
    
    slength = dim(seguimientoCod)
    if(slength[1] > 1){
      seguimientoCod = seguimientoCod[order(seguimientoCod$Fecha.Fin),]
      rRow = .removeSameRow(seguimientoCod$Fecha.Fin)
      if(length(rRow) > 0){
        seguimientoCod = seguimientoCod[-rRow,]      
      }
    }
    for(iii in 1:length(isat)){
      itime = as.numeric(abs(difftime(sisesat[isat,"Fecha.Fin"][iii], seguimientoCod$Fecha.Fin, units = "hours")))
      if(length(itime) > 0){
        if(itime[which.min(itime)] < 12){
          sisesat[isat,"tipo"][iii]    = seguimientoCod[which.min(itime), "tipo"]
          sisesat[isat,"cb"][iii]      = seguimientoCod[which.min(itime), "cb"]
          sisesat[isat,"captura"][iii] = seguimientoCod[which.min(itime), "anchoveta"]
          # incluir tambien jurel y caballa y especies incidentales
        }
      }
    }  
  }
  sisesat = sisesat[!is.na(sisesat$cb),]
  return(sisesat)
}

