# source("funcione.R")
# source("funcion_viaje_prueba.R")

# paquetes ----------------------------------------------------------------
require(foreign) 
require(fenix)    # 
require(gdata)
require(stringr)
require(parallel) # multinucleo
require(doSNOW)   # multinucleo
require(VMSkit)
require(shape)    # Arrows (mapVMS)


# DEMO --------------------------------------------------------------------
directorio = "E:/vms_prueba_tesis"
directorio = "E:/enviar_IMARPE_data2010a2015"
setwd(directorio)

# data de todas las caletas en peru
#caletas <- read.csv("E:/vms_prueba/combinado_mariano_erich_manuel.csv",header = F)

year1 <- c(2000:2002,2004:2009)
year2 <- c(2010:2015)

year1 <- 2010
# CREA CARPETA
#viajeyear <- paste("viaje", year1,sep = "")
#createFolder(directorio, year)
#createFolder(directorio, viajeyear)

# aún falta automatizar esta parte y luego incluir los demas indicadores
baseBoat2(directorio, year = year1, caletas)
#baseBoot2(directorio, year = year2, caletas)

# ir haciendo las modificaciones al paquete
# corregir la funcion baseBoat
baseBoat2(directorio, year =  2010, caletas) # listo baseBoot2 2010-2015 # limpieza del año 2011
# baseBoat2(directorio, year =  2000:2009, caletas) # listo baseBoot2 2000-2009
# empezar a ver lo de la presentacion que figuras pueden caer
# baseBoat01(directorio, year, caletas, nodes = 6) # no listo multinucleo 2000-2009


#  ------------------------------------------------------------------------
# Identificamos los viajes ------------------------------------------------
#  ------------------------------------------------------------------------

calcularViaje(directorio, year = 2002) 
calcularViaje(directorio, year = 2003)
calcularViaje(directorio, year = 2004)
calcularViaje(directorio, year = 2005)
calcularViaje(directorio, year = 2006)
calcularViaje(directorio, year = 2007)
calcularViaje(directorio, year = 2008)
calcularViaje(directorio, year = 2009)
calcularViaje(directorio, year = 2010)
calcularViaje(directorio, year = 2011)
calcularViaje(directorio, year = 2012)
calcularViaje(directorio, year = 2013)
calcularViaje(directorio, year = 2014)

ddir = "E:/vms_prueba_tesis/viajes_all_vrs_antigua/" 
ddir2 = "E:/vms_prueba_tesis/viajes_VMS/"

#  ------------------------------------------------------------------------
# Calculamos la base viaje ------------------------------------------------
#  ------------------------------------------------------------------------

baseViajeVMS(directory = paste(ddir,"viaje",2014,"dat",sep = ""), directory2 = ddir2,year = 2014)
baseViajeVMS(directory = paste(ddir,2013,"dat",sep = ""), directory2 = ddir2,year = 2013)
baseViajeVMS(directory = paste(ddir,2012,"dat",sep = ""), directory2 = ddir2,year = 2012)
baseViajeVMS(directory = paste(ddir,2011,"dat",sep = ""), directory2 = ddir2,year = 2011)
baseViajeVMS(directory = paste(ddir,2010,"dat",sep = ""), directory2 = ddir2,year = 2010)
baseViajeVMS(directory = paste(ddir,2009,"dat",sep = ""), directory2 = ddir2,year = 2009)
baseViajeVMS(directory = paste(ddir,2008,"dat",sep = ""), directory2 = ddir2,year = 2008)
baseViajeVMS(directory = paste(ddir,2007,"dat",sep = ""), directory2 = ddir2,year = 2007)
baseViajeVMS(directory = paste(ddir,2006,"dat",sep = ""), directory2 = ddir2,year = 2006)
baseViajeVMS(directory = paste(ddir,2005,"dat",sep = ""), directory2 = ddir2,year = 2005)
baseViajeVMS(directory = paste(ddir,2004,"dat",sep = ""), directory2 = ddir2,year = 2004)
baseViajeVMS(directory = paste(ddir,2003,"dat",sep = ""), directory2 = ddir2,year = 2003)
baseViajeVMS(directory = paste(ddir,2002,"dat",sep = ""), directory2 = ddir2,year = 2002)
baseViajeVMS(directory = paste(ddir,2001,"dat",sep = ""), directory2 = ddir2,year = 2001)
baseViajeVMS(directory = paste(ddir,2000,"dat",sep = ""), directory2 = ddir2,year = 2000)





ddir = dirmap1
ddir2 = "E:/enviar_IMARPE_data2010a2015/viajes_VMS"

# corregir la funcion baseViajeVMS " el directorio
baseViajeVMS(directory = paste(ddir,"/","dat",2010,sep = ""), directory2 = ddir2,year = 2010)
baseViajeVMS(directory = paste(ddir,"/","dat",2011,sep = ""), directory2 = ddir2,year = 2011)
baseViajeVMS(directory = paste(ddir,"/","dat",2012,sep = ""), directory2 = ddir2,year = 2012)
baseViajeVMS(directory = paste(ddir,"/","dat",2013,sep = ""), directory2 = ddir2,year = 2013)
baseViajeVMS(directory = paste(ddir,"/","dat",2010,sep = ""), directory2 = ddir2,year = 2014)
baseViajeVMS(directory = paste(ddir,"/","dat",2010,sep = ""), directory2 = ddir2,year = 2015)

#  ------------------------------------------------------------------------
# Histogramas para algunos identificadores --------------------------------
#  ------------------------------------------------------------------------

require(ggplot2)
require(GGally)
require(VGAM)

dataTrip <- read.csv("E:/vms_prueba_tesis/viajes_VMS/base_viaje_vms_2014.csv") 
dataTrip$color <- NULL
dataTrip[dataTrip$sinuosidad01 > 0.9,"color"]  <- "Viajes Traslado"
dataTrip[dataTrip$sinuosidad01 <= 0.9,"color"] <- "viajes Normales"
dataTrip$color <- as.factor(dataTrip$color)
dataTrip       <- dataTrip[!is.na(dataTrip$velocidadvar),]
 
png("E:/vms_prueba_tesis/figuras/velocidadvar.png")
ggplot(dataTrip, aes(velocidadvar,fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/velocidadsd.png")
ggplot(dataTrip, aes(velocidadsd, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/duracionviaje.png")
ggplot(dataTrip, aes(dv, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/distCostaMean.png")
ggplot(dataTrip, aes(distCostaMean, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/distCostaSd.png")
ggplot(dataTrip, aes(distCostaSd, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/distCostaVar.png")
ggplot(dataTrip, aes(distCostaVar, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/indRectitud.png")
ggplot(dataTrip, aes(sinuosidad01, fill=color)) + geom_density(alpha=0.5)
dev.off()

png("E:/vms_prueba_tesis/figuras/Sinuosidad.png")
ggplot(dataTrip, aes(sinuosidad10, fill=color)) + geom_density(alpha=0.5)
dev.off()

#  ------------------------------------------------------------------------
# Identificamos las trayectorias de los viajes ----------------------------
#  ------------------------------------------------------------------------

calcularViaje2(directorio, year = 2000)
calcularViaje2(directorio, year = 2001)
calcularViaje2(directorio, year = 2002) 
calcularViaje2(directorio, year = 2003)
calcularViaje2(directorio, year = 2004)
calcularViaje2(directorio, year = 2005)
calcularViaje2(directorio, year = 2006)
calcularViaje2(directorio, year = 2007)
calcularViaje2(directorio, year = 2008)
calcularViaje2(directorio, year = 2009)
calcularViaje2(directorio, year = 2010)
calcularViaje2(directorio, year = 2011)
calcularViaje2(directorio, year = 2012)
calcularViaje2(directorio, year = 2013)
calcularViaje2(directorio, year = 2014)

#  ------------------------------------------------------------------------
# mapas -------------------------------------------------------------------
#  ------------------------------------------------------------------------
dirmap1 <- "E:/vms_prueba_tesis"
dirmap2 <- "E:/vms_prueba_tesis/mapas"

allmapVMS(year = 2000, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2001, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2002, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2003, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2004, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2005, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2006, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2007, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2008, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2009, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2010, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2011, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2012, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2013, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2014, dirmap1 = dirmap1, dirmap2 = dirmap2)

#  ------------------------------------------------------------------------

# data <- read.csv("E:/vms_prueba_tesis/viaje2000dat/ADRIANA.csv")
# data$PUERTO <- idPuerto(data$puerto)

# revisando nueva informacion ---------------------------------------------

data  <- read.table("E:/enviar_IMARPE_data2010a2015/GEOYMEGAdataOK7.txt", sep ="|", header = TRUE)
data2 <- read.table("E:/enviar_IMARPE_data2010a2015/MACSdataOK4.txt", sep ="|")
# Hay que revisar  
names(data)  <- c("MATRICULA", "EMBARCACIO", "NUMERO_EMB", "DATACION", "X", "Y", "VELOCIDAD", "RUMBO", "CLASE", "PROVEEDOR")
names(data2) <- c("MATRICULA", "EMBARCACIO", "NUMERO_EMB", "DATACION", "X", "Y", "VELOCIDAD", "RUMBO", "CLASE")

data <- data[,-10]
data <- rbind(data,data2)

# 
data2010 <- data[as.numeric(substring(data$DATACION,1,4)) == 2010, ]
data2011 <- data[as.numeric(substring(data$DATACION,1,4)) == 2011, ]
data2012 <- data[as.numeric(substring(data$DATACION,1,4)) == 2012, ]
data2013 <- data[as.numeric(substring(data$DATACION,1,4)) == 2013, ]
data2014 <- data[as.numeric(substring(data$DATACION,1,4)) == 2014, ]
data2015 <- data[as.numeric(substring(data$DATACION,1,4)) == 2015, ]
 
write.csv(data2010,"E:/enviar_IMARPE_data2010a2015/2010/data.csv")
write.csv(data2011,"E:/enviar_IMARPE_data2010a2015/2011/data.csv")
write.csv(data2012,"E:/enviar_IMARPE_data2010a2015/2012/data.csv")
write.csv(data2013,"E:/enviar_IMARPE_data2010a2015/2013/data.csv")
write.csv(data2014,"E:/enviar_IMARPE_data2010a2015/2014/data.csv")
write.csv(data2015,"E:/enviar_IMARPE_data2010a2015/2015/data.csv")
 
# data20102 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2010, ]
# data20112 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2011, ]
# data20122 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2012, ]
# data20132 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2013, ]
# data20142 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2014, ]
# data20152 <- data2[as.numeric(substring(data2$DATACION,1,4)) == 2015, ]
# 
# write.csv(data20102,"E:/enviar_IMARPE_data2010a2015/2010/data2.csv")
# write.csv(data20112,"E:/enviar_IMARPE_data2010a2015/2011/data2.csv")
# write.csv(data20122,"E:/enviar_IMARPE_data2010a2015/2012/data2.csv")
# write.csv(data20132,"E:/enviar_IMARPE_data2010a2015/2013/data2.csv")
# write.csv(data20142,"E:/enviar_IMARPE_data2010a2015/2014/data2.csv")
# write.csv(data20152,"E:/enviar_IMARPE_data2010a2015/2015/data2.csv")

directorioa <- "E:/enviar_IMARPE_data2010a2015"
baseBoat2(directorio = directorioa, year = 2010, caletas)
baseBoat2(directorio = directorioa, year = 2011, caletas)
baseBoat2(directorio = directorioa, year = 2012, caletas)
baseBoat2(directorio = directorioa, year = 2013, caletas)
baseBoat2(directorio = directorioa, year = 2014, caletas)
baseBoat2(directorio = directorioa, year = 2015, caletas)




# REVISION DE LOS ERRORES -------------------------------------------------

#data2010 <- read.csv("E:/enviar_IMARPE_data2010a2015/2010/data.csv", encoding = "utf-8",  dec = ".")
#data2010 <- read.csv("E:/enviar_IMARPE_data2010a2015/2011/data.csv", encoding = "utf-8",  dec = ".")
#data2010 <- read.csv("E:/enviar_IMARPE_data2010a2015/2012/data.csv", encoding = "utf-8",  dec = ".")
#data2010 <- read.csv("data.csv",sep ="|", header = TRUE) falta corregir el 2013
#data2010 <- read.csv("E:/enviar_IMARPE_data2010a2015/2014/data.csv", encoding = "utf-8",  dec = ".")
#data2010 <- read.csv("E:/enviar_IMARPE_data2010a2015/2015/data.csv", encoding = "utf-8",  dec = ".")

require(gdata)
require(fenix)

# RECONOCER LOS VIAJES DE POTA --------------------------------------------

data2010$EMBARCACIO <- trim(data2010$EMBARCACIO)
data2010$EMBARCACIO <- toupper(data2010$EMBARCACIO)
data2010$MATRICULA  <- trim(data2010$MATRICULA)
data2010$MATRICULA  <- toupper(data2010$MATRICULA)
data2010$X          <- gsub(",", '.', data2010$X, fixed = T)
data2010$Y          <- gsub(",", '.', data2010$Y, fixed = T)
data2010$VELOCIDAD  <- gsub(",", '.', data2010$VELOCIDAD, fixed = T)
data2010$RUMBO      <- gsub(",", '.', data2010$RUMBO, fixed = T)

# barcos_pota$BARCO <- trim(barcos_pota$BARCO)
# barcos_pota$BARCO <- toupper(barcos_pota$BARCO)

MAT_RAROS <- c("APNN-6413",  "P-00-00845", "30927-05", "P-00-0740", "PU-0561", "0000", "35845-PEXT-4", "PU-059", "APNN-5438",
              "APNN-6082", "PU-059", "KGI 587", "MGI 2038", "KGI 5855", "AMI 524", "AMMT-1334", "30406-05", "31349-06", "30741-05",
              "BI 5142", "1351", "XXXXXXXXXXXXX", "30361-04", "AMMT-1470", "P-00-0506", "AMMT-1335", "P-04-0576", "P-00-00767", 
              "AAAA" , "APNN-7606", "P-00-0727", "31763-06-A", "BOYA 2", "33420-PEXT-1", "XXXX", "6675-76-C", "OCHO", "30473-PEXT F-5",
              "95100656260007", "08070016261100", "0812001-6263803", "03100016261407", "APNN-8569","6675-76-C","33420-PEXT-1",
              "30537-05", "0201112633-2", "30473-PEXT F-5", "31763-06-A","P-00-00788", "P-04-00765", "P-04-00773","P-00-0688",
              "29947-04-C",   "P-00-00782",   "31756-06-A",   "APNN-7022" ,   "P-00-00794",   "P-00-0715", "AMMT-2094",    
              "P-04-00637", "P-00-0738", "L-18-38014", "P-04-00657", "APNN-7499", "P-04-0627", "P-04-00847", "P-04-0627","P-00-0743",
              "P-00-00649", "P-04-00697","PT-0399-CM","P00 0776","P00 0776","AMMT-2429","1916","143-01-08-4-A(EX-33228-07-A)","SKN 1001171","AMMT-1327",
              "OW-2434","APNN 7231","32239-06","AMMT-1479","P-04-00855","APNN-5751", "P-0000","P-00-00532", "P-04-00749",
              "P-00-00759", "P-00-00825", "P-04-00911","P-04-00006","P-04-00753","34934-09","31349-05-B")
MAT_RAROS <- unique(MAT_RAROS)
OTROS_RAROS  <- c("PT-5240-CM","CO-29711-PM", "SE-0041-CM","CO-29381-PM","PT-17970-CM",
                  "PL-17599-CM", "CO-15861-PM", "PS-0147-BM","PL-14314-CM", "PS-0143-BM","PS-0110-BM",
                  "CO-10833-PM", "CE-12516-PM", "CO-32169-PM","CO-30903-PM","CO-31412-PM","CO-31194-PM",
                  "CO-33547-PM","PL-5497-BM","PT-3977-CM","CO-29725-PM", "CO-30906-PM","CO-30905-PM",
                  "CO-30904-PM", "CO-30388-PM","CO-12598-CM")
BIC_OLAYA    <- c("CO-17706-EM")


data2010anch  <- data2010[!data2010$EMBARCACIO %in% barcos_pota$BARCO,]
data2010anch  <- data2010anch[!data2010anch$MATRICULA %in% as.factor(BIC_OLAYA), ]
data2010anch2 <- data2010anch[!data2010anch$MATRICULA %in% as.factor(MAT_RAROS), ] # arrastreros atuneros

# archivos a guardar
data2010anch3     <- data2010anch2[!data2010anch2$MATRICULA %in% as.factor(OTROS_RAROS), ] # matricula normal diferente distribucion
data2010Pota      <- data2010[ data2010$EMBARCACIO %in% barcos_pota$BARCO,]
data2010noid      <- data2010anch[data2010anch$MATRICULA %in% as.factor(MAT_RAROS),]
data2010anchnoid  <- data2010anch[data2010anch$MATRICULA %in% as.factor(OTROS_RAROS),]
crucero           <- data2010anch2[data2010anch2$MATRICULA %in% as.factor(BIC_OLAYA),]
#
write.csv(data2010anch3, "E:/enviar_IMARPE_data2010a2015/2015_total/anchoveta/data.csv")
write.csv(data2010Pota, "E:/enviar_IMARPE_data2010a2015/2015_total/pota/data.csv")
write.csv(data2010noid, "E:/enviar_IMARPE_data2010a2015/2015_total/otros/data.csv")
write.csv(data2010anchnoid, "E:/enviar_IMARPE_data2010a2015/2015_total/anchoveta_raros/data.csv")
write.csv(crucero , "E:/enviar_IMARPE_data2010a2015/2015_total/crucero/data.csv")
write.csv(data2010 , "E:/enviar_IMARPE_data2010a2015/2015_total/total/data.csv")

# pdf("barconoid.pdf")
# for(i in unique(data2010noid$EMBARCACIO)){
#   base <- data2010noid[data2010noid$EMBARCACIO == i,c("X","Y")]
#   plot(base$X, base$Y, main = base$EMBARCACIO[1])  
#   lines(shoreline, lwd = 2)
# }
# dev.off()
# 
# #
# pdf("barcoPota.pdf")
# for(i in unique(data2010Pota$EMBARCACIO)){
#   base <- data2010Pota[data2010Pota$EMBARCACIO == i,c("X","Y")]
#   plot(base$X, base$Y, main = base$EMBARCACIO[1])  
#   lines(shoreline, lwd = 2)
# }
# dev.off()

# pdf("barcoanch.pdf")
# for(i in unique(data2010anch2$MATRICULA)){
#   base <- data2010anch3[data2010anch3$MATRICULA == i,c("X","Y","MATRICULA","DATACION")]
#   plot(base$X, base$Y, main = base$EMBARCACIO[1])    
# }
# dev.off()

# DDDD <- unlist(lapply(strsplit(data2010anch2$MATRICULA, split = "-"),function(x) x[3]))
# which(DDDD == "CN")
# which(DDDD == "EM")
# 
# data2010anch2[DDDD %in% which(c("CN","EM")),"MATRICULA"]
# data2010anch2[1620693,]
# table(unlist(lapply(strsplit(unique(data2010anch2$MATRICULA), split = "-"),function(x) x[3])))
# plot(data2010noid$X, data2010noid$Y)
# lines(shoreline, lwd = 2)
# 
# plot(data2010Pota$X, data2010Pota$Y)
# lines(shoreline, lwd = 2)
# 
# plot(data2010anch$X, data2010anch$Y)
# lines(shoreline, lwd = 2)

calcularViaje(directorio, year = 2010)
calcularViaje(directorio, year = 2011)
calcularViaje(directorio, year = 2012)
calcularViaje(directorio, year = 2013)
calcularViaje(directorio, year = 2014)
calcularViaje(directorio, year = 2015)

dirmap1 <- "E:/enviar_IMARPE_data2010a2015"
dirmap2 <- "E:/enviar_IMARPE_data2010a2015/mapas"

allmapVMS(year = 2010, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2011, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2012, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2013, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2014, dirmap1 = dirmap1, dirmap2 = dirmap2)
allmapVMS(year = 2015, dirmap1 = dirmap1, dirmap2 = dirmap2)



# mapa presentacion -------------------------------------------------------
data <- read.csv("E:/vms_prueba_tesis/viaje2013dat/1005.csv")
dat <- data[data$viaje == 4,]

mapVMS(x = dat$X, y = dat$Y, velocidad = dat$velocidadEmision, texto = dat$id)
legend("topleft", col = c(0,2,7,5,3), lty = c(0,1,1,1,1), lwd = c(2,2,2,2,2),
       c("velocidad (mn/h)","<=2", ">2 & <=5", ">5 & <=8", ">8"), cex = 0.7, bty = "n")
#text(dat$X, dat$Y, dat$id, cex = 0.3)
