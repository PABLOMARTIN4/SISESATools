require(foreign) 
require(fenix)    # 
require(gdata)
require(stringr)
require(parallel) # multinucleo
require(doSNOW)   # multinucleo
require(VMSkit)
require(shape)    # Arrows (mapVMS)

rm(data)
directorio = "E:/vms_prueba_tesis/data_vms"

datsat = read.csv("C:/Users/pmarin/Desktop/datsat.csv")
datsat$date <- strptime(datsat$date, format = "%d/%m/%Y %H:%M")

save(datsat, file = "datsat.rda")


data(datsat)
datsat = datsat[datsat$idTrip == "10060-8",]

# Proyet datsat
library(lubridate)
projet.datsat <- projet(datsat)

# Interpolate the VMS data
interpolation <- interpolatevms(projet.datsat, interval = 60, proj4 = NULL, area = NULL)



# directorio = "E:/enviar_IMARPE_data2010a2015"
# readVMSvesselafter(directorio, year = 2000:2014, caletas = caletas)

# allPuertos = read.csv("E:/package/base_puertos.csv")
# temporadaSardina = read.csv("E:/package/temporada_sardina.csv")

# save(allPuertos, file = "allPuertos.rda")
# save(temporadaSardina, file = "temporadaSardina.rda")
# vamos a probar el readVMSvesselbefore2010

# temporadaAnchovetaNC = read.csv("E:/package/temporada_anch_nc.csv")
# save(temporadaAnchovetaNC, file = "temporadaAnchovetaNC.rda")                
