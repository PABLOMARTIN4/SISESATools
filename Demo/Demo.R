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
#directorio = "E:/enviar_IMARPE_data2010a2015"
#readVMSvesselafter(directorio, year = 2000:2014, caletas = caletas)

#allPuertos = read.csv("E:/package/base_puertos.csv")
#temporadaSardina = read.csv("E:/package/temporada_sardina.csv")
# 
# save(allPuertos, file = "allPuertos.rda")
# save(temporadaSardina, file = "temporadaSardina.rda")
# vamos a probar el readVMSvesselbefore2010


temporadaAnchovetaNC = read.csv("E:/package/temporada_anch_nc.csv")
save(temporadaAnchovetaNC, file = "temporadaAnchovetaNC.rda")                
