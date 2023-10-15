############### CALCOLO INDICE URBANO (NDBI) NEL TERRITORIO DI FIUMICINO #################

# metto a confronto 2 immagini landsat del 1993 e del 2023
# mese di acquisizione marzo
# calcolo percentuale urbano e non urbano


###### # Sostituisci 'output_file.tif' con il percorso e il nome del tuo file GeoTIFF di destinazione
# writeRaster(rgb_raster, "output_file.tif", format = "GTiff", overwrite = TRUE)
# esportare in geotiff un file raster 



# librerie 
library(raster)       # gestione immagini raster
library(ggplot2)      # visualizzazione dati in modo potente, per plot con ggplot
library(RStoolbox)    # visualizzazione immagine e calcolo variabilità
library(patchwork)    # per multiframe con plot ggplot2

# set working directory 
setwd("/Users/alicegiacomelli/Desktop/dati_tesi")

###################### 2023

# Immagini scaricate da https://earthexplorer.usgs.gov/ 
# Immagini dal Landsat 8 OLI


# creo una lista con pattern in comune = banda23
list23 <- list.files(pattern="banda23")
# importo con la funzione lapply del pacchetto raster
import23 <- lapply(list23, raster)
# unisco componenti della list18 in un unico blocco con la funzione raster stack del pacchetto raster
fiumicino_bande_23 <- stack(import23) 
fiumicino_bande_23 # per vedere nomi delle bande 


# plotto immagine con titolo - immagine colori naturali
# bande rgb con bande del red 4, green 2 e blue 1 (viste dalle indomarmazioni di fiumicino_bande_23
fiumicino23 <- ggRGB(fiumicino_bande_23, 4, 2, 1, stretch="lin") +
      ggtitle("Fiumicino 2023")
fiumicino23 # plot


# SALVATAGGIO 
# png("Fiumicino23.png")
# ggRGB(fiumicino_bande_23, 4, 2, 1, stretch="lin") +
     #  ggtitle("Fiumicino 2023")
 # dev.off()


##### NDBI 

# Normalized Difference Built-up Index - NDBI = (SWIR1-NIR)/(SWIR1+NIR)

# banda 3 = NIR
# banda 5 = SWIR1
ndbi23 = (fiumicino_bande_23[[5]] - fiumicino_bande_23[[3]]) / (fiumicino_bande_23[[5]] + fiumicino_bande_23[[3]])
ndbi23


# PLOT

# utilizzo una palette di colori specifica 
cl <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ndbi23_plot <- plot(ndbi23, col=cl) 
ndbi23_plot


########################### 1993

# Immagini scaricate da https://earthexplorer.usgs.gov/ 
# immagini da Landsat 5

# creo una lista con pattern in comune = banda93
list93 <- list.files(pattern="banda93")
# importo con la funzione lapply del pacchetto raster
import93 <- lapply(list93, raster)
# unisco componenti della list18 in un unico blocco con la funzione raster stack del pacchetto raster
fiumicino_bande_93 <- stack(import93) 
fiumicino_bande_93 # per vedere nomi delle bande


##### NDBI 

# Normalized Difference Built-up Index - NDBI = (SWIR1-NIR)/(SWIR1+NIR)

# banda 3 = NIR
# banda 5 = SWIR1
ndbi93 = (fiumicino_bande_93[[5]] - fiumicino_bande_93[[3]]) / (fiumicino_bande_93[[5]] + fiumicino_bande_93[[3]])
ndbi93

# PLOT

ndbi93_plot <- plot(ndbi93, col=cl)
ndbi93_plot


# MULTIFRAME 

# multiframe tramite funzione par del pacchetto raster 
# con 1 riga e 3 colonne 
par(mfrow=c(1,2))
plot(ndbi23, col=cl) +
 title(main = "2023")
plot(ndbi93, col=cl) +
 title(main = "1993")


###### CALCOLO ALTEZZE

altezze <- brick("per_moltiplicare tagliato.tif") 

#sistema di riferimento
crs(ndbi23)
crs(altezze)

#riproietto altezze
altezze <- projectRaster(altezze, ndbi23)

#moltiplico altezze
nndbi23 <- ndbi23 * altezze
nndbi93 <- ndbi93 * altezze

#nndbi con altezze
par(mfrow=c(1,2))
plot(nndbi23, col=cl) +
 title(main = "2023")
plot(nndbi93, col=cl) +
 title(main = "1993")

####### LANDCOVER

# 8 classi

landcover23 <- unsuperClass(nndbi23, nClasses=8)
landcover93 <- unsuperClass(nndbi93, nClasses=8)

# visualizzo le informazioni
landcover23
landcover93

# plotto la mappa con le classificazioni  
plot(landcover23$map) 
plot(landcover93$map)
# NB: i colori della seconda immagine rappresentano classi diverse rispetto ai colori della prima immagine 

# creo un multiframe con 1 riga e 3 colonne con la funzione par del pacchetto raster
# per visualizzare meglio le differenze 
par(mfrow=c(1,2))
plot(landcover23$map, col=cl) +
 title(main = "2023")
plot(landcover93$map, col=cl) +
 title(main = "1993")

# PERCENTUALI

freq(landcover23$map)
      value  count
# [1,]     1  19993
# [2,]     2   7930
# [3,]     3  10074
# [4,]     4 143275 NON URBANO 
# [5,]     5  17000
# [6,]     6  23308
# [7,]     7  13019
# [8,]     8   2821
# [9,]    NA 294056

> freq(landcover93$map)
      value  count
# [1,]     1  19537
# [2,]     2  13889 
# [3,]     3 146348 NON URBANO
# [4,]     4   3410
# [5,]     5   8153
# [6,]     6  26843
# [7,]     7  13819
# [8,]     8   5421
# [9,]    NA 294056

nndbi23
# dimensions : 1034, 514, 531476  (nrow, ncol, ncell)

tot <- 531476

# percentuale non urbano 
non_urbano23 <- 143275 * 100 / tot
non_urbano93 <- 146348 * 100 /tot

non_urbano23
# 26.95794
non_urbano93
# 27.53614

# percentuali urbano 
urbano23 <- 100 - non_urbano23
urbano93 <- 100 - non_urbano93

urbano23
# 73.04206
urbano93
# 72.46386

