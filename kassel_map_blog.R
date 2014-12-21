
#packages####
library("maptools")
library("plyr")
library("dplyr")
library("RColorBrewer")
library("osmar")
library("XML")

#loading shapefile####
shapes <- readShapeSpatial("plz/post_pl.shp")

#subsetting only Kassel shapes
ks_shapes <- shapes[shapes$PLZORT99 == "Kassel",]

plot(ks_shapes)
#data####
#connecting districts with postalcodes
districts_ks <- read.csv2("data/mapping_postalcodes_districts_kassel.csv", stringsAsFactors = FALSE)
districts_ks

districts_ks


districts_data <- read.csv2("data/districts_information.csv", stringsAsFactors = FALSE)

districts_data

#correction of district names in districts_data
districts_data$District[districts_data$District == "Nord (Holland)"] <- "Nord-Holland"
districts_data$District[districts_data$District == "Philippinenhof/Warteberg"] <- "Philippinenhof-Warteberg"
districts_data$District[districts_data$District == "Süsterfeld/Helleböhn"] <- "Süsterfeld"
districts_data$District[districts_data$District == "Wolfsanger/Hasenhecke"] <- "Wolfsanger-Hasenhecke"

#merging datasets
districts_data <- merge(districts_data, districts_ks, all.x = TRUE)

#count number of postal code areas each district intersects with
districts_data <- ddply(districts_data, 
                        c("District"), 
                        transform, 
                        cntPostalcode = length(Households))


#aggregation by postalcode, accounting for partial overlaps
data.agg <- ddply(districts_data, 
                  c("Postalcode"), 
                  summarize, 
                  District = paste(unique(District),collapse = " & "), 
                  Households = sum(Households / cntPostalcode), 
                  cars = sum(private.used.cars.per.1000.residents / cntPostalcode))

data.agg


#match postalcodes in shapes with postalcodes in data
position <- match(ks_shapes$PLZ99_N, data.agg$Postalcode)

summary(data.agg$Households)


#define classes for number of households
classes <- cut(data.agg$Households[position], c(0,2000,5000,10000,15000,20000))
levels(classes) <- c("under 2000", "2000 - 5000", "5000 - 10000", "10000 - 15000","15000 - 20000")

colours <- c("#73AC97", "#489074", "#267356", "#0E563B", "#003924")

#plot#####
par(mar = c(0,0,1,9),
    oma = c(3,0,2,1),
    bg = "lightgrey",
    xpd = TRUE)


plot(ks_shapes,border = "darkgrey", col = colours[classes])
text(coordinates(ks_shapes), labels =ks_shapes$PLZ99_N, col = "white")

legend(par("usr")[1],par("usr")[3], pch = 15, col = colours, legend = levels(classes),ncol = 1, bty = "n", title = "Households",
       xjust = -3.4, yjust = -1.8)
title("Households in Kassel")




#konrad stations########

#getting Kassel osm data from http://openstreetmap.de/
src <- osmsource_file("C:/Users/msc.EODA/workspace/geodaten_visualisierung/Kassel.osm")

#osmar way####
#this takes a while
# OBJ <- get_osm(complete_file(), source = src)
# 
# #load("kasselOsmar.RData")
# 
# #find konrad nodes, use %agrep% for fuzzy string matching
# konrad_id <- find(OBJ, node(tags(v %agrep% "Konrad")))
# 
# #build subset of osmar object only for Konrad Nodes
# kon_sub <- subset(OBJ, node_ids = konrad_id)
# 
# #writing coordinates to data.frame
# stations <- data.frame(lat = kon_sub$nodes$attrs$lat,
#                        lon = kon_sub$nodes$attrs$lon,
#                        stringsAsFactors = FALSE)


#xpath way####
osm <- xmlParse("Kassel.osm")


#get nodes which contain a tag where attribute v has value Konrad and then get coordinates
stations <- data.frame(lat = xpathSApply(osm, path = "//node[tag[@v = 'Konrad']]/@lat"),
                       lon = xpathSApply(osm, path = "//node[tag[@v = 'Konrad']]/@lon"),
                       stringsAsFactors = FALSE)


#adding the stations to plot####
points(stations$lon, stations$lat, col = adjustcolor("white",0.4),pch = 19)
legend(par("usr")[1],par("usr")[3], pch = 19, col = adjustcolor("white",0.4), legend = "Konrad\nStations", bty = "n",
       xjust = -5.1, yjust = -4)

par("usr")

mtext("Sources:\ndatamarket.com\nhttp://arnulf.us/PLZ\nopenstreetmap.de", side = 1, line = 1, outer = TRUE, cex = 0.7, col = "darkgrey")








