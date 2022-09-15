library(ggplot2)

g <- ggplot(faithfuld, aes(waiting, eruptions))

g + geom_raster(aes(fill = density))
g + geom_contour(aes(z = density))




library(elevatr)
elev = get_elev_raster(Tamb, z=8)

plot(elev)
Poligo_alt    <- crop(elev, Tamb)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Tamb)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(elev)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


g <- ggplot(Geo_data, aes(x, y))

g + geom_raster(aes(fill = density))
g + geom_contour(aes(z = density))


ggplot()+
  geom_contour(data = Geo_data_frame,aes(x=x, y=y,z = alt))







library(sf)
library(raster)
Peru  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per   <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Tamb =  subset(Peru , NAME_1 == "Madre de Dios")
Dis  =  subset(Per , NAME_1 == "Madre de Dios")

Puer_Mal <- st_read ("SHP/Puerto.shp") # Caragmos un shp de puerto maldonado
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))

ggplot()+
  geom_sf(data =  CetroPo,size=1.5, alpha=0.3, color="black", pch=21, fill="red")+
  geom_sf_text(data = CetroPo_xy , aes(label = NOMBCP ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 3, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  
  coord_sf(xlim = c(-69.26, -69.12), ylim = c(-12.68  , -12.48)) 

library(ggrepel)
library(elevatr)
elev = get_elev_raster(Puer_Mal , z=12)
plot(elev)
Poligo_alt    <- crop(elev, Puer_Mal)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Puer_Mal)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

library(ggnewscale) 
library(ggspatial)

D=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,80,100,120,180, 200, 250),
                       na.value = 'white',
                       labels = c("[0 - 79] ","[80 - 99]", "[100 - 119]",
                                  "[120 - 179]", "[180 - 199]", "[200 - 249]",
                                  "[250 - 300]"),
                       name='Elevacion \n(msnm)')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=1.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_contour(data = Geo_data_frame,aes(x=x, y=y,z = alt), color="#9a031e", size=0.1)+
  geom_sf_label(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  geom_label( x=1990, y=55000, label="Amanda reached 3550\nbabies in 1970", size=4, color="#69b3a2") +
  coord_sf(xlim = c(-69.26, -69.12), ylim = c(-12.68  , -12.48)) +
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = c(0.2, 0.3),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_vline(xintercept = c(-69.26,-69.24,-69.22,-69.20,-69.18,-69.16,-69.14,-69.12), color = "gray50",linetype = "dashed", size = 0.5)+
  geom_hline(yintercept = c(-12.65, -12.60,-12.55,-12.50), color = "gray50",linetype = "dashed", size = 0.5)
legend <- get_legend(D)

DD= D + theme(legend.position = "none")

A=ggplot()+
  geom_sf(data=Peru, fill="white", color="black", size=0.1)+
  geom_sf(data=Tamb, fill="gray", color="black", size=0.1)+
  geom_sf_text(data = Peru, aes(label=NAME_1 ),family="serif", color="black", size =1.5
               ,  fontface="italic")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

B=ggplot()+
  geom_sf(data=Tamb, fill=NA, color="black", size=0.1)+
  geom_sf(data=Dis, fill="white", color="black", size=0.1)+
  geom_sf(data=Puer_Mal, fill="gray", color="black", size=0.1)+
  geom_sf_text(data = Dis, aes(label=NAME_3 ),family="serif", color="black", size =1.5
               ,  fontface="italic")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

library(cowplot)

ggAgru =ggdraw() +
  coord_equal(xlim = c(0, 9), ylim = c(0, 21), expand = FALSE) +
  draw_plot(B , width = 7, height = 7,x = 0.5, y = 0.5)+
  draw_plot(A , width = 9, height = 9,x = 0, y = 7.5)+
  draw_plot(legend , width = 9, height = 9,x = 2.5, y = 16)+
  theme_void()

Mapa=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(DD  , width = 21, height = 21,x = 3, y = 0)+
  draw_plot(ggAgru  , width = 20, height = 20,x = 14.5, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Mapa ,"Mapa/Mapa de curva de nivel.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)











