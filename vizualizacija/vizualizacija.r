# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(mapproj)

zivljenje <- read_csv("podatki/zivljenje.csv")
zivljenje.po.spolu <- read_csv("podatki/zivljenje-po-spolu.csv")




################################################################################
# grafi

# skupaj pricakovana starost in zdrava leta
ggplot(zivljenje.po.spolu) +
  geom_line(aes(x = leto, y = pricakovana.starost, color = spol)) + 
  geom_line(aes(x = leto, y = zdrava.leta, color = spol))+
  facet_wrap(.~drzava)+
  ylab("pričakovana starost ob rojstvu in število zdravih let")


# odstotek po spolu;
ggplot(zivljenje.po.spolu) +
  aes(x = leto, y = odstotek.zdravih.od.pricakovanih, color = spol)+
  geom_line()+
  facet_wrap(.~drzava)+ 
  ggtitle("Odstotek zdravih let življenja od pričakovane starosti")+
  ylab("Odstotek zdravih let življenja od pričakovane starosti")
  
#zdrava leta in pričakovana starost splošno:
ggplot(zivljenje) +
  geom_line(aes(x = leto, y = pricakovana.starost), color = "red")+
  geom_line(aes(x = leto, y = zdrava.leta), color = "blue")+
  facet_wrap(.~drzava)

ggplot(zivljenje) +
  aes(x = leto, y = pricakovana.starost)




#-------------------------------------------------------------------------------
#Zemljevidi

svet.sp = readOGR("podatki/zemljevidi/TM_WORLD_BORDERS-0.3.shp", "TM_WORLD_BORDERS-0.3")
svet.sp = gBuffer(svet.sp, byid = TRUE, width = 0)
svet.map = svet.sp %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

svet = tibble(svet.map@data)

svet.poligoni = svet.map %>% fortify() %>%
  left_join(
    rownames_to_column(svet.map@data),
    by = c("id" = "rowname")
  ) %>%
  dplyr::select(
    drzava = NAME, long, lat, order, hole, piece, id, group
  ) %>%
  mutate(
    drzava = replace(
      drzava,
      drzava == "The former Yugoslav Republic of Macedonia",
      "North Macedonia"
    )
  )

svet.poligoni %>% write_csv("podatki/drzave-poligoni.csv")

svet.centroidi = svet.map %>% coordinates() %>% as.data.frame()
colnames(svet.centroidi) = c("long", "lat")

svet.centroidi = rownames_to_column(svet.centroidi) %>%
  left_join(
    rownames_to_column(svet.map@data),
    by = "rowname"
  ) %>%
  dplyr::select(
    drzava = NAME, long = LON, lat = LAT
  ) %>%
  mutate(
    drzava = replace(
      drzava,
      drzava == "The former Yugoslav Republic of Macedonia",
      "North Macedonia"
    )
  )
svet.centroidi %>% write_csv("podatki/drzave-centroidi.csv")


evropske.drzave = tibble(
  drzava = c(
    "Albania", "Andorra", "Armenia",
    "Austria", "Azerbaijan", "Belarus",
    "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus",
    "Czech Republic", "Denmark", "Estonia",
    "Finland", "France", "Georgia",
    "Germany", "Greece", "Hungary",
    "Iceland", "Ireland", "Italy",
    "Kazakhstan", "Latvia",
    "Liechtenstein", "Lithuania",
    "Luxembourg", "Malta", "Moldova",
    "Monaco", "Montenegro",
    "Netherlands", "North Macedonia",
    "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino",
    "Serbia", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "United Kingdom",
    "Holy See (Vatican City)"
  )
)

evropa.izsek = as(extent(-25, 60, 30, 75), "SpatialPolygons")
sp::proj4string(evropa.izsek) <- sp::proj4string(svet.sp)

evropa.poligoni = svet.sp %>% crop(evropa.izsek) %>% fortify() %>%
  left_join(
    rownames_to_column(svet.map@data),
    by = c("id" = "rowname")
  ) %>%
  dplyr::select(
    drzava = NAME, long, lat, order, hole, piece, id, group
  ) %>%
  mutate(
    drzava = replace(
      drzava,
      drzava == "The former Yugoslav Republic of Macedonia",
      "North Macedonia"
    )
  ) %>%
  left_join(
    evropske.drzave,
    by = "drzava"
  )

evropa.centroidi = evropske.drzave %>%
  left_join(
    svet.centroidi,
    by = "drzava"
  )

evropa.poligoni %>%
  ggplot() +
  geom_polygon(
    mapping = aes(long, lat, group = group),
    color = "grey",
    fill = "white"
  ) +
  coord_map() +
  geom_text(
    data = evropa.centroidi,
    mapping = aes(x = long, y = lat, label = drzava),
    size = 1.5
  ) +
  xlim(-25, 60) +
  ylim(30, 75) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )





#zemljevidi za leto 2018, saj je zadnje leto s celostnimi podatki

pricakovana.starost.map <- zivljenje %>% 
  filter(leto == "2018")%>%
  dplyr::select(drzava, pricakovana.starost)%>%
  full_join(evropa.poligoni, by = "drzava") %>%
  ggplot() +
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = pricakovana.starost ),
    color = "grey",
  ) +
  coord_map() +
  geom_text(
    data = evropa.centroidi,
    mapping = aes(x = long, y = lat, label = drzava),
    size = 1.5
  ) +
  xlim(-25, 60) +
  ylim(30, 72) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )+
  ggtitle("Pričakovana starost ob rojstvu v Evropi, leto 2018")+
  labs(fill="Pričakovana starost")+
  ggsave("slike/pricakovana-starost-map.pdf", dev = cairo_pdf, width = 9, height = 6)
  



zdrava.leta.map <- zivljenje %>% 
  filter(leto == "2018")%>%
  dplyr::select(drzava, zdrava.leta)%>%
  full_join(evropa.poligoni, by = "drzava") %>%
  ggplot() +
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = zdrava.leta ),
    color = "grey",
  ) +
  coord_map() +
  geom_text(
    data = evropa.centroidi,
    mapping = aes(x = long, y = lat, label = drzava),
    size = 1.5
  ) +
  xlim(-25, 60) +
  ylim(30, 72) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )+
  ggtitle("Zdrava leta v Evropi, leto 2018")+
  labs(fill="Zdrava leta")+
  ggsave("slike/zdrava-leta-map.pdf", dev = cairo_pdf, width = 9, height = 6)





odstotek.zdravih.map <- zivljenje %>% 
  filter(leto == "2018")%>%
  dplyr::select(drzava, odstotek.zdravih.od.pricakovanih)%>%
  full_join(evropa.poligoni, by = "drzava") %>%
  ggplot() +
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = odstotek.zdravih.od.pricakovanih ),
    color = "grey"
  ) +
  coord_map() +
  geom_text(
    data = evropa.centroidi,
    mapping = aes(x = long, y = lat, label = drzava),
    size = 1.5
  ) +
  xlim(-25, 60) +
  ylim(30, 72) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )+
  ggtitle("Odstotek zdravih let od pričakovane starosti, leto 2018")+
  labs(fill="Odstotek")+
  ggsave("slike/odstotek-zdravih-od-pricakovanih-map.pdf", dev = cairo_pdf, width = 9, height = 6)





