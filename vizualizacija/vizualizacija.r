# 3. faza: Vizualizacija podatkov


#zivljenje <- read_csv("podatki/zivljenje.csv")
#zivljenje.po.spolu <- read_csv("podatki/zivljenje-po-spolu.csv")


#-------------------------------------------------------------------------------
#GRAFI

#spol:

zdruzitev <- zivljenje.po.spolu %>% 
  dplyr::select(drzava, leto,spol, pricakovana.starost, zdrava.leta) %>%
  pivot_longer(cols = c(pricakovana.starost, zdrava.leta),
               names_to = "ID", 
               values_to = "leta")
zdruzitev$zdruzeno <- paste(zdruzitev$ID, zdruzitev$spol, sep = "-")


# skupaj pricakovana starost in zdrava leta
leta.po.drzavah.graf <- ggplot(zdruzitev) +
  geom_line(aes(x = leto, y = leta, color = zdruzeno)) + 
  scale_color_manual("",
                     values = c("red", "blue", "#F8766D", "#00BFC4"),
                     labels = c("ženske - pričakovana starost",
                                "moški - pričakovana starost",
                                "ženske - zdrava leta",
                                "moški - zdrava leta") )+
  facet_wrap(.~drzava)+
  labs(
    x = "Leto",
    y = "Pričakovana starost ob rojstvu in število zdravih let",
    title = "Evropa"
  )

leta.po.drzavah.graf


# odstotek po spolu;
odstotek.po.drzavah.graf <- ggplot(zivljenje.po.spolu) +
  aes(x = leto, y = odstotek.zdravih.od.pricakovanih, color = spol)+
  geom_line()+
  facet_wrap(.~drzava)+ 
  ggtitle("Evropa")+
  ylab("Odstotek zdravih let življenja od pričakovane starosti")+
  scale_color_manual("Spol",
                     values = c("#F8766D", "#00BFC4"),
                     labels = c("ženske", "moški"))

odstotek.po.drzavah.graf

# samo pričakovana starost in zdrava leta, po državah oz. letih

pricakovana.2018.graf <- zivljenje %>%filter(leto == "2018")%>%
  ggplot()+
  geom_bar(mapping = aes(x = reorder(drzava, -pricakovana.starost),
                               y = pricakovana.starost),
                 stat = "identity")+
  ylab("Pričakovana starost ob rojstvu")+
  xlab("Država")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 50, vjust = 0.5),
    axis.title.x = element_text(vjust = 0))+
  ggtitle("Pričakovana starost po državah leta 2018")+
  geom_hline(yintercept=mean(filter(zivljenje, leto == "2018")$pricakovana.starost),color="red")

pricakovana.2018.graf
  
#pricakovana.2018.graf %>% ggsave("slike/pricakovana-2018.pdf" , dev = cairo_pdf, width = 9, height = 6)


zivljenje %>% ggplot(mapping = aes(x = as.character(leto), y = zdrava.leta))+ 
  geom_boxplot()+
  labs(
    x = "leto",
    y = "zdrava leta"
  )

zivljenje %>% ggplot(mapping = aes(x = as.character(leto), y = pricakovana.starost))+
  geom_boxplot()+
  labs(
    x = "leto",
    y = "pričakovana starost"
  )


# pricakovana leta in BDP, sadje in zelenjava, revscina


pricakovana.in.BDP.graf <- zivljenje %>%filter(odstotek.BDP.ki.gre.v.zdravstvo != "NA")%>% #filter(leto == "2011") %>%
  ggplot() +
  aes(x = odstotek.BDP.ki.gre.v.zdravstvo, y = pricakovana.starost, color = zdrava.leta) +
  geom_point()+
  labs(
    x ="% BDP, ki ga država nameni v zdravstvo",
    y = "Pričakovana starost",
    title = "Pričakovana starost v odvisnosti od % BDP namenjenega v zdravstvo",
    color = "Zdrava leta"
  )+
  scale_color_gradient(low = "black", high = "green")
pricakovana.in.BDP.graf


pricakovana.in.sadje.graf <- ggplot(zivljenje %>% filter(kg.na.osebo != "NA"))+
  aes(x = kg.na.osebo, y = pricakovana.starost, color = zdrava.leta) +
  geom_point()+
  labs(
    x = "sadje in zelenjava, ki je na voljo na eno osebo v enem letu, v kg",
    y = "pričakovana starost",
    color = "zdrava leta"
  )+ scale_color_gradient(low = "black", high = "green")

pricakovana.in.sadje.graf



pricakovana.in.revscina.graf <- ggplot(zivljenje %>% filter(tveganje.revscine != "NA")) +
  aes(x = tveganje.revscine, y = pricakovana.starost, color = zdrava.leta)+
  geom_point()+
  labs(
    x = "tveganje revščine",
    y = "pričakovana starost"
  )

pricakovana.in.revscina.graf



#dodatni grafi
# pricakovana leta po državah čez leta
ggplot(zivljenje) + aes(x = leto, y= pricakovana.starost, color = drzava) + geom_line() 


zivljenje.po.spolu%>% filter(leto == "2018") %>% ggplot(mapping = aes(x = spol, y = pricakovana.starost)) + geom_boxplot()
zivljenje.po.spolu%>% filter(leto == "2018") %>% ggplot(mapping = aes(x = spol, y = odstotek.zdravih.od.pricakovanih)) + geom_boxplot()




#-------------------------------------------------------------------------------
#Zemljevidi

data("World")
zivljenje1 <- zivljenje
zivljenje1$drzava[zivljenje1$drzava == "Czech Republic"] <- "Czech Rep."

pricakovana.starost.map <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  starost <- zivljenje1 %>% filter (leto == 2018) %>% dplyr::select('drzava', 'pricakovana.starost')
  podatki <- merge(y = starost,x = evropa, by.x='name', by.y = 'drzava')
  evropa <- tm_shape(podatki) + tm_polygons('pricakovana.starost', popup.vars = c("Pričakovana starost: " = "pricakovana.starost"))
  tmap_mode('view')
  return(evropa)
}
pricakovana.starost.map()




zdrava.leta.map <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  starost <- zivljenje1 %>% filter (leto == 2018) %>% dplyr::select('drzava', 'zdrava.leta')
  podatki <- merge(y = starost,x = evropa, by.x='name', by.y = 'drzava')
  evropa <- tm_shape(podatki) + tm_polygons('zdrava.leta')
  tmap_mode('view')
  return(evropa)
}
zdrava.leta.map()


odstotek.map <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  starost <- zivljenje1 %>% filter (leto == 2018) %>% dplyr::select('drzava', 'odstotek.zdravih.od.pricakovanih')
  podatki <- merge(y = starost,x = evropa, by.x='name', by.y = 'drzava')
  evropa <- tm_shape(podatki) + tm_polygons('odstotek.zdravih.od.pricakovanih') 
  tmap_mode('view')
  return(evropa)
}

odstotek.map()


