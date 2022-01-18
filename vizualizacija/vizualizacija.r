# 3. faza: Vizualizacija podatkov

#-------------------------------------------------------------------------------
#GRAFI

#spol:

zdruzitev <- zivljenje.po.spolu %>% 
  dplyr::select(drzava, leto,spol, pricakovana.starost, zdrava.leta) %>%
  pivot_longer(cols = c(pricakovana.starost, zdrava.leta),
               names_to = "ID", 
               values_to = "leta")
zdruzitev$drzava[zdruzitev$drzava == "United Kingdom"] <- "UK"
zdruzitev$drzava[zdruzitev$drzava == "Czech Republic"] <- "Czech Rep."
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
    y = "Pričakovana življenjska doba ob rojstvu in število zdravih let",
    title = "Gibanje življenjske dobe ob rojstvu \nin števila zdravih let, glede na spol in leto v Evropi"
  )+
  theme(
    axis.text.x = element_text(angle = 50, vjust = 0.5),
    axis.title.x = element_text(vjust = 0))

leta.po.drzavah.graf


odstotek.po.spolu.graf <- zivljenje.po.spolu%>% filter(leto == "2018") %>% 
  ggplot(mapping = aes(x = spol, y = odstotek.zdravih.od.pricakovanih)) +
  geom_boxplot()+
  ylab("Odstotek zdravih let od pričakovane življenjske dobe")+
  xlab("Spol")+
  ggtitle("Odstotek zdravih let življenja od pričakovane življenjske dobe glede na spol za leto 2018")+
  scale_x_discrete(labels=c("Ženske","Moški"))

odstotek.po.spolu.graf



# samo pričakovana starost in zdrava leta, po državah oz. letih

pricakovana.2018.graf <- zivljenje %>%filter(leto == "2018")%>%
  ggplot()+
  geom_bar(mapping = aes(x = reorder(drzava, -pricakovana.starost),
                               y = pricakovana.starost),
                 stat = "identity")+
  ylab("Pričakovana življenjska doba ob rojstvu")+
  xlab("Država")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 50, vjust = 0.5),
    axis.title.x = element_text(vjust = 0))+
  ggtitle("Pričakovana življenjska doba po državah leta 2018")+
  geom_hline(yintercept=mean(filter(zivljenje, leto == "2018")$pricakovana.starost),color="red")
pricakovana.2018.graf
  


# pricakovana leta in BDP, sadje in zelenjava, revscina


pricakovana.in.BDP.graf <- zivljenje %>%filter(!is.na(odstotek.BDP.ki.gre.v.zdravstvo))%>% filter(leto == "2011") %>%
  ggplot() +
  aes(x = odstotek.BDP.ki.gre.v.zdravstvo, y = pricakovana.starost, color = zdrava.leta) +
  geom_point()+
  labs(
    x ="% BDP, ki ga država nameni v zdravstvo",
    y = "Pričakovana življenjska doba",
    title = "Pričakovana življenjska doba v odvisnosti od % BDP namenjenega v zdravstvo leta 2011",
    color = "Zdrava leta"
  )+
  scale_color_gradient(low = "blue", high = "red")
  
pricakovana.in.BDP.graf

pricakovana.in.sadje.graf <- ggplot(zivljenje %>% filter(!is.na(kg.na.osebo)) %>% filter(leto == "2011"))+
  aes(x = kg.na.osebo, y = pricakovana.starost, color = zdrava.leta) +
  geom_point()+
  labs(
    x = "Sadje in zelenjava, ki je na voljo na eno osebo v enem letu, v kg",
    y = "Pričakovana življenjska doba",
    color = "Zdrava leta", 
    title = "Pričakovana življenjska doba in količina sadja in zelenjave na osebo leta 2011",
    color = "Zdrava leta"
  )+ scale_color_gradient(low = "blue", high = "red")

pricakovana.in.sadje.graf



pricakovana.in.revscina.graf <- ggplot(zivljenje %>% filter(!is.na(tveganje.revscine))%>%filter(leto == "2011")) +
  aes(x = tveganje.revscine, y = pricakovana.starost, color = zdrava.leta)+
  geom_point()+
  labs(
    x = "tveganje revščine",
    y = "pričakovana življenjska doba",
    title = "Pričakovana življenjska doba ob rojstvu glede na tveganje revščine v letu 2011",
    color = "Zdrava leta"
  )+ scale_color_gradient(low = "blue", high = "red")

pricakovana.in.revscina.graf



#Dodatni grafi 
# pricakovana leta po državah čez leta
ggplot(zivljenje) + aes(x = leto, y= pricakovana.starost, color = drzava) + geom_line() 



# odstotek po spolu;
ggplot(zivljenje.po.spolu) +
  aes(x = leto, y = odstotek.zdravih.od.pricakovanih, color = spol)+
  geom_line()+
  facet_wrap(.~drzava)+ 
  ggtitle("Evropa")+
  ylab("Odstotek zdravih let življenja od pričakovane starosti")+
  scale_color_manual("Spol",
                     values = c("#F8766D", "#00BFC4"),
                     labels = c("ženske", "moški"))


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


#-------------------------------------------------------------------------------
#ZEMLJEVIDI

data("World")
zivljenje1 <- zivljenje
zivljenje1$drzava[zivljenje1$drzava == "Czech Republic"] <- "Czech Rep."



map <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  starost <- zivljenje1 %>% filter (leto =="2018") %>% dplyr::select('drzava', 'pricakovana.starost', "zdrava.leta")
  podatki <- merge(y = starost,x = evropa, by.x='name', by.y = 'drzava')
  evropa <- tm_shape(podatki) +
    tm_polygons(c('pricakovana.starost','zdrava.leta'),
                popup.vars = c("Pričakovana starost: " = "pricakovana.starost", "Zdrava leta:" = "zdrava.leta")) + 
    tm_facets(sync = TRUE)
    tmap_mode('view')
  return(evropa)
}
map()




#pomožni zemljevid:


odstotek.map <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  starost <- zivljenje1 %>% filter (leto == 2018) %>% dplyr::select('drzava', 'odstotek.zdravih.od.pricakovanih')
  podatki <- merge(y = starost,x = evropa, by.x='name', by.y = 'drzava')
  evropa <- tm_shape(podatki) + tm_polygons('odstotek.zdravih.od.pricakovanih') 
  tmap_mode('view')
  return(evropa)
}

odstotek.map()




