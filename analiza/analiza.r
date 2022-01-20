# 4. faza: Napredna analiza podatkov


# CLUSTERING ###################################################################

#podatki za zdravstvo po letu 2011 vedno slabši zato je najbolj optimalno leto za analizo 2011


# tabela:

X <- zivljenje %>% filter(leto == "2011") %>% filter(!drzava %in% c("Bulgaria","Luxembourg"))
X$drzava[X$drzava == "Czech Republic"] <- "Czech Rep."
X.norm <- X %>% dplyr::select(pricakovana.starost, kg.na.osebo, odstotek.BDP.ki.gre.v.zdravstvo, tveganje.revscine)%>% 
  scale()
rownames(X.norm) <- X$drzava
X.norm <- X.norm[1:29, -1]



## HIERARHIČNO

#funkcije s predavanj:


hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    )+
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    )+
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    )+
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...2)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}


library(rgeos)
library(ggalt)


dendrogram <- dist(X.norm) %>% hclust(method = "ward.D")

r = hc.kolena(dendrogram)
diagram.kolena(r)
# -> 4 skupine


#zakomentirano da ne izpisuje v rmd
#plot(dendrogram, hang=-1, cex=0.4, main = "drzava", labels = X$drzava)
#rect.hclust(dendrogram,k=4,border="red")

skupine = dendrogram %>% cutree(k = 4) %>% as.ordered()
skupine

diagram.skupine(X.norm, colnames(X.norm), skupine, 2 )

#drzave <- attributes(X.norm)$dimnames[[1]]  # dobimo imena, ki jih spremenimo potem v stolpce (to ni df)
#drzave.x.y = as_tibble(X.norm %>% cmdscale(k = 2)) #%>% bind_cols(drzave) %>%
#  rename(drzava = ...3, x = V1, y = V2)


# Narišemo države tako, da jih pobarvamo glede na pripadnost skupini
#  diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, 4)


data("World")
evropa <- World %>% filter (continent == 'Europe')
skupina <- data.frame(name = X$drzava, skupine=factor(skupine))
zem <- merge(x = evropa, y =  skupina)
grupiranje.map <- tm_shape(zem) + tm_polygons("skupine", palette = "Pastel1", popup.vars = c("Skupina: " = "skupine"))+tmap_mode("view")

grupiranje.map


# NAPOVED ######################################################################

# za napoved pricakovane zivljenjske dobe v Sloveniji za leto 2020 
# uvozila nove podatke, z daljšim časovnim obdobjem

pricakovana.slo <- read_csv("podatki/HFA_43_EN.csv",
                        skip = 25,
                        na = "-",
                        locale = locale(encoding = "Windows-1250"))%>% 
  filter(COUNTRY == "SVN")%>%
  dplyr::select(YEAR, VALUE )

library(ranger)

Lag <- function(x, n){c(rep(NA, n), x)[1:length(x)]}

naredi.df <- function(x){
  data.frame(pricakovana = x,
             pricakovana1 = Lag(x, 1),
             pricakovana2 = Lag(x, 2),
             pricakovana3 = Lag(x, 3),
             pricakovana4 = Lag(x, 4))
             
}

df <- naredi.df(pricakovana.slo$VALUE)
model = ranger(formula = pricakovana ~ ., data = df %>% drop_na())

n = nrow(df)

df2 <- naredi.df(c(pricakovana.slo$VALUE, NA))
napoved <- predict(model, data = df2[n+1,])$predictions
df2[n+1,1] = napoved

pricakovanje <- as_tibble(data.frame(
  c(1985:2020),
  df2$pricakovana))


napoved.graf <- ggplot(pricakovanje) + geom_line(mapping = aes(x = c.1985.2020., y = df2.pricakovana), color = "red")+
  geom_line(mapping = aes(x = c.1985.2020., y = df2.pricakovana, color = c.1985.2020. <= 2019), show.legend = FALSE)+
  labs(
    x = "Leto",
    y = "Pričakovana življenjska doba",
    title = "Gibanje pričakovane življenjske dobe v Sloveniji in napoved za leto 2020"
  )




#### 
#samo da ugotovim korelacijo:
ggpairs(data.frame(X.norm))
