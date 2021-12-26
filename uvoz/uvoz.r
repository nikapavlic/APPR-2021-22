# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

library(readr)
library(tibble)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)



#tabela s pričakovanimi leti življenja in zdravih let

leta.zivljenja.po.spolu <- read_csv("podatki/hlth_hlye_1_Data.csv", na = c("-"), locale = locale(encoding = "Windows-1250")) %>%
  select(TIME, GEO, SEX, INDIC_HE, Value) %>% 
  mutate(Value = parse_number(Value)) %>%
  pivot_wider(names_from = INDIC_HE, values_from = Value) %>%
  rename(leto = TIME, drzava = GEO, spol = SEX,
         pricakovana.starost = "Life expectancy in absolute value at birth",
         zdrava.leta ="Healthy life years in absolute value at birth")


leta.zivljenja.po.spolu$drzava <- str_replace_all(leta.zivljenja.po.spolu$drzava, "Germany \\(until 1990 former territory of the FRG\\)","Germany")
leta.zivljenja.po.spolu$odstotek.zdravih.od.pricakovanih <- round((leta.zivljenja.po.spolu$zdrava.leta / leta.zivljenja.po.spolu$pricakovana.starost) * 100, 1)

#-------------------------------------------------------------------------------
# tabela s pričakovanimi leti življenja in zdravih let, neodvisno od spola

leta.zivljenja <- leta.zivljenja.po.spolu %>% 
  group_by(leto, drzava) %>% 
  summarise(pricakovana.starost = mean(pricakovana.starost),
            zdrava.leta = mean(zdrava.leta),
            odstotek.zdravih.od.pricakovanih = mean(odstotek.zdravih.od.pricakovanih) )

#-------------------------------------------------------------------------------

#tabela s imeni in kraticami držav:
oznake.drzav <- read_xlsx("podatki/oznake_drzav.xlsx")%>%
  select("Code", "Short name")%>%
  rename(kratica = Code, drzava = "Short name")

#-------------------------------------------------------------------------------

#tabela s podatki, koliko sadja in zelenjave je na voljo na eno osebo v kg

sadje.in.zelenjava <- read_csv("podatki/HFA_446_EN.csv", skip = 25, na = "-", locale = locale(encoding = "Windows-1250"))%>% 
  filter(COUNTRY != "", YEAR > 2003) %>%
  select(COUNTRY,YEAR, VALUE)%>%
  rename(kratica = COUNTRY, leto = YEAR, kg.na.osebo = VALUE)%>%
  left_join(oznake.drzav)%>%
  select(leto, drzava, kg.na.osebo)

#-------------------------------------------------------------------------------

#tabela s podatki, koliko % BDP država nameni v zdravstvo
izdatki.za.zdravstvo <- read_csv("podatki/HFA_566_EN.csv", skip = 25, na = "-", locale = locale(encoding = "Windows-1250"))%>%
  filter(COUNTRY != "", YEAR > 2003) %>%
  select(COUNTRY, YEAR, VALUE)%>%
  rename(kratica = COUNTRY, leto = YEAR, odstotek.BDP.ki.gre.v.zdravstvo =VALUE)%>%
  left_join(oznake.drzav)%>%
  select(leto, drzava, odstotek.BDP.ki.gre.v.zdravstvo)
  
#-------------------------------------------------------------------------------

#tveganje revščine
tveganje.revscine.po.spolu <- read_csv("podatki/ilc_li02_1_Data.csv", na = "-", locale = locale(encoding = "Windows-1250")) %>%
  select(TIME, GEO, SEX, Value)%>%
  mutate(Value = parse_number(Value))%>%
  rename(leto = TIME, drzava = GEO, spol = SEX, tveganje.revscine = Value)

tveganje.revscine.po.spolu$drzava <- str_replace_all(tveganje.revscine.po.spolu$drzava,
                                                     "Germany \\(until 1990 former territory of the FRG\\)","Germany" )
tveganje.revscine.po.spolu$drzava <- str_replace_all(tveganje.revscine.po.spolu$drzava,
                                                     "Kosovo \\(under United Nations Security Council Resolution 1244/99\\)","Kosovo")

#-------------------------------------------------------------------------------

#tveganje revščine neodvisno od spola

tveganje.revscine <- tveganje.revscine.po.spolu %>% group_by(leto, drzava)%>%
  summarise(tveganje.revscine = mean(tveganje.revscine))


#-------------------------------------------------------------------------------
#KONČNE TABELE

# s spolom:

zivljenje.po.spolu <- left_join(leta.zivljenja.po.spolu, tveganje.revscine.po.spolu)

zdruzeno <- leta.zivljenja %>%
  left_join(sadje.in.zelenjava)%>%
  left_join(izdatki.za.zdravstvo)%>%
  left_join(tveganje.revscine)






