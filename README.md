# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza pričakovane življenjske dobe in števila zdravih let v evropskih državah

V projektu bom analizirala pričakovano življenjsko dobo ob rojstvu in število zdravih let v evropskih državah. Rada bi ugotovila povezavo pričakovane življenjske dobe in števila zdravih let s povprečno količino sadja in zelenjave, ki je na voljo na osebo, skupnimi izdatki države za zdravstvo kot % BDP in stopnjo tveganja revščine v državi.

Viri podatkov:
* [Eurostat](https://ec.europa.eu/eurostat/data/database)
* [World health organization](https://gateway.euro.who.int/en/datasets/european-health-for-all-database/)

### Tabele:
Tabela 1: Pričakovana življenjska doba in število zdravih let  
* Država
* Leto
* Pričakovana življenska doba
* Število zdravih let

Tabela 2: Povprečna količina sadja in zelenjave, ki je na voljo na osebo  
* Država
* Leto
* Povprečna količina sadja in zelenjave, ki je na voljo na osebo(kg)

Tabela 3: Skupni izdatki za zdravstvo kot % BDP
* Država
* Leto
* Skupni izdatki za zdravstvo kot % BDP

Tabela 4: Stopnja tveganja revščine
* Država
* Leto
* Stopnja tveganja revščine

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
