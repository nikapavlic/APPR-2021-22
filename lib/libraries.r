library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)

#uvoz:
library(readxl)
library(stringr)


#vizualizacija:
library(ggplot2)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(mapproj)
library(mosaic)
library(maptools)

# analiza
library(GGally)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
