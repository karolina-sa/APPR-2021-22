library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(plotly)
library(maptools)
library(readr)
library(stringr)
require(data.table)
library(xml2)
library(gridExtra)
library(cluster)
library(ggalt)
library(readxl)
require(maps)
require(viridis)

options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

