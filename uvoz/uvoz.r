library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rvest)
require(dplyr)
require(data.table)
#require(httr)

#install.packages('xml2')
#library('xml2')

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# ==============================================================================
# ==============================================================================

prenocitve.po.drzavah.letno <- read_csv("prenočitve_po_drzavah_letno.csv", 
                               locale = locale(encoding = "Windows-1250"),
                               col_names=TRUE, 
                               col_types = cols(.default = col_guess()))

prenocitve.po.drzavah.letno <- prenocitve.po.drzavah.letno %>% 
  pivot_longer(
    cols = colnames(prenocitve.po.drzavah.letno)[-1],
    names_to = "Leto",
    values_to = "Število prenočitev"
)
  
stevilo.prenocitev.letno <- prenocitve.po.drzavah.letno[1:11, 2:3] 

# ==============================================================================


#stevilo.zaposlenih <- read_html("stevilo_zaposlenih_v_turizmu_vec_let.html", skip = 0, remove.empty = TRUE, trim = TRUE)


# ==============================================================================
# ==============================================================================

odhod.slovencev.v.tujino <- read.csv("odhod_slovencev_v_tujino_po_drzavah_letno.csv",
                                     na.strings = c("N", "-"))
                                     # locale = locale(encoding = "Windows-1250"),
                                     # col_names=TRUE,
                                     # col_types = cols(
                                     #   .default = col_guess(),
                                     #   "X2012" = col_double(),
                                     #   "X2013" = col_double(),
                                     #   "X2014" = col_double(),
                                     #   "X2015" = col_double(),
                                     #   "X2016" = col_double(),
                                     #   "X2017" = col_double(),
                                     #   "X2018" = col_double(),
                                     #   "X2019" = col_double(),
                                     #   "X2020" = col_double()
                                     # )
                                     #  )

odhod.slovencev.v.tujino <- odhod.slovencev.v.tujino %>%
  pivot_longer(cols = colnames(odhod.slovencev.v.tujino)[-c(1,2,3)],
               names_to = "Leto",
               values_to = "Število") %>%
  mutate(
    Leto = str_replace_all(Leto, "(X)(\\d{4})", "\\2")
  )

names(odhod.slovencev.v.tujino) <- c("Vrsta turističnega potovanja", "Država", "Meritev", "Leto", "Število")


# razdelim tebelo na nočitve ter izdatke:

odhod.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
                                                               'Povprečni izdatki na turista na prenočitev (EUR)',]
odhod.slovencev.v.tujino.izdatki.na.turista <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
                                                                          'Povprečno število prenočitev',]

#razdelim tabelo nocitev na zasebna in poslovna potovanja:
zasebna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[1:117,] %>%
  select(Država, Leto, Število) %>%
  rename("Zasebna potovanja" = "Število")
poslovna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[118:234,] %>%
  select(Država, Leto, Število) %>%
  rename("Poslovna potovanja" = "Število")

# ==============================================================================

# 4
# ODHOD SLOVENCEV V TUJINO

POTOVANJA.SLOVENCEV.V.TUJINO.NOCITVE <- zasebna.potovanja.slovencev.v.tujino.nocitve %>%
  full_join(poslovna.potovanja.slovencev.v.tujino.nocitve, by=c("Država", "Leto")) 

POTOVANJA.SLOVENCEV.V.TUJINO.NOCITVE$Skupaj <- POTOVANJA.SLOVENCEV.V.TUJINO.NOCITVE$`Zasebna potovanja` +
  POTOVANJA.SLOVENCEV.V.TUJINO.NOCITVE$`Poslovna potovanja`


# ==============================================================================
# ==============================================================================

izdatki.tujcev <- read_csv("izdatki_tujcev_za_turisticno_potrosnjo_v_Sloveniji.csv", 
                           locale = locale(encoding = "Windows-1250"),
                           col_names=TRUE, 
                           col_types = cols(.default = col_guess()))

izdatki.tujcev <- izdatki.tujcev %>% 
  pivot_longer(cols = colnames(izdatki.tujcev)[-1],
               names_to = "leto", 
               values_to = "izdatki.tujcev.v.sloveniji" ) %>%
  mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
  select(-c(1))

# ==============================================================================

izdatki.slovencev.v.sloveniji <- read_csv("izdatki_slovencev_za_turisticno_potrosnjo_v_Sloveniji.csv", 
                           locale = locale(encoding = "Windows-1250"),
                           col_names=TRUE, 
                           col_types = cols(.default = col_guess()))

izdatki.slovencev.v.sloveniji <- izdatki.slovencev.v.sloveniji %>%
  pivot_longer(cols = colnames(izdatki.slovencev.v.sloveniji)[-1],
               names_to = "leto",
               values_to = "izdatki.slovencev.v.sloveniji" ) %>%
  mutate_at("leto", str_replace, " Na potovanjih v Sloveniji Vrsta obiskovalcev - Skupaj", "") %>%
  select(-c(1))

# ==============================================================================

izdatki.slovencev.v.tujini <- read_csv("izdatki_slovencev_za_turisticno_potrosnjo_v_tujini.csv", 
                                          locale = locale(encoding = "Windows-1250"),
                                          col_names=TRUE, 
                                          col_types = cols(.default = col_guess()))

izdatki.slovencev.v.tujini <- izdatki.slovencev.v.tujini %>%
  pivot_longer(cols = colnames(izdatki.slovencev.v.tujini)[-1],
               names_to = "leto",
               values_to = "izdatki.slovencev.v.tujini" ) %>%
  mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
  select(-c(1))

# ==============================================================================

# 5
# IZDATKI ZA TURIZEM (zdruzitev treh tabel)
# stevilke so v milijonih €

IZDATKI <- izdatki.tujcev %>% 
  full_join(izdatki.slovencev.v.sloveniji, by="leto") %>%
  full_join(izdatki.slovencev.v.tujini, by="leto")

# ==============================================================================
# ==============================================================================


                           