library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rvest)
require(dplyr)
require(data.table)
library(xml2) # za html uvoz

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
    values_to = "Stevilo nocitev"
)
  
stevilo.prenocitev.letno <- prenocitve.po.drzavah.letno[1:11, 2:3] 
stevilo.prenocitev.letno$Leto <- parse_number(stevilo.prenocitev.letno$Leto)

# ==============================================================================

st.zap <- read_html("stevilo_zaposlenih_v_turizmu_vec_let.html", skip = 2, remove.empty = TRUE, trim = TRUE)
st.zap.vmesna <- html_nodes(st.zap, "table")
stevilo.zaposlenih <- html_table(st.zap.vmesna, header = FALSE, fill = TRUE)[[1]]
names(stevilo.zaposlenih) <- c("Storitve", "2012", "2014", "2015", "2017")
stevilo.zaposlenih <- stevilo.zaposlenih[-c(1, 2, 3, 10, 11),]

stevilo.zaposlenih <- stevilo.zaposlenih %>%
  pivot_longer(cols = colnames(stevilo.zaposlenih)[-1],
               names_to = "Leto",
               values_to = "Stevilo prenocitev") %>%
  mutate(
    Storitve = str_replace_all(Storitve, "(..)(.*)", "\\2")
  ) %>%
  arrange(Leto)

stevilo.zaposlenih$Leto <- parse_number(stevilo.zaposlenih$Leto)
stevilo.zaposlenih$Stevilo <- parse_number(stevilo.zaposlenih$Stevilo)

stevilo.zaposlenih.letno <- stevilo.zaposlenih %>% group_by(Leto) %>% summarise("Stevilo zaposlenih" = sum(Stevilo))

# ==============================================================================

# 2 
# VEČLETNI PREGLED ZA SLOVENIJO

VECLETNI.PREGLED.ZA.SLOVENIJO <- stevilo.prenocitev.letno %>%
  right_join(stevilo.zaposlenih.letno, by="Leto")

# ==============================================================================
# ==============================================================================

odhod.slovencev.v.tujino <- read.csv("odhod_slovencev_v_tujino_po_drzavah_letno.csv",
                                     na.strings = c("N", "-"))

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

odlocitev.glede.na.izobrazbo <- read_csv("odlocitev_za_potovanaje_glede_na_izobrazbo.csv", 
                                         locale = locale(encoding = "Windows-1250"),
                                         col_names=TRUE,
                                         col_types = cols(.default = col_guess()))

odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo == "N"] <- NA  

odlocitev.glede.na.izobrazbo$`2018` <- parse_number(odlocitev.glede.na.izobrazbo$`2018`)
odlocitev.glede.na.izobrazbo$`2020` <- parse_number(odlocitev.glede.na.izobrazbo$`2020`)

odlocitev.glede.na.izobrazbo <- odlocitev.glede.na.izobrazbo %>%
  pivot_longer(cols = colnames(odlocitev.glede.na.izobrazbo)[-c(1,2)],
               names_to = "Leto",
               values_to = "Stevilo" )

# ==============================================================================

odlocitev.glede.na.velikost.gospodinjstva <- read_csv("odlocitev_za_potovanaje_glede_na_velikost_gospodinjstva.csv",
                                                      locale = locale(encoding = "Windows-1250"),
                                                      col_names=TRUE, 
                                                      col_types = cols(.default = col_guess(),
                                                                       "2012" = col_double(),
                                                                        "2013" = col_double(),
                                                                        "2014" = col_double(),
                                                                        "2015" = col_double(),
                                                                        "2016" = col_double(),
                                                                        "2017" = col_double(),
                                                                        "2018" = col_double(),
                                                                        "2019" = col_double(),
                                                                        "2020" = col_double()))
                                                      
odlocitev.glede.na.velikost.gospodinjstva <- odlocitev.glede.na.velikost.gospodinjstva %>%
  pivot_longer(cols = colnames(odlocitev.glede.na.velikost.gospodinjstva)[-c(1,2)],
               names_to = "Leto",
               values_to = "Število gospodinjstev")

# # razdelitev v tri ločene tabele:
# 
# odlocitev.glede.na.velikost.gospodinjstva.niso.sli <- 
#   odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva$MERITVE ==
#                                               "Niso šli na turistično potovanje (zasebno in/ali poslovno)",] %>%
#   select("VELIKOST GOSPODINJSTVA", "Leto", "Število gospodinjstev") %>%
#   rename("Niso šli na potovanje" = "Število gospodinjstev", "Velikost gospodinjstva" = "VELIKOST GOSPODINJSTVA")
#        
# odlocitev.glede.na.velikost.gospodinjstva.na.zasebno <- 
#   odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva$MERITVE ==
#                                               "Šli na zasebno potovanje",] %>%
#   select("VELIKOST GOSPODINJSTVA", "Leto", "Število gospodinjstev") %>%
#   rename("Šli na zasebno potovanje" = "Število gospodinjstev", "Velikost gospodinjstva" = "VELIKOST GOSPODINJSTVA")                 
# 
# odlocitev.glede.na.velikost.gospodinjstva.na.poslovno <-
#   odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva$MERITVE ==
#                                               "Šli na poslovno potovanje",] %>%
#   select("VELIKOST GOSPODINJSTVA", "Leto", "Število gospodinjstev") %>%
#   rename("Šli na poslovno potovanje" = "Število gospodinjstev", "Velikost gospodinjstva" = "VELIKOST GOSPODINJSTVA") 
# 
# ODLOCITEV.GLEDE.NA.VELIKOST.GOSPODINJSTVA <-
#   odlocitev.glede.na.velikost.gospodinjstva.niso.sli %>% full_join(odlocitev.glede.na.velikost.gospodinjstva.na.zasebno, by=c("Leto", "Velikost gospodinjstva")) %>%
#   full_join(odlocitev.glede.na.velikost.gospodinjstva.na.poslovno, by=c("Leto", "Velikost gospodinjstva"))

# ==============================================================================

odlocitev.glede.na.zaposlenost <- read_csv("odlocitev_za_potovanaje_glede_na_zaposlenost.csv",
                                           locale = locale(encoding = "Windows-1250"),
                                           col_names=TRUE, 
                                           col_types = cols(.default = col_guess(),
                                                            "2012" = col_double(),
                                                            "2013" = col_double(),
                                                            "2014" = col_double(),
                                                            "2015" = col_double(),
                                                            "2016" = col_double(),
                                                            "2017" = col_double(),
                                                            "2018" = col_double(),
                                                            "2019" = col_double(),
                                                            "2020" = col_double()))

odlocitev.glede.na.zaposlenost <- odlocitev.glede.na.zaposlenost %>%
  pivot_longer(cols = colnames(odlocitev.glede.na.zaposlenost)[-c(1,2,3)],
               names_to = "Leto",
               values_to = "Število") %>%
  select("ZAPOSLITVENI STATUS", "Leto", "Število") %>%
  rename("Zaposlitveni status" = "ZAPOSLITVENI STATUS") 

odlocitev.glede.na.zaposlenost <- odlocitev.glede.na.zaposlenost[1 : 36, ]

# ==============================================================================
# ==============================================================================
