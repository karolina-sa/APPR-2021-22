# 2. faza: Uvoz podatkov

source("lib/libraries.r")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# ==============================================================================
# ==============================================================================

# Evropske države:
evropske_drzave = c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
             "Czech Republic","Denmark","Estonia","Finland","France",
             "Germany","Greece","Hungary","Ireland","Italy","Latvia",
             "Lithuania","Luxembourg","Malta","Netherlands","Poland",
             "Portugal","Romania","Slovakia","Slovenia","Spain",
             "Sweden","United Kingdom", "Russia", "Ukraine", "Belarus", 
             "Switzerland", "Norway", "Moldova", "Bosnia and Herzegovina",
             "Albania", "North Macedonia", "Montenegro", "Malta", "Iceland",
             "Andorra", "Monaco", "Liechtenstein", "San Marino",
             "Holy See")

options("scipen"=100, "digits"=4) # da kaže cele številke, ne pa e

turizem.svetovno <- read_csv("podatki/turizem_svetovno.csv",
                             skip = 4,
                             locale = locale(encoding = "Windows-1250"),
                             col_names=TRUE, 
                             col_types = cols(.default = col_guess()))
turizem.svetovno <- turizem.svetovno[-c(2:53)]
turizem.svetovno <- turizem.svetovno %>%
  pivot_longer(
    cols = colnames(turizem.svetovno)[-1],
    names_to = "Leto",
    values_to = "Stevilo turistov"
  ) %>%
  group_by(`Country Name`) %>%
  summarize(Povprecje = mean(`Stevilo turistov`, na.rm = TRUE)) %>%
  na.omit() %>% # izbriše stolpce z Nan 
  rename(Drzava = `Country Name`)

drzave.v.slovenscini <- c("Albania", "Andora", "Avstrija", "Belorusija", 
                          "Belgija", "Bosna in Herzegovina", 
                          "Bulgaria", "Hrvaska", "Ciper",
                          "Ceska", "Danska", "Estonija",
                          "Finska", "Francija", "Nemcija",
                          "Grcija", "Madzarska", "Islandija",
                          "Irska", "Italija", "Latvija",
                          "Lihtenstajn", "Litva", "Luksemburg",
                          "Malta", "Moldavija", "Monako",
                          "Crna gora", "Nizozemska", "Severna Makedonija",
                          "Norveska", "Poljska", "Portugalska",
                          "Romunija", "San Marino", "Slovenija",
                          "Spanija", "Svedska", "Svica", 
                          "Ukrajina", "Zdruzeno kraljestvo")

turizem.evropa <-  turizem.svetovno %>%
  filter(Drzava %in% evropske_drzave)
turizem.evropa$Drzava <- drzave.v.slovenscini

drzave.povrsine.km2 <- c("28.748", "468", "83.879", "207.600", 
                          "30.688", "51.197", 
                          "110.994", "56.594", "9.251",
                          "78.871", "42.933", "45.338",
                          "338.440", "543.940", "357.588",
                          "131.957", "93.030", "103.592",
                          "70.274", "302.073", "64.589",
                          "160", "65.300", "2.586",
                          "316", "33.846", "202",
                          "13.812", "41.543", "25.713",
                          "385.207", "312.679", "92.212",
                          "238.397", "61.2", "20.271",
                          "505.990", "450.295", "41.285", 
                          "603.548", "242.495")

turizem.evropa.povrsina <- turizem.evropa
turizem.evropa.povrsina$Povrsina <- drzave.povrsine.km2
turizem.evropa.povrsina$Povrsina <- parse_number(turizem.evropa.povrsina$Povrsina)
turizem.evropa.povrsina <- turizem.evropa.povrsina %>%
  transform(StTuristov_na_km2 = Povprecje / Povrsina) %>%
  dplyr::select(Drzava, StTuristov_na_km2)

# ==============================================================================

# Tabela za svetovno analizo:

TURIZEM.EVROPA <- turizem.evropa

TURIZEM.EVROPA.POVRSINA <- turizem.evropa.povrsina

# ==============================================================================
# ==============================================================================

prenocitve.regije <- read_excel("podatki/prenočitve_regije_mesečno.xlsx")
colnames(prenocitve.regije) <- prenocitve.regije[2,]
prenocitve.regije <- prenocitve.regije[3:41,-c(2,4)]

# dopolnitev prvega stolpca 
regija <- prenocitve.regije[1,1]
for (i in 1:39) {
  if (is.na(prenocitve.regije[i,1])) {
    prenocitve.regije[i,1] <- regija
  }
  else {
    regija <- prenocitve.regije[i,1]
  }
}

prenocitve.regije[prenocitve.regije == "-"] <- NA

colnames(prenocitve.regije)[1] <- "Regija"
colnames(prenocitve.regije)[2] <- "Drzava"

prenocitve.regije <- prenocitve.regije %>%
  pivot_longer(
    cols = colnames(prenocitve.regije)[-c(1, 2)],
    names_to = "Leto",
    values_to = "Prenocitve"
  ) %>%
  filter(Regija != "SLOVENIJA") %>%
  mutate(Leto = str_replace_all(Leto, "(\\d{4})([:alpha:])(\\d{2})", "\\1-\\3"))
prenocitve.regije$Drzava <- gsub("Država - SKUPAJ", "Skupaj", prenocitve.regije$Drzava)
prenocitve.regije$Prenocitve <- parse_number(prenocitve.regije$Prenocitve)

# ==============================================================================

nastanitvena.doba.regije <- read_csv("podatki/povprecna_doba_nastanitve_regije_mesecno.csv",
                                     locale = locale(encoding = "Windows-1250"),
                                     col_names=TRUE,
                                     col_types = cols(.default = col_guess()))

nastanitvena.doba.regije[nastanitvena.doba.regije == "N" | nastanitvena.doba.regije == "..."] <- NA
names(nastanitvena.doba.regije) <- c("Leto", "Regija", "Domaci", "Tuji")

nastanitvena.doba.regije <- nastanitvena.doba.regije[! nastanitvena.doba.regije$Regija ==
                                                       "SLOVENIJA", ]

nastanitvena.doba.regije$Domaci <- parse_number(nastanitvena.doba.regije$Domaci)
nastanitvena.doba.regije$Tuji <- parse_number(nastanitvena.doba.regije$Tuji)

nastanitvena.doba.regije$Skupaj <- nastanitvena.doba.regije$Domaci +
  nastanitvena.doba.regije$Tuji

# ==============================================================================

# nastanitveni.obrat.regije <- read_csv("podatki/prenočitve_domačih_in_tujih_turistov_glede_na_nastanitveni_obrat_regije_mesečno.csv",
#                                       locale = locale(encoding = "Windows-1250"),
#                                       col_names=TRUE,
#                                       col_types = cols(.default = col_guess()))
# nastanitveni.obrat.regije[nastanitveni.obrat.regije == "N" | nastanitveni.obrat.regije == "z" |
#                           nastanitveni.obrat.regije == "-"] <- NA
# nastanitveni.obrat.regije <- nastanitveni.obrat.regije[, -4]
# 
# nastanitveni.obrat.regije <- nastanitveni.obrat.regije %>%
#   pivot_longer(
#     cols = colnames(nastanitveni.obrat.regije)[-c(1, 2, 3)],
#     names_to = "Leto",
#     values_to = "Prenocitve"
#   )
# names(nastanitveni.obrat.regije) <- c("Regija", "Nastanitev", "Drzava", "Leto", "Stevilo")
# 
# nastanitveni.obrat.regije <- nastanitveni.obrat.regije[! nastanitveni.obrat.regije$Regija ==
#                                                        "SLOVENIJA", ]
# 
# # ==============================================================================
# 
# povprecni.dohodek.regije <- read_csv("podatki/povprecna_mesecna_placa_po_regijah.csv",
#                                      locale = locale(encoding = "Windows-1250"),
#                                      col_names=TRUE,
#                                      col_types = cols(.default = col_guess()))
# 
# names(povprecni.dohodek.regije) <- c("Regija", "x", "Leto", "Placa")
# povprecni.dohodek.regije <- povprecni.dohodek.regije %>% .[-2]
# 
# povprecni.dohodek.regije <- povprecni.dohodek.regije[! povprecni.dohodek.regije$Regija ==
#                                                        "SLOVENIJA", ]
# 
# ==============================================================================

# 1
# MESECNI PREGLED ZA REGIJE

prenocitve.regije

nastanitvena.doba.regije

# # ==============================================================================
# # ==============================================================================
# 
# prenocitve.po.drzavah.letno <- read_csv("podatki/prenočitve_po_drzavah_letno.csv", 
#                                locale = locale(encoding = "Windows-1250"),
#                                col_names=TRUE, 
#                                col_types = cols(.default = col_guess()))
# 
# prenocitve.po.drzavah.letno <- prenocitve.po.drzavah.letno %>% 
#   pivot_longer(
#     cols = colnames(prenocitve.po.drzavah.letno)[-1],
#     names_to = "Leto",
#     values_to = "Stevilo"
# )
#   
# stevilo.prenocitev.letno <- prenocitve.po.drzavah.letno[1:11, 2:3] 
# stevilo.prenocitev.letno$Stevilo <- parse_number(stevilo.prenocitev.letno$Stevilo)
# 
# # ==============================================================================
# 
# st.zap <- read_html("podatki/stevilo_zaposlenih_v_turizmu_vec_let.html", skip = 2, remove.empty = TRUE, trim = TRUE)
# st.zap.vmesna <- html_nodes(st.zap, "table")
# stevilo.zaposlenih <- html_table(st.zap.vmesna, header = FALSE, fill = TRUE)[[1]]
# names(stevilo.zaposlenih) <- c("Storitve", "2012", "2014", "2015", "2017")
# stevilo.zaposlenih <- stevilo.zaposlenih[-c(1, 2, 3, 10, 11),]
# 
# stevilo.zaposlenih <- stevilo.zaposlenih %>%
#   pivot_longer(cols = colnames(stevilo.zaposlenih)[-1],
#                names_to = "Leto",
#                values_to = "Stevilo") %>%
#   mutate(
#     Storitve = str_replace_all(Storitve, "(..)(.*)", "\\2")
#   ) %>%
#   arrange(Leto)
# 
# stevilo.zaposlenih$Stevilo <- parse_number(stevilo.zaposlenih$Stevilo)
# 
# stevilo.zaposlenih.letno <- stevilo.zaposlenih %>% group_by(Leto) %>% summarise("Stevilo zaposlenih" = sum(Stevilo))
# 
# # ==============================================================================
# 
# # 2 
# # VEČLETNI PREGLED ZA SLOVENIJO
# 
# stevilo.prenocitev.letno
# 
# VECLETNI.PREGLED.ZA.SLOVENIJO <- stevilo.prenocitev.letno %>%
#   right_join(stevilo.zaposlenih.letno, by="Leto")
# 
# ==============================================================================
# ==============================================================================

motivi <- read_html("podatki/motiv_obiska_slovenije_iz_tujine.html", skip = 2, remove.empty = TRUE, trim = TRUE)
motivi.vmesna <- html_nodes(motivi, "table")
motivi.prihoda <- html_table(motivi.vmesna, header = FALSE, fill = TRUE)[[1]]
motivi.prihoda <- motivi.prihoda[-c(1, 2, 368), - c(7, 8, 9)]

motivi.prihoda[motivi.prihoda == "-"] <- "0"
motivi.prihoda$X2 <- parse_number(motivi.prihoda$X2)
motivi.prihoda$X3 <- parse_number(motivi.prihoda$X3)
motivi.prihoda$X4 <- parse_number(motivi.prihoda$X4)
motivi.prihoda$X5 <- parse_number(motivi.prihoda$X5)
motivi.prihoda$X6 <- parse_number(motivi.prihoda$X6)

# POM kot pomembno
# NEP kot nepomembno
# NITI kot niti pomembno, niti nepomembno
motivi.prihoda$POM <- motivi.prihoda$X5 + motivi.prihoda$X6
motivi.prihoda$NEP <- motivi.prihoda$X2 + motivi.prihoda$X3
motivi.prihoda$NITI <- motivi.prihoda$X4
motivi.prihoda$Sestevek <- motivi.prihoda$POM - motivi.prihoda$NEP

motivi.prihoda <- motivi.prihoda %>%
  dplyr::select(X1, POM, NEP, NITI, Sestevek)

# zanima me, katere države so vključene v tabelo. Opazila sem, da je poleg vsake 
#   države v naslednjih stolpcih NA
# pogledam v katerih vrsticah se pojavi NA:
indeksi <- which(is.na(motivi.prihoda$Sestevek))
# seznam indeksov popravim tako, da bom lahko 'rezala' tabelo (dva indeksa ne 
#                 smeta biti zaporedni števili)
indeksi.popravljeni <- c()
for (i in 1:length(indeksi))
{
  zadnji.indeks <- indeksi[length(indeksi)]
  if (indeksi[i] != zadnji.indeks) {
    if (indeksi[i]+1 < indeksi[i+1]) {
      indeksi.popravljeni <- append(indeksi.popravljeni, indeksi[i])
    }
  }
}
indeksi.popravljeni <- append(indeksi.popravljeni, zadnji.indeks)
indeksi.popravljeni # to so indeksi po katerih bom rezala tabelo

# priprava novih tabel(imena tabel bodo MPštevilka)

for (i in 1:(length(indeksi.popravljeni)-1)) 
{
  ime <- paste("MP", i, sep = "")
  assign(ime, motivi.prihoda[(indeksi.popravljeni[i]):(indeksi.popravljeni[i+1]-1), ]) 
}
i = length(indeksi.popravljeni)
ime <- paste("MP", i, sep = "")
assign(ime, motivi.prihoda[(indeksi.popravljeni[i]):length(motivi.prihoda$Sestevek), ])

# length(indeksi.popravljeni)=24 je število vseh tabel
# Sedaj bom vse tabele popravila tako, da bo prvi stolpec 'ime' tabele, da bom 
#       lahko uporabila join

MP.vse.tabele <- vector(mode = "list", length = 24)
MP.vse.tabele[[1]] <- MP1
MP.vse.tabele[[2]] <- MP2
MP.vse.tabele[[3]] <- MP3
MP.vse.tabele[[4]] <- MP4
MP.vse.tabele[[5]] <- MP5
MP.vse.tabele[[6]] <- MP6
MP.vse.tabele[[7]] <- MP7
MP.vse.tabele[[8]] <- MP8
MP.vse.tabele[[9]] <- MP9
MP.vse.tabele[[10]] <- MP10
MP.vse.tabele[[11]] <- MP11
MP.vse.tabele[[12]] <- MP12
MP.vse.tabele[[13]] <- MP13
MP.vse.tabele[[14]] <- MP14
MP.vse.tabele[[15]] <- MP15
MP.vse.tabele[[16]] <- MP16
MP.vse.tabele[[17]] <- MP17
MP.vse.tabele[[18]] <- MP18
MP.vse.tabele[[19]] <- MP19
MP.vse.tabele[[20]] <- MP20
MP.vse.tabele[[21]] <- MP21
MP.vse.tabele[[22]] <- MP22
MP.vse.tabele[[24]] <- MP23
MP.vse.tabele[[24]] <- MP24

# for (i in 1:24)
# {
#   vek <- rep(MP.vse.tabele[[i]]$X1[1],15)
#   MP.vse.tabele[[i]]$Drzava <- vek
#   MP.vse.tabele[[i]] <- MP.vse.tabele[[i]][2:15,]
# }


# ==============================================================================

# 3
# RAZLOGI ZA PRIHDO TUJCEV V SLOVENIJO IN PREVOZNO SREDSTVO
#MOTIVI PRIHODA <- motivi prihoda


# # ==============================================================================
# # ==============================================================================
# 
# odhod.slovencev.v.tujino <- read.csv("podatki/odhod_slovencev_v_tujino_po_drzavah_letno.csv",
#                                      na.strings = c("N", "-"))
# 
# odhod.slovencev.v.tujino <- odhod.slovencev.v.tujino %>%
#   pivot_longer(cols = colnames(odhod.slovencev.v.tujino)[-c(1,2,3)],
#                names_to = "Leto",
#                values_to = "Število") %>%
#   mutate(
#     Leto = str_replace_all(Leto, "(X)(\\d{4})", "\\2")
#   )
# 
# names(odhod.slovencev.v.tujino) <- c("Vrsta", "Država", "Meritev", "Leto", "Število")
# 
# 
# # razdelim tebelo na nočitve ter izdatke:
# 
# odhod.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
#                                                                'Povprečni izdatki na turista na prenočitev (EUR)',]
# odhod.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[, -3]
# odhod.slovencev.v.tujino.nocitve[odhod.slovencev.v.tujino.nocitve == "Zasebna potovanja"] <- "Zasebna"
# odhod.slovencev.v.tujino.nocitve[odhod.slovencev.v.tujino.nocitve == "Poslovna potovanja"] <- "Poslovna"
# 
# odhod.slovencev.v.tujino.izdatki.na.turista <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
#                                                                           'Povprečno število prenočitev',]
# 
# # 
# # #razdelim tabelo nocitev na zasebna in poslovna potovanja:
# # zasebna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[1:117,] %>%
# #   select(Država, Leto, Število) %>%
# #   rename("Zasebna potovanja" = "Število")
# # poslovna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[118:234,] %>%
# #   select(Država, Leto, Število) %>%
# #   rename("Poslovna potovanja" = "Število")
# 
# # ==============================================================================
# 
# # 4
# # ODHOD SLOVENCEV V TUJINO
# 
# odhod.slovencev.v.tujino.nocitve
# 
# # ==============================================================================
# # ==============================================================================
# 
# izdatki.tujcev <- read_csv("podatki/izdatki_tujcev_za_turisticno_potrosnjo_v_Sloveniji.csv", 
#                            locale = locale(encoding = "Windows-1250"),
#                            col_names=TRUE, 
#                            col_types = cols(.default = col_guess()))
# 
# izdatki.tujcev <- izdatki.tujcev %>% 
#   pivot_longer(cols = colnames(izdatki.tujcev)[-1],
#                names_to = "leto", 
#                values_to = "izdatki.tujcev.v.sloveniji" ) %>%
#   mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
#   select(-c(1))
# 
# # ==============================================================================
# 
# izdatki.slovencev.v.sloveniji <- read_csv("podatki/izdatki_slovencev_za_turisticno_potrosnjo_v_Sloveniji.csv", 
#                            locale = locale(encoding = "Windows-1250"),
#                            col_names=TRUE, 
#                            col_types = cols(.default = col_guess()))
# 
# izdatki.slovencev.v.sloveniji <- izdatki.slovencev.v.sloveniji %>%
#   pivot_longer(cols = colnames(izdatki.slovencev.v.sloveniji)[-1],
#                names_to = "leto",
#                values_to = "izdatki.slovencev.v.sloveniji" ) %>%
#   mutate_at("leto", str_replace, " Na potovanjih v Sloveniji Vrsta obiskovalcev - Skupaj", "") %>%
#   select(-c(1))
# 
# # ==============================================================================
# 
# izdatki.slovencev.v.tujini <- read_csv("podatki/izdatki_slovencev_za_turisticno_potrosnjo_v_tujini.csv", 
#                                           locale = locale(encoding = "Windows-1250"),
#                                           col_names=TRUE, 
#                                           col_types = cols(.default = col_guess()))
# 
# izdatki.slovencev.v.tujini <- izdatki.slovencev.v.tujini %>%
#   pivot_longer(cols = colnames(izdatki.slovencev.v.tujini)[-1],
#                names_to = "leto",
#                values_to = "izdatki.slovencev.v.tujini" ) %>%
#   mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
#   select(-c(1))
# 
# # ==============================================================================
# 
# sestava <- read_html("podatki/sestava_turisticne_potrosnje_tujcev_v_sloveniji.html", skip = 2, remove.empty = TRUE, trim = TRUE)
# sestava.vmesna <- html_nodes(sestava, "table")
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- html_table(sestava.vmesna, header = FALSE, fill = TRUE)[[1]]
# names(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI) <- c("Storitve", "2012", "2012", "2014","2014", "2015", "2015", "2017", "2017")
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI[-c(1, 2, 3, 10, 11),]
# 
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI %>%
#   pivot_longer(cols = colnames(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI)[-1],
#                names_to = "Leto",
#                values_to = "Stevilo") %>%
#   mutate(
#     Storitve = str_replace_all(Storitve, "(..)(.*)", "\\2")
#   ) %>%
#   arrange(Leto)
# 
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Leto <- parse_number(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Leto)
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Stevilo <- parse_number(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Stevilo)
# 
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI %>% 
#   group_by(Storitve, Leto) %>%
#   summarise("Izdatek" = sum(Stevilo))
# 
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <- gsub("Storitve prevoza potnikov in najema prevoznih sredstev",
#                                                                  "Prevozna sredstva", 
#      SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <- 
#   gsub("Storitve turističnih agencij in organizatorjev potovanj",
#      "Storitve turističnih agencij", 
#      SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <- 
#   gsub("Športne in rekreacijske storitve", "Športne storitve", 
#      SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <- 
#   gsub("Strežba hrane in pijač", "Gostinjske storitve", 
#      SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
# 
# 
# # ==============================================================================
# 
# izdatki <- izdatki.tujcev %>% 
#   full_join(izdatki.slovencev.v.sloveniji, by="leto") %>%
#   full_join(izdatki.slovencev.v.tujini, by="leto")
# 
# izdatki <- izdatki %>%
#   pivot_longer(
#     cols = colnames(izdatki)[-1],
#     names_to = "Leto",
#     values_to = "Stevilo"
#   )
# names(izdatki) <- c("Leto", "Vrsta", "Stevilo")
# 
# # ==============================================================================
# 
# # 5
# # IZDATKI ZA TURIZEM (zdruzitev treh tabel)
# # stevilke so v milijonih €
# 
# IZDATKI <- izdatki
# 
# SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI
# 
# # ==============================================================================
# # ==============================================================================
# 
# odlocitev.glede.na.izobrazbo <- read_csv("podatki/odlocitev_za_potovanaje_glede_na_izobrazbo.csv", 
#                                          locale = locale(encoding = "Windows-1250"),
#                                          col_names=TRUE,
#                                          col_types = cols(.default = col_guess()))
# 
# odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo == "N"] <- NA  
# 
# odlocitev.glede.na.izobrazbo$`2018` <- parse_number(odlocitev.glede.na.izobrazbo$`2018`)
# odlocitev.glede.na.izobrazbo$`2020` <- parse_number(odlocitev.glede.na.izobrazbo$`2020`)
# 
# odlocitev.glede.na.izobrazbo <- odlocitev.glede.na.izobrazbo %>%
#   pivot_longer(cols = colnames(odlocitev.glede.na.izobrazbo)[-c(1,2)],
#                names_to = "Leto",
#                values_to = "Stevilo" ) 
# 
# names(odlocitev.glede.na.izobrazbo) <- c("Izobrazba", "Odhod", "Leto", "Stevilo")
# odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
#                                             "Niso šli na zasebno potovanje"] <-
#   "Niso šli"
# odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
#                                             "Šli na zasebno potovanje"] <-
#   "Zasebno"
# odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
#                                             "Šli na poslovno potovanje"] <-
#   "Poslovno"
# 
# odlocitev.glede.na.izobrazbo$Leto <- parse_number(odlocitev.glede.na.izobrazbo$Leto)
# 
# # ==============================================================================
# 
# odlocitev.glede.na.velikost.gospodinjstva <- read_csv("podatki/odlocitev_za_potovanaje_glede_na_velikost_gospodinjstva.csv",
#                                                       locale = locale(encoding = "Windows-1250"),
#                                                       col_names=TRUE, 
#                                                       col_types = cols(.default = col_guess(),
#                                                                        "2012" = col_double(),
#                                                                         "2013" = col_double(),
#                                                                         "2014" = col_double(),
#                                                                         "2015" = col_double(),
#                                                                         "2016" = col_double(),
#                                                                         "2017" = col_double(),
#                                                                         "2018" = col_double(),
#                                                                         "2019" = col_double(),
#                                                                         "2020" = col_double()))
#                                                       
# odlocitev.glede.na.velikost.gospodinjstva <- odlocitev.glede.na.velikost.gospodinjstva %>%
#   pivot_longer(cols = colnames(odlocitev.glede.na.velikost.gospodinjstva)[-c(1,2)],
#                names_to = "Leto",
#                values_to = "Stevilo")
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
#                                             "Niso šli na turistično potovanje (zasebno in/ali poslovno)"] <-
#   "Niso"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
#                                             "Šli na zasebno potovanje"] <-
#   "Zasebno"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
#                                             "Šli na poslovno potovanje"] <-
#   "Poslovno"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
#                                             "1- člansko"] <- "1"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
#                                             "2- člansko"] <- "2"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
#                                             "3- ali 4- člansko"] <- "3/4"
# odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
#                                             "5- ali veččlansko"] <- "5<"
# 
# names(odlocitev.glede.na.velikost.gospodinjstva) <- c("StClanov", "Odhod", "Leto", "Stevilo")
# 
# # ==============================================================================
# 
# odlocitev.glede.na.zaposlenost <- read_csv("podatki/odlocitev_za_potovanaje_glede_na_zaposlenost.csv",
#                                            locale = locale(encoding = "Windows-1250"),
#                                            col_names=TRUE, 
#                                            col_types = cols(.default = col_guess(),
#                                                             "2012" = col_double(),
#                                                             "2013" = col_double(),
#                                                             "2014" = col_double(),
#                                                             "2015" = col_double(),
#                                                             "2016" = col_double(),
#                                                             "2017" = col_double(),
#                                                             "2018" = col_double(),
#                                                             "2019" = col_double(),
#                                                             "2020" = col_double()))
# 
# odlocitev.glede.na.zaposlenost <- odlocitev.glede.na.zaposlenost %>%
#   pivot_longer(cols = colnames(odlocitev.glede.na.zaposlenost)[-c(1,2,3)],
#                names_to = "Leto",
#                values_to = "Število") %>%
#   select("ZAPOSLITVENI STATUS", "Leto", "Število") %>%
#   rename("Status" = "ZAPOSLITVENI STATUS") 
# 
# odlocitev.glede.na.zaposlenost <- odlocitev.glede.na.zaposlenost[1 : 36, ]
# odlocitev.glede.na.zaposlenost[odlocitev.glede.na.zaposlenost == 
#                                  "Zaposlen, samozaposlen"] <- "Zaposlen"
# odlocitev.glede.na.zaposlenost[odlocitev.glede.na.zaposlenost == 
#                                  "Dijak ali študent"] <- "Študent"
# 
# 
# # ==============================================================================
# 
# # 6
# # KAJ VPLIVA NA ODLOČITVE POTOVANJA
# 
# odlocitev.glede.na.izobrazbo
# odlocitev.glede.na.velikost.gospodinjstva
# odlocitev.glede.na.zaposlenost
# 
# # ==============================================================================
# # ==============================================================================
# 
# 
