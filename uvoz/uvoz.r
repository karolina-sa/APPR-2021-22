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
names(nastanitvena.doba.regije) <- c("Mesec", "Regija", "Slovenski turisti", "Tuji turisti")

nastanitvena.doba.regije <- nastanitvena.doba.regije[! nastanitvena.doba.regije$Regija ==
                                                       "SLOVENIJA", ] %>%
  pivot_longer(
    cols = colnames(nastanitvena.doba.regije)[-c(1, 2)],
    names_to = "Drzava",
    values_to = "StDni"
  ) %>%
  na.omit() %>%
  mutate(Mesec = str_replace_all(Mesec, "(\\d{4})([:alpha:])(\\d{2})", "\\3"))

# drugače stat_smooth ne dela:
nastanitvena.doba.regije$StDni <- as.integer(nastanitvena.doba.regije$StDni)
nastanitvena.doba.regije$Mesec <- as.integer(nastanitvena.doba.regije$Mesec)

# za analizo si bom izračunala povprečno nastanitveno dobo regije:
povprecna.nastanitvena.doba.regija <- nastanitvena.doba.regije %>%
  group_by(Regija) %>% summarize(Povprecje = mean(StDni, na.rm = TRUE)) %>%
  arrange(desc(Povprecje))

# ==============================================================================

nastanitveni.obrat.regije <- read_csv("podatki/prenočitve_domačih_in_tujih_turistov_glede_na_nastanitveni_obrat_regije_mesečno.csv",
                                      locale = locale(encoding = "Windows-1250"),
                                      col_names=TRUE,
                                      col_types = cols(.default = col_guess()))
nastanitveni.obrat.regije[nastanitveni.obrat.regije == "N" | nastanitveni.obrat.regije == "z" |
                          nastanitveni.obrat.regije == "-"] <- NA
nastanitveni.obrat.regije <- nastanitveni.obrat.regije[, -4]

nastanitveni.obrat.regije <- nastanitveni.obrat.regije %>%
  pivot_longer(
    cols = colnames(nastanitveni.obrat.regije)[-c(1, 2, 3)],
    names_to = "Leto",
    values_to = "Prenocitve"
  ) %>%
  na.omit() %>%
  mutate(Leto = str_replace_all(Leto, "(\\d{4})([:alpha:])(\\d{2})", "\\3"))

names(nastanitveni.obrat.regije) <- c("Regija", "Tip", "Drzava", "Mesec", "Stevilo nastanitev")

nastanitveni.obrat.regije[nastanitveni.obrat.regije == "1 Hoteli in podobni nastanitveni objekti"] <- "Hotel"
nastanitveni.obrat.regije[nastanitveni.obrat.regije == "2 Kampi"] <- "Kamp"
nastanitveni.obrat.regije[nastanitveni.obrat.regije == "3 Drugi nastanitveni obrati"] <- "Drugo"

nastanitveni.obrat.regije$Mesec <- as.integer(nastanitveni.obrat.regije$Mesec)

# ==============================================================================

# MESECNI PREGLED ZA REGIJE

prenocitve.regije

nastanitvena.doba.regije

nastanitveni.obrat.regije

# ==============================================================================
# ==============================================================================

prenocitve.po.drzavah.letno <- read_csv("podatki/prenočitve_po_drzavah_letno.csv",
                               locale = locale(encoding = "Windows-1250"),
                               col_names=TRUE,
                               col_types = cols(.default = col_guess()))

prenocitve.po.drzavah.letno <- prenocitve.po.drzavah.letno %>%
  pivot_longer(
    cols = colnames(prenocitve.po.drzavah.letno)[-1],
    names_to = "Leto",
    values_to = "Stevilo"
)

prenocitve.letno <- prenocitve.po.drzavah.letno %>%
  filter()

# ==============================================================================

st.zap <- read_html("podatki/stevilo_zaposlenih_v_turizmu_vec_let.html", skip = 2, remove.empty = TRUE, trim = TRUE)
st.zap.vmesna <- html_nodes(st.zap, "table")
stevilo.zaposlenih <- html_table(st.zap.vmesna, header = FALSE, fill = TRUE)[[1]]
names(stevilo.zaposlenih) <- c("Storitve", "2012", "2014", "2015", "2017", "2019", "2020")
stevilo.zaposlenih <- stevilo.zaposlenih[-c(1, 2, 3, 10, 11),]

stevilo.zaposlenih <- stevilo.zaposlenih %>%
  pivot_longer(cols = colnames(stevilo.zaposlenih)[-1],
               names_to = "Leto",
               values_to = "Stevilo") %>%
  mutate(
    Storitve = str_replace_all(Storitve, "(..)(.*)", "\\2")
  ) %>%
  arrange(Leto)

stevilo.zaposlenih$Stevilo <- parse_number(stevilo.zaposlenih$Stevilo)

# ==============================================================================

BDP.turizem <- read_csv("podatki/BDP_turizma.csv",
                        locale = locale(encoding = "Windows-1250"),
                        col_names=TRUE,
                        col_types = cols(.default = col_guess()))
BDP.turizem <- BDP.turizem %>%
  pivot_longer(
    cols = colnames(BDP.turizem)[-1],
    names_to = "Leto",
    values_to = "Stevilo"
  )

BDP.turizem <- BDP.turizem[, -1]

# ==============================================================================

# VEČLETNI PREGLED ZA SLOVENIJO

prenocitve.po.drzavah.letno

stevilo.zaposlenih

BDP.turizem

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

# POM kot pomembno; NEP kot nepomembno; NITI kot niti pomembno, niti nepomembno
motivi.prihoda$POM <- motivi.prihoda$X5 + motivi.prihoda$X6
motivi.prihoda$NEP <- motivi.prihoda$X2 + motivi.prihoda$X3
motivi.prihoda$NITI <- motivi.prihoda$X4
motivi.prihoda$Sestevek <- motivi.prihoda$POM - motivi.prihoda$NEP

motivi.prihoda <- motivi.prihoda %>%
  dplyr::select(X1, POM, NEP, NITI, Sestevek)

motivi.prihoda$Drzava <- motivi.prihoda$X1
motivi.prihoda <- motivi.prihoda[-c(1,2,3),c(6,1,2,3,4,5)]

ime = "Država prebivališča - SKUPAJ"
for (i in 1:362) {
  if (is.na(motivi.prihoda$Sestevek[i])) {
    ime = motivi.prihoda[i,1]
  }
  else {
    motivi.prihoda[i,1] <- ime
  }
}

motivi.prihoda[motivi.prihoda == "Država prebivališča - SKUPAJ"] <- "Skupaj"
motivi.prihoda <- motivi.prihoda[ ! is.na(motivi.prihoda$Sestevek),] %>%
  group_by(Drzava, X1) %>%
  summarise(Pomembno = sum(POM), Nepomembno = sum(NEP), NitiNiti = sum(NITI)) %>%
  mutate(Pomembnost = Pomembno / (Pomembno + Nepomembno + NitiNiti))
motivi.prihoda <- motivi.prihoda %>%
  pivot_longer(
    cols = colnames(motivi.prihoda)[-c(1,2)],
    names_to = "Presoja",
    values_to = "StGlasov"
  ) %>%
  rename("Motiv" = X1)

motivi.prihoda[motivi.prihoda == "Kulturne znamenitosti in prireditve"] <- "Prireditve"
motivi.prihoda[motivi.prihoda == "Možnosti za zabavo"] <- "Zabava"
motivi.prihoda[motivi.prihoda == "Osebna varnost med bivanjem"] <- "Občutek varnosti"
motivi.prihoda[motivi.prihoda == "Možnosti za počitek in sprostitev"] <- "Sprostitev"
motivi.prihoda[motivi.prihoda == "Primernost za družinske počitnice"] <- "Družinsko okolje"
motivi.prihoda[motivi.prihoda == "Raznolika gastronomska ponudba"] <- "Gastronomija"
motivi.prihoda[motivi.prihoda == "Redne letalske povezave z državo"] <- "Letalske povezave"
motivi.prihoda[motivi.prihoda == "Rekreativne dejavnosti"] <- "Rekreacija"
motivi.prihoda[motivi.prihoda == "Skrb za zdravje in storitve dobrega počutja (velnes)"] <- "Velnes"

# ==============================================================================

# RAZLOGI ZA PRIHDO TUJCEV V SLOVENIJO IN PREVOZNO SREDSTVO

motivi.prihoda

# ==============================================================================
# ==============================================================================
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
# # ODHOD SLOVENCEV V TUJINO
# 
# odhod.slovencev.v.tujino.nocitve

# ==============================================================================
# ==============================================================================

izdatki.tujcev <- read_csv("podatki/izdatki_tujcev_za_turisticno_potrosnjo_v_Sloveniji.csv",
                           locale = locale(encoding = "Windows-1250"),
                           col_names=TRUE,
                           col_types = cols(.default = col_guess()))

izdatki.tujcev <- izdatki.tujcev %>%
  pivot_longer(cols = colnames(izdatki.tujcev)[-1],
               names_to = "leto",
               values_to = "Izdatki tujcev v Sloveniji" ) %>%
  mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
  select(-c(1))

# ==============================================================================

izdatki.slovencev.v.sloveniji <- read_csv("podatki/izdatki_slovencev_za_turisticno_potrosnjo_v_Sloveniji.csv",
                           locale = locale(encoding = "Windows-1250"),
                           col_names=TRUE,
                           col_types = cols(.default = col_guess()))

izdatki.slovencev.v.sloveniji <- izdatki.slovencev.v.sloveniji %>%
  pivot_longer(cols = colnames(izdatki.slovencev.v.sloveniji)[-1],
               names_to = "leto",
               values_to = "Izdatki Slovencev v Sloveniji" ) %>%
  mutate_at("leto", str_replace, " Na potovanjih v Sloveniji Vrsta obiskovalcev - Skupaj", "") %>%
  select(-c(1))

# ==============================================================================

izdatki.slovencev.v.tujini <- read_csv("podatki/izdatki_slovencev_za_turisticno_potrosnjo_v_tujini.csv",
                                          locale = locale(encoding = "Windows-1250"),
                                          col_names=TRUE,
                                          col_types = cols(.default = col_guess()),
                                          skip = 2)

izdatki.slovencev.v.tujini <- izdatki.slovencev.v.tujini %>%
  pivot_longer(cols = colnames(izdatki.slovencev.v.tujini)[-1],
               names_to = "leto",
               values_to = "Izdatki Slovencev v tujini" ) %>%
  mutate_at("leto", str_replace, " Vrsta obiskovalcev - SKUPAJ", "") %>%
  select(-c(1))

# ==============================================================================

sestava <- read_html("podatki/sestava_turisticne_potrosnje_tujcev_v_sloveniji.html", skip = 2, remove.empty = TRUE, trim = TRUE)
sestava.vmesna <- html_nodes(sestava, "table")
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- html_table(sestava.vmesna, header = FALSE, fill = TRUE)[[1]]
names(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI) <- c("Storitve", "2012", "2012", "2014","2014", "2015", "2015", "2017", "2017", "2019", "2019", "2020", "2020")
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI[-c(1, 2, 3, 10, 11),]

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI %>%
  pivot_longer(cols = colnames(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI)[-1],
               names_to = "Leto",
               values_to = "Stevilo") %>%
  mutate(
    Storitve = str_replace_all(Storitve, "(..)(.*)", "\\2")
  ) %>%
  arrange(Leto)

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Leto <- parse_number(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Leto)
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Stevilo <- parse_number(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Stevilo)

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI %>%
  group_by(Storitve, Leto) %>%
  summarise("Izdatek" = sum(Stevilo))

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <- gsub("Storitve prevoza potnikov in najema prevoznih sredstev",
                                                                 "Prevozna sredstva",
     SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <-
  gsub("Storitve turističnih agencij in organizatorjev potovanj",
     "Storitve turističnih agencij",
     SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <-
  gsub("Športne in rekreacijske storitve", "Športne storitve",
     SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve <-
  gsub("Strežba hrane in pijač", "Gostinjske storitve",
     SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI$Storitve)


# ==============================================================================

izdatki <- izdatki.tujcev %>%
  full_join(izdatki.slovencev.v.sloveniji, by="leto") %>%
  full_join(izdatki.slovencev.v.tujini, by="leto")

izdatki <- izdatki %>%
  pivot_longer(
    cols = colnames(izdatki)[-1],
    names_to = "Leto",
    values_to = "Stevilo"
  )
names(izdatki) <- c("Leto", "Vrsta", "Stevilo")

# ==============================================================================

# IZDATKI ZA TURIZEM (zdruzitev treh tabel)
# stevilke so v milijonih €

IZDATKI <- izdatki

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI

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

leto2022 <- read_excel("podatki/2022.xlsx") %>%
  dplyr::select(c(2,10,18)) %>%
  na.omit()
colnames(leto2022) <- (c("Prihodi", "Prenočitve", "Država"))
leto2022 <- leto2022 %>% 
  mutate(Država = str_replace_all(Država, "(from)?([:blank:])(.+)", "\\3"))
leto2022$Leto <- rep(2022, times=56)
  
leto2021 <- read_excel("podatki/2021.xlsx") %>%
  dplyr::select(c(2,10,18)) %>%
  na.omit()
colnames(leto2021) <- (c("Prihodi", "Prenočitve", "Država"))
leto2021 <- leto2021 %>% 
  mutate(Država = str_replace_all(Država, "(from)?([:blank:])(.+)", "\\3"))
leto2021$Leto <- rep(2021, times=52)

leto2020 <- read_excel("podatki/2020.xlsx") %>%
  dplyr::select(c(2,8,14)) %>%
  na.omit()
colnames(leto2020) <- (c("Prihodi", "Prenočitve", "Država"))
leto2020 <- leto2020 %>% 
  mutate(Država = str_replace_all(Država, "(from)?([:blank:])(.+)", "\\3"))
leto2020$Leto <- rep(2020, times=55)

leto2019 <- read_excel("podatki/2019.xlsx") %>%
  dplyr::select(c(2,8,14)) %>%
  na.omit()
colnames(leto2019) <- (c("Prihodi", "Prenočitve", "Država"))
leto2019 <- leto2019 %>% 
  mutate(Država = str_replace_all(Država, "(from)?([:blank:])(.+)", "\\3"))
leto2019$Leto <- rep(2019, times=56)

leto2018 <- read_excel("podatki/2018.xlsx") %>%
  dplyr::select(c(2,5,8)) %>%
  na.omit()
colnames(leto2018) <- (c("Prihodi", "Prenočitve", "Država"))
leto2018 <- leto2018 %>% 
  mutate(Država = str_replace_all(Država, "(from)?([:blank:])(.+)", "\\3"))
leto2018$Leto <- rep(2018, times=56)

vec.let <- leto2022 %>%
  full_join(leto2021, by=c("Država", "Prenočitve", "Prihodi")) %>%
  full_join(leto2020, by=c("Država", "Prenočitve", "Prihodi")) %>%
  full_join(leto2019, by=c("Država", "Prenočitve", "Prihodi")) %>%
  full_join(leto2018, by=c("Država", "Prenočitve", "Prihodi")) 
vec.let$Leto <- rowSums(vec.let[,c(4,5,6,7,8)], na.rm = TRUE)
vec.let <- vec.let[,c(1,2,3,8)] %>%
  pivot_longer(
    cols = colnames(vec.let)[c(1,2)],
    names_to = "Kaj",
    values_to = "Število"
  )

# popravljanje držav (da se izriše zemljevid)
vec.let[vec.let == "Domestic"] <- "Slovenia"
vec.let[vec.let == "Czech Republic"] <- "Czechia"
vec.let[vec.let == "Northern Macedonia"] <- "North Macedonia"
vec.let[vec.let == "Korea (Republic of)"] <- "South Korea"
vec.let[vec.let == "Russian Federation"] <- "Russia"
vec.let[vec.let == "United States"] <- "United States of America"

vec.let.samo.prave.drzave <- vec.let[!vec.let$Država %in% c("Total", "Foreign", "other African countries", 
          "other Asian countries", "other countries of North America", 
          "other countries of Oceania", "	
          other countries of South and Middle America", "other European countries"),]
          
vec.let.samo.prave.drzave$Število <- as.integer(vec.let.samo.prave.drzave$Število)

# ==============================================================================

vec.let.samo.prave.drzave

# ==============================================================================
# ==============================================================================

slovenci.prenocitve <- read_csv("podatki/prenocitve.slovencev.v.sloveniji.csv",
              locale = locale(encoding = "Windows-1250"),
              col_names=TRUE,
              col_types = cols(.default = col_guess()))

slovenci.prenocitve <- slovenci.prenocitve %>%
  pivot_longer(cols = colnames(slovenci.prenocitve)[-1],
               names_to = "Leto",
               values_to = "Število" ) %>%
  mutate(Leto = str_replace_all(Leto, "(Prenočitve turistov )(\\d{4})([:alpha:])(\\d{2})(.*)", "\\2-\\4-01")) %>%
  dplyr::select(Leto, Število)

# da linearna regresija dela:
slovenci.prenocitve$Število <- as.integer(slovenci.prenocitve$Število)
slovenci.prenocitve$Leto <- as.Date(slovenci.prenocitve$Leto)

# ==============================================================================

