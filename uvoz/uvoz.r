# 2. faza: Uvoz podatkov

source("C:/Users/Uporabnik/OneDrive/Namizje/SOLA/2.letnik/ANALIZA PODATKOV S PROGRAMOM R/APPR-2021-22/lib/libraries.r")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# ==============================================================================
# ==============================================================================

prenocitve.regije <- read_csv("prenočitve_regije_mesečno.csv",
                              locale = locale(encoding = "Windows-1250"),
                              col_names=TRUE, 
                              col_types = cols(.default = col_guess()))
prenocitve.regije <- prenocitve.regije[-c(1, 2),]
prenocitve.regije[prenocitve.regije == "-"] <- NA
prenocitve.regije$`2020M04` <- parse_number(prenocitve.regije$`2020M04`)

prenocitve.regije <- prenocitve.regije %>% 
  pivot_longer(
    cols = colnames(prenocitve.regije)[-c(1, 2, 3, 4)],
    names_to = "Leto",
    values_to = "Prenocitve"
  ) 
prenocitve.regije <- prenocitve.regije[, c(1, 3, 5, 6)]
names(prenocitve.regije) <- c("Regija", "Drzava", "Leto", "Stevilo")


# prenocitve.regije.tuji.domaci.zdruzeno <- prenocitve.regije %>%
#   group_by(Regija, Drzava, Leto) %>%
#   summarise("Prenocitve" = sum(Stevilo))

# ==============================================================================

nastanitvena.doba.regije <- read_csv("povprecna_doba_nastanitve_regije_mesecno.csv",
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

nastanitveni.obrat.regije <- read_csv("prenočitve_domačih_in_tujih_turistov_glede_na_nastanitveni_obrat_regije_mesečno.csv", 
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
  ) 
names(nastanitveni.obrat.regije) <- c("Regija", "Nastanitev", "Drzava", "Leto", "Stevilo")

nastanitveni.obrat.regije <- nastanitveni.obrat.regije[! nastanitveni.obrat.regije$Regija ==
                                                       "SLOVENIJA", ]

# ==============================================================================

povprecni.dohodek.regije <- read_csv("povprecna_mesecna_placa_po_regijah.csv",
                                     locale = locale(encoding = "Windows-1250"),
                                     col_names=TRUE, 
                                     col_types = cols(.default = col_guess()))

names(povprecni.dohodek.regije) <- c("Regija", "x", "Leto", "Placa")
povprecni.dohodek.regije <- povprecni.dohodek.regije %>% .[-2]

povprecni.dohodek.regije <- povprecni.dohodek.regije[! povprecni.dohodek.regije$Regija ==
                                                       "SLOVENIJA", ]

# ==============================================================================

# 1
# MESECNI PREGLED ZA REGIJE

# MESECNI.PREGLED.REGIJE <-

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
               values_to = "Stevilo") %>%
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

motivi <- read_html("motiv_obiska_slovenije_iz_tujine.html", skip = 2, remove.empty = TRUE, trim = TRUE)
motivi.vmesna <- html_nodes(motivi, "table")
motivi.prihoda <- html_table(motivi.vmesna, header = FALSE, fill = TRUE)[[1]]
motivi.prihoda <- motivi.prihoda[-c(1, 2, 3, 4, 368), - c(4, 7, 8, 9)]
  
motivi.prihoda[motivi.prihoda == "-"] <- "0"
motivi.prihoda$X2 <- parse_number(motivi.prihoda$X2)
motivi.prihoda$X3 <- parse_number(motivi.prihoda$X3)
motivi.prihoda$X5 <- parse_number(motivi.prihoda$X5)
motivi.prihoda$X6 <- parse_number(motivi.prihoda$X6)

# POM kot pomembno
# NEP kot nepomembno
motivi.prihoda$POM <- motivi.prihoda$X5 + motivi.prihoda$X6
motivi.prihoda$NEP <- motivi.prihoda$X2 + motivi.prihoda$X3

motivi.prihoda <- motivi.prihoda[, -c(2:5)] %>% mutate(Pomembnost = case_when(
  (is.na(POM)) ~ "Drzava",
  (NEP > POM) ~ "Nepomembno",
  (NEP < POM) ~ "Pomembno"
))

# manjša še razdelitev na dražve!

# ==============================================================================

# 3
# RAZLOGI ZA PRIHDO TUJCEV V SLOVENIJO IN PREVOZNO SREDSTVO


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

names(odhod.slovencev.v.tujino) <- c("Vrsta", "Država", "Meritev", "Leto", "Število")


# razdelim tebelo na nočitve ter izdatke:

odhod.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
                                                               'Povprečni izdatki na turista na prenočitev (EUR)',]
odhod.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[, -3]
odhod.slovencev.v.tujino.nocitve[odhod.slovencev.v.tujino.nocitve == "Zasebna potovanja"] <- "Zasebna"
odhod.slovencev.v.tujino.nocitve[odhod.slovencev.v.tujino.nocitve == "Poslovna potovanja"] <- "Poslovna"

odhod.slovencev.v.tujino.izdatki.na.turista <- odhod.slovencev.v.tujino[!odhod.slovencev.v.tujino$Meritev == 
                                                                          'Povprečno število prenočitev',]

# 
# #razdelim tabelo nocitev na zasebna in poslovna potovanja:
# zasebna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[1:117,] %>%
#   select(Država, Leto, Število) %>%
#   rename("Zasebna potovanja" = "Število")
# poslovna.potovanja.slovencev.v.tujino.nocitve <- odhod.slovencev.v.tujino.nocitve[118:234,] %>%
#   select(Država, Leto, Število) %>%
#   rename("Poslovna potovanja" = "Število")

# ==============================================================================

# 4
# ODHOD SLOVENCEV V TUJINO

odhod.slovencev.v.tujino.nocitve

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

sestava <- read_html("sestava_turisticne_potrosnje_tujcev_v_sloveniji.html", skip = 2, remove.empty = TRUE, trim = TRUE)
sestava.vmesna <- html_nodes(sestava, "table")
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI <- html_table(sestava.vmesna, header = FALSE, fill = TRUE)[[1]]
names(SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI) <- c("Storitve", "2012", "2012", "2014","2014", "2015", "2015", "2017", "2017")
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

# 5
# IZDATKI ZA TURIZEM (zdruzitev treh tabel)
# stevilke so v milijonih €

IZDATKI <- izdatki

SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI

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

names(odlocitev.glede.na.izobrazbo) <- c("Izobrazba", "Odhod", "Leto", "Stevilo")
odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
                                            "Niso šli na zasebno potovanje"] <-
  "Niso"
odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
                                            "Šli na zasebno potovanje"] <-
  "Zasebno"
odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo ==
                                            "Šli na poslovno potovanje"] <-
  "Poslovno"

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
               values_to = "Stevilo")
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
                                            "Niso šli na turistično potovanje (zasebno in/ali poslovno)"] <-
  "Niso"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
                                            "Šli na zasebno potovanje"] <-
  "Zasebno"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva ==
                                            "Šli na poslovno potovanje"] <-
  "Poslovno"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
                                            "1- člansko"] <- "1"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
                                            "2- člansko"] <- "2"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
                                            "3- ali 4- člansko"] <- "3/4"
odlocitev.glede.na.velikost.gospodinjstva[odlocitev.glede.na.velikost.gospodinjstva == 
                                            "5- ali veččlansko"] <- "5<"

names(odlocitev.glede.na.velikost.gospodinjstva) <- c("StClanov", "Odhod", "Leto", "Stevilo")

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
  rename("Status" = "ZAPOSLITVENI STATUS") 

odlocitev.glede.na.zaposlenost <- odlocitev.glede.na.zaposlenost[1 : 36, ]
odlocitev.glede.na.zaposlenost[odlocitev.glede.na.zaposlenost == 
                                 "Zaposlen, samozaposlen"] <- "Zaposlen"
odlocitev.glede.na.zaposlenost[odlocitev.glede.na.zaposlenost == 
                                 "Dijak ali študent"] <- "Študent"


# ==============================================================================

# 6
# KAJ VPLIVA NA ODLOČITVE POTOVANJA

odlocitev.glede.na.izobrazbo
odlocitev.glede.na.velikost.gospodinjstva
odlocitev.glede.na.zaposlenost

# ==============================================================================
# ==============================================================================

