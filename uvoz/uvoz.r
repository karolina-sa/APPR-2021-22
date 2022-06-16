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
                    "Portugal","Romania","Slovakia","Slovenia","Spain", "Serbia",
                    "Sweden","United Kingdom", "Russia", "Ukraine", "Belarus", 
                    "Switzerland", "Norway", "Moldova", "Bosnia and Herzegovina",
                    "Albania", "North Macedonia", "Montenegro", "Malta", "Iceland",
                    "Andorra", "Monaco", "Liechtenstein", "San Marino", "Kosovo",
                    "Holy See")

options("scipen"=100, "digits"=4) # da kaže cele številke, ne pa e

turizem.svetovno <- read_csv("podatki/turizem_svetovno.csv",
                             skip = 4,
                             locale = locale(encoding = "Windows-1250"),
                             col_names=TRUE, 
                             col_types = cols(.default = col_guess()))
turizem.svetovno <- turizem.svetovno[-c(2:53, 66)]
turizem.svetovno <- turizem.svetovno %>%
  pivot_longer(
    cols = colnames(turizem.svetovno)[-1],
    names_to = "Leto",
    values_to = "Stevilo turistov"
  ) %>%
  group_by(`Country Name`) %>%
  summarize(Povprecje = mean(`Stevilo turistov`, na.rm = TRUE)) %>%
  na.omit()
names(turizem.svetovno)[1] <- "Drzava"

gdp.svetovno <- read_csv("podatki/GDP.csv",
                         skip = 4,
                         locale = locale(encoding = "Windows-1250"),
                         col_names=TRUE, 
                         col_types = cols(.default = col_guess()))
gdp.svetovno <- gdp.svetovno[-c(2:53)]
gdp.svetovno <- gdp.svetovno %>%
  pivot_longer(
    cols = colnames(gdp.svetovno)[-1],
    names_to = "Leto",
    values_to = "GDP"
  ) %>%
  group_by(`Country Name`) %>%
  summarize(Povprecje = mean(`GDP`, na.rm = TRUE)) %>%
  na.omit() 
names(gdp.svetovno)[1] <- "Drzava"

# združitev tabel za napredno analizo:
turizem.svetovno <- turizem.svetovno %>%
  full_join(gdp.svetovno, by="Drzava")
names(turizem.svetovno)[2] <- "Turisti"
names(turizem.svetovno)[3] <- "BDP"

turizem.svetovno[turizem.svetovno == "Russian Federation"] <- "Russia"
turizem.svetovno[turizem.svetovno == "Northern Macedonia"] <- "North Macedonia"
turizem.svetovno[turizem.svetovno == "Slovak Republic"] <- "Slovakia"

drzave.povrsine <- read_csv("podatki/povrsina_drzav.csv",
                            locale = locale(encoding = "Windows-1250"),
                            col_names=TRUE, 
                            col_types = cols(.default = col_guess())) %>%
  dplyr::select(c(2,3)) %>%
  na.omit() # izbriše stolpce z Nan 
colnames(drzave.povrsine) <- c("Drzava", "Povrsina")
drzave.povrsine<- drzave.povrsine %>%
  mutate(Povrsina = str_replace_all(Povrsina, "(.*)([:blank:]{1})(.*)", "\\1"))

turizem.evropa <-  turizem.svetovno %>%
  filter(Drzava %in% evropske_drzave) %>%
  full_join(drzave.povrsine, by="Drzava") %>%
  na.omit()
turizem.evropa$Povrsina <- str_replace_all(turizem.evropa$Povrsina, ",", "")
turizem.evropa$Povrsina <- parse_number(turizem.evropa$Povrsina)
turizem.evropa <- turizem.evropa %>%
  mutate(StNaPovrsino = Turisti / Povrsina)
turizem.evropa <- turizem.evropa[,-c(2,4)]

turizem.evropa[turizem.evropa == "Czech Republic"] <- "Czechia"
turizem.evropa[turizem.evropa == "Serbia"] <- "Republic of Serbia"

turizem.evropa$BDP <- as.numeric(turizem.evropa$BDP)
turizem.evropa$StNaPovrsino <- as.numeric(turizem.evropa$StNaPovrsino)

# ==============================================================================

# Tabela za svetovno analizo:

TURIZEM.EVROPA <- turizem.evropa

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
colnames(nastanitvena.doba.regije) <- c("Mesec", "Regija", "Slovenski turisti", "Tuji turisti")

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

# MESECNI PREGLED ZA REGIJE

prenocitve.regije
nastanitvena.doba.regije

# ==============================================================================
# ==============================================================================

prenocitve.letno <- read_csv("podatki/prenočitve_po_drzavah_letno.csv",
                               locale = locale(encoding = "Windows-1250"),
                               col_names=TRUE,
                               col_types = cols(.default = col_guess()))

prenocitve.letno <- prenocitve.letno %>%
  pivot_longer(
    cols = colnames(prenocitve.letno)[-1],
    names_to = "Leto",
    values_to = "Stevilo"
)

prenocitve.letno[prenocitve.letno == "Združene države (ZDA)"] <- "ZDA"
prenocitve.letno[prenocitve.letno == "Ruska federacija"] <- "Rusija"
prenocitve.letno[prenocitve.letno == "Koreja (Republika)"] <- "Koreja"
prenocitve.letno[prenocitve.letno == "Kitajska (Ljudska republika)"] <- "Kitajska"

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

prenocitve.letno
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
motivi.prihoda <- motivi.prihoda[!is.na(motivi.prihoda$Sestevek),] %>%
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
motivi.prihoda[motivi.prihoda == "Osebna varnost med bivanjem"] <- "Občutek \nvarnosti"
motivi.prihoda[motivi.prihoda == "Možnosti za počitek in sprostitev"] <- "Sprostitev"
motivi.prihoda[motivi.prihoda == "Primernost za družinske počitnice"] <- "Družinsko \nokolje"
motivi.prihoda[motivi.prihoda == "Raznolika gastronomska ponudba"] <- "Gastronomija"
motivi.prihoda[motivi.prihoda == "Redne letalske povezave z državo"] <- "Letalske \npovezave"
motivi.prihoda[motivi.prihoda == "Rekreativne dejavnosti"] <- "Rekreacija"
motivi.prihoda[motivi.prihoda == "Skrb za zdravje in storitve dobrega počutja (velnes)"] <- "Velnes"

# ==============================================================================

# RAZLOGI ZA PRIHDO TUJCEV V SLOVENIJO IN PREVOZNO SREDSTVO

motivi.prihoda

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
  dplyr::select(-c(1))

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
  dplyr::select(-c(1))

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
  dplyr::select(-c(1))

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
colnames(izdatki) <- c("Leto", "Vrsta", "Stevilo")

# ==============================================================================

# IZDATKI ZA TURIZEM (zdruzitev treh tabel)
# stevilke so v milijonih €

IZDATKI <- izdatki
SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI
 
# ==============================================================================
# ==============================================================================

uredi <- function(tabela, leto){
  colnames(tabela)[1] <- "Prihodi"
  colnames(tabela)[2] <- "Prenocitve"
  colnames(tabela)[3] <- "Drzava"
  tabela <- tabela %>% na.omit() %>% 
    mutate(Leto = leto,
           Drzava = str_replace_all(Drzava, "(from)?([:blank:])(.+)", "\\3"))
}

leto2022 <- read_excel("podatki/2022.xlsx") %>%
  dplyr::select(c(2,10,18)) %>% uredi(2022)

leto2021 <- read_excel("podatki/2021.xlsx")%>%
  dplyr::select(c(2,10,18)) %>% uredi(2021)

leto2020 <- read_excel("podatki/2020.xlsx") %>%
  dplyr::select(c(2,8,14)) %>% uredi(2020)

leto2019 <- read_excel("podatki/2019.xlsx") %>%
  dplyr::select(c(2,8,14)) %>% uredi(2019)

leto2018 <- read_excel("podatki/2018.xlsx") %>%
    dplyr::select(c(2,5,8)) %>% uredi(2018)

vec.let.skupaj <- rbind(leto2022, leto2022, leto2020, leto2019, leto2018) %>% 
  pivot_longer( c(1,2),
                names_to = "Kaj",
                values_to = "Stevilo"
  )

vec.let.skupaj[vec.let.skupaj == "Domestic"] <- "Slovenia"
vec.let.skupaj[vec.let.skupaj == "Czech Republic"] <- "Czechia"
vec.let.skupaj[vec.let.skupaj == "Northern Macedonia"] <- "North Macedonia"
vec.let.skupaj[vec.let.skupaj == "Korea (Republic of)"] <- "South Korea"
vec.let.skupaj[vec.let.skupaj == "Russian Federation"] <- "Russia"
vec.let.skupaj[vec.let.skupaj == "United States"] <- "United States of America"
vec.let.skupaj[vec.let.skupaj == "Serbia"] <- "Republic of Serbia"

vec.let.skupaj$Stevilo <- as.integer(vec.let.skupaj$Stevilo)

# ==============================================================================

vec.let.skupaj

# ==============================================================================
# ==============================================================================

slovenci.prenocitve <- read_csv("podatki/prenocitve.slovencev.v.sloveniji.csv",
              locale = locale(encoding = "Windows-1250"),
              col_names=TRUE,
              col_types = cols(.default = col_guess()))

slovenci.prenocitve <- slovenci.prenocitve %>%
  pivot_longer(cols = colnames(slovenci.prenocitve)[-1],
               names_to = "Leto",
               values_to = "Stevilo" ) %>%
  mutate(Leto = str_replace_all(Leto, "(Prenočitve turistov )(\\d{4})([:alpha:])(\\d{2})(.*)", "\\2-\\4-01")) %>%
  dplyr::select(Leto, Stevilo)

# da linearna regresija dela:
slovenci.prenocitve$Stevilo <- as.integer(slovenci.prenocitve$Stevilo)
slovenci.prenocitve$Leto <- as.Date(slovenci.prenocitve$Leto)

# ==============================================================================

slovenci.prenocitve

# ==============================================================================
# ==============================================================================


