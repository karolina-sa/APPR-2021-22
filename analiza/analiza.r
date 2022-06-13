# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

# ==============================================================================
# Uporabljene funkcije pri napredni analizi:
# ==============================================================================

# funkcije uporabljene pri razvrščanju v skupine:

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "black"
    )+
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "black"
    )+
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "red", size = 2.5
    )+
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x, y = y, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue(h = c(180, 270)) +
    xlab("") +
    ylab("") +
    theme_classic() 
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  razdalje = dist(podatki)
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic() 
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# funkcije za prečno preverjanje:

ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    log.reg = glm(formula, data = podatki, family = "binomial"),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    log.reg = ifelse(
      predict(model, podatki, type = "response") >= 0.5,
      1, -1
    ),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yn.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yn - yn.hat) ^ 2
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yd.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yd != yd.hat)
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

# Razreži vektor x na k enako velikih kosov
razbitje = function(x, k) {
  # Razreži indekse vektorja na k intervalov
  razrez = cut(seq_along(x), k, labels = FALSE)
  # Razbij vektor na k seznamov
  # na osnovi razreza intervalov
  split(x, razrez)
}

pp.razbitje = function(n, k = 10, stratifikacija = NULL, seme = NULL) {
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
  if (is.null(stratifikacija)) {
    return(razbitje(sample(1:n), k))
  }
  # če pa opravljamo stratifikacijo, razbitje izvedemo za vsako
  # vrednost spremenljive stratifikacija posebej in nato
  # podmnožice združimo v skupno razbitje
  r = NULL
  for (v in levels(stratifikacija)) {
    # Če smo pri prvi vrednosti vzpostavimo razbitje
    if (is.null(r)) {
      # opravimo razbitje samo za primere z vrednostjo v
      r = razbitje(sample(which(stratifikacija == v)), k)
    } else {
      # opravimo razbitje za vrednost v
      # in podmnožice združimo s trenutnim razbitjem
      r.v = razbitje(sample(which(stratifikacija == v)), k)
      for (i in 1:k) {
        r[[i]] = c(r[[i]], r.v[[i]])
      }
    }
  }
  r
}

precno.preverjanje = function(podatki, razbitje, formula, algoritem, razvrscanje) {
  # pripravimo vektor za napovedi
  if (razvrscanje) {
    pp.napovedi = factor(rep(1, nrow(podatki)), levels = c(-1,1))
  } else {
    pp.napovedi = rep(0, nrow(podatki))
  }
  # gremo čez vse podmnožice Si razbitja S
  for (i in 1:length(razbitje)) {
    # naučimo se modela na množici S \ Si
    model = podatki[ -razbitje[[i]], ] %>% ucenje(formula, algoritem)
    
    # naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = podatki[ razbitje[[i]], ] %>% napovedi(model, algoritem)
  }
  if (razvrscanje) {
    mean(pp.napovedi != podatki$yd)
  } else {
    mean((pp.napovedi - podatki$yn) ^ 2)
  }
}
# Funkciji Predictor$new moramo povedati kako napovedujemo z modelom naključnih gozdov 
pfun = function(model, newdata) {
  predict(model, data = newdata, predict.all = FALSE)$predictions
}

# ==============================================================================
# RAZVRSTITEV V SKUPINE
# ==============================================================================

# 1) k-means

# koliko skupin naj izberem pri razdelitvi z metodo voditeljev:
r.hc = TURIZEM.EVROPA[, -1] %>% obrisi(hc = TRUE)
r.km = TURIZEM.EVROPA[, -1] %>% obrisi(hc = FALSE)

r.hc.plt <- diagram.obrisi(r.hc)
r.km.plt <- diagram.obrisi(r.km)
# oba predlagata dve skupini

# - - - - - - - - -

set.seed(42)

k = 3
skupine <- TURIZEM.EVROPA[,-1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()
# print(skupine)

tabela.skupine.k.means <- TURIZEM.EVROPA %>% 
  mutate(Skupine = as.numeric(skupine))

# uvozim zemljevid, ki ga bom potrebovala tudi v nadaljevanju. Prikazala se bo samo Evropa
zemljevid <-
  uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    "ne_50m_admin_0_countries",
    mapa = "zemljevidi",
    pot.zemljevida = "",
    encoding = "UTF-8"
  ) %>%
  fortify() %>% 
  filter(CONTINENT %in% c("Europe"),
         long < 50 & long > -25 & lat > 35 & lat < 85)

names(tabela.skupine.k.means)[1] <- "ADMIN"

zemljevid.kmeans <- ggplot() +
  aes(x = long, y = lat, group = group, fill = factor(Skupine)) +
  geom_polygon(data = tabela.skupine.k.means %>% 
                 right_join(zemljevid, by = "ADMIN")) +
  xlab("") +
  ylab("") +
  ggtitle("Razvrstitev Evropskih držav v tri skupine z metodo k-tih voditeljev") +
  coord_fixed(ratio = 1) +
  guides(fill=guide_legend(title="Skupina")) +
  theme_bw() +
  theme(plot.caption.position = "plot")  +
  scale_fill_brewer(palette = "YlGnBu", na.translate = F)

zemljevid.kmeans

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 2) HIERARHIČNO RAZVRŠČANJE

X <- TURIZEM.EVROPA[,-1] %>% as.matrix() %>% scale()
drzave <- TURIZEM.EVROPA[, 1] %>% unlist()
razdalje <- TURIZEM.EVROPA[, -1] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
# plot(dendrogram,
#      labels = TURIZEM.EVROPA$Drzava,
#      ylab = "višina",
#      main = NULL)

# izračun kolen:
r = hc.kolena(dendrogram)
diagram.kolena(r)

# za kolena predlaga: 2, 3, 4, 6

drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

# izberem število skupin:
k = 4

skupine <- TURIZEM.EVROPA[, 2] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()

tabela.skupine.hierarh <- TURIZEM.EVROPA %>%
  mutate(Skupine = as.numeric(skupine))

names(tabela.skupine.hierarh)[1] <- "ADMIN"

zemljevid.hierarh <- ggplot() +
  aes(x = long, y = lat, group = group, fill = factor(Skupine)) +
  geom_polygon(data = tabela.skupine.hierarh %>% 
                 right_join(zemljevid, by = "ADMIN")) +
  xlab("") +
  ylab("") +
  ggtitle("Razvrstitev Evropskih držav v štiri skupine po Wardovi metodi") +
  coord_fixed(ratio = 1) +
  guides(fill=guide_legend(title="Skupina")) +
  theme_bw() +
  theme(plot.caption.position = "plot") +
  scale_fill_brewer(palette = "YlGnBu", na.translate = F)

zemljevid.hierarh

# v skupini s Slovnijo:
# tabela.skupine.hierarh[tabela.skupine.hierarh$Skupine == 1, 1]



# ==============================================================================
# NAPOVEDNI MODEL
# ==============================================================================

# priprava tabele:
slovenija.turizem <- read_csv("podatki/turizem_svetovno.csv",
                             skip = 4,
                             locale = locale(encoding = "Windows-1250"),
                             col_names=TRUE, 
                             col_types = cols(.default = col_guess()))
slovenija.turizem <- slovenija.turizem[slovenija.turizem$`Country Name` == "Slovenia",-c(2:39)]
slovenija.turizem <- slovenija.turizem %>%
  pivot_longer(
    cols = colnames(slovenija.turizem)[-1],
    names_to = "Leto",
    values_to = "Stevilo"
  ) %>%
  na.omit() %>%
  dplyr::select(Leto, "Stevilo")
slovenija.turizem$Stevilo <- as.integer(slovenija.turizem$Stevilo)

# tabela in dva stolpca, prvi so leta, drugi pa število turističnih obiskov

# napovedovanje:
zamakni <- function(x, n){c(rep(NA, n), x)[1:length(x)]}

# napovedovanje glede na prejšnja 4 leta:

naredi.df.4 <- function(x){
  data.frame(pricak  = x,
             "Leto 2020"  = zamakni(x, 1),
             "Leto 2019" = zamakni(x, 2),
             "Leto 2018" = zamakni(x, 3),
             "Leto 2017" = zamakni(x, 4))
}

df.4 <- naredi.df.4(slovenija.turizem$Stevilo)
model = ranger(formula = pricak ~ ., data = df.4 %>% drop_na())

n = nrow(df.4)

df.4.2 = df.4
for(i in 1:3){
  df.4.2 = naredi.df.4(c(df.4.2$pricak, NA))
  napoved = predict(model, data = df.4.2[n+i,])$predictions
  df.4.2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi = df.4.2[(n+1):(n+3),1]

# s temi podaki bom dopolnila tabelo slovenija.turizem
slovenija.turizem.z.napovednjo <- slovenija.turizem
slovenija.turizem.z.napovednjo$Leto <- as.integer(slovenija.turizem.z.napovednjo$Leto)
slovenija.turizem.z.napovednjo$Stevilo <- as.numeric(slovenija.turizem.z.napovednjo$Stevilo)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi[i])
  slovenija.turizem.z.napovednjo[nrow(slovenija.turizem.z.napovednjo) + 1, ] <-
    list(novo_leto, nov_podatek)
}

Leto <- 1995:2023
napovedovanje.graf <- ggplot(slovenija.turizem.z.napovednjo, 
                             aes(x = Leto, y = Stevilo)) +
  geom_line(color = "black") + 
  slovenija.turizem.z.napovednjo %>%
  filter(Leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = Leto, y = Stevilo),
    color = "red"
  ) +
  labs(
    x = "Leto",
    y = "Število turističnih obiskov",
    title = "Število turističnih obiskov Slovenija ob leta 1995 do 2020 z napovedjo \nza leta 2021, 2022 in 2023 "
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Leto", labels = as.character(Leto), breaks = Leto)
  
napovedovanje.graf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prečno preverjanje napovedovanja glede na pretekla 4 leta:

# vrednosti NA motijo delovanje funckije
df.4 <- df.4 %>% na.omit()

ucni <- pp.razbitje(df.4, stratifikacija = df.4$pricak)

# Pripravimo le napovedne spremenljivke, ki jih rabi funkcija Predictor$new
X <- df.4[,-1]

# pripravimo najprej objekt razreda Prediktor
# prvi argument funkcije je model,
# drugi in tretji so podatki o napovednih
# in ciljni spremenljivki,
# zadnji pa funkcija za napovedovanje

reg.pred = Predictor$new(
  model,
  data = X, y = df.4$pricak,
  predict.fun = pfun
)

# na koncu uporabimo funkcijo FeatureImp$new
reg.moci = FeatureImp$new(reg.pred, loss = "mse")

reg.moci.4.plot <- plot(reg.moci) + theme_bw()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# napovedovanje glede na prejšnje 3 leta:

naredi.df.3 <- function(x){
  data.frame(pricak  = x,
             "Leto 2020"  = zamakni(x, 1),
             "Leto 2019" = zamakni(x, 2),
             "Leto 2018" = zamakni(x, 3))
}

df.3 <- naredi.df.3(slovenija.turizem$Stevilo)
model.3 = ranger(formula = pricak ~ ., data = df.3 %>% drop_na())

n = nrow(df.3)

df.3.2 = df.3
for(i in 1:3){
  df.3.2 = naredi.df.3(c(df.3.2$pricak, NA))
  napoved = predict(model.3, data = df.3.2[n+i,])$predictions
  df.3.2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi.3 = df.3.2[(n+1):(n+3),1]

# s temi podaki bom dopolnila tabelo slovenija.turizem
slovenija.turizem.z.napovednjo.3 <- slovenija.turizem
slovenija.turizem.z.napovednjo.3$Leto <- as.integer(slovenija.turizem.z.napovednjo.3$Leto)
slovenija.turizem.z.napovednjo.3$Stevilo <- as.integer(slovenija.turizem.z.napovednjo.3$Stevilo)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi.3[i])
  slovenija.turizem.z.napovednjo.3[nrow(slovenija.turizem.z.napovednjo.3) + 1, ] <-
    list(novo_leto, nov_podatek)
}

Leto <- 1995:2023
napovedovanje.graf.3 <- ggplot(slovenija.turizem.z.napovednjo.3, 
                             aes(x = Leto, y = Stevilo)) +
  geom_line(color = "black") + 
  slovenija.turizem.z.napovednjo.3 %>%
  filter(Leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = Leto, y = Stevilo),
    color = "red"
  ) +
  labs(
    x = "Leto",
    y = "Število turističnih obiskov",
    title = "Število turističnih obiskov Slovenija ob leta 1995 do 2020 z napovedjo \nza leta 2021, 2022 in 2023 "
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Leto", labels = as.character(Leto), breaks = Leto)

napovedovanje.graf.3

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prečno preverjanje napovedovanja glede na pretekla 3 leta:

# vrednosti NA motijo delovanje funckije
df.3 <- df.3 %>% na.omit()

ucni.3 <- pp.razbitje(df.3, stratifikacija = df.3$pricak)

# Pripravimo le napovedne spremenljivke, ki jih rabi funkcija Predictor$new
X.3 <- df.3[,-1]

# pripravimo najprej objekt razreda Prediktor
# prvi argument funkcije je model,
# drugi in tretji so podatki o napovednih
# in ciljni spremenljivki,
# zadnji pa funkcija za napovedovanje

reg.pred.3 = Predictor$new(
  model.3,
  data = X.3, y = df.3$pricak,
  predict.fun = pfun
)

# na koncu uporabimo funkcijo FeatureImp$new
reg.moci.3 = FeatureImp$new(reg.pred.3, loss = "mse")

reg.moci.3.plot <- plot(reg.moci.3) + theme_bw()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# skupni graf napovedovanja (za poročilo):
Leto <- 1995:2023
napovedovanje.graf.skupaj <- ggplot(slovenija.turizem.z.napovednjo, 
                             aes(x = Leto, y = Stevilo)) +
  geom_line(color = "black") + 
  slovenija.turizem.z.napovednjo %>%
  filter(Leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = Leto, y = Stevilo),
    color = "goldenrod1",
    size = 0.75
  ) +
  slovenija.turizem.z.napovednjo.3 %>%
  filter(Leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = Leto, y = Stevilo),
    color = "red",
    linetype = "dotdash",
    size = 0.75
  ) +
  labs(
    x = "Leto",
    y = "Število turističnih obiskov",
    title = "Število turističnih obiskov Slovenija ob leta 1995 do 2020 z napovedjo \nza leta 2021, 2022 in 2023 "
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("Leto", labels = as.character(Leto), breaks = Leto) +
  annotate(geom="text", x=2022, y=3000000, label="napoved glede \nna pretekla \n4 leta",
           color="goldenrod1", size = 2.5) +
  annotate(geom="text", x=2022.5, y=1900000, label="napoved \nglede na \npretekla \n3 leta",
           color="red", size = 2.5)

napovedovanje.graf.skupaj



# ==============================================================================
# LINEARNA REGRESIJA
# ==============================================================================

linearna.regrasija.graf <- 
  ggplot(slovenci.prenocitve, aes(x = Leto, y = Število)) +
  geom_point(stat='identity', position='identity', aes(colour=Število),size=1.3) +
  scale_colour_gradient(low='yellow', high='#de2d26') +
  xlab("Leto") +
  ylab("Število prenočitev") +
  ggtitle("Število prenočitev Slovencev v Sloveniji od leta 2010 do leta 2022") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_smooth(method = "lm", formula = y ~ x, colour="black", size=0.7)

linearna.regrasija.graf

