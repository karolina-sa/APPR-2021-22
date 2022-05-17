# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

# ==============================================================================
# METODA VODITELJEV
# ==============================================================================

# uporabljene funkcije:

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

# ==============================================================================

# Evropski turizem:
# Razdelitev v skupinev (zanima me, turizmu katerih evropskih državah je turizem
#           Slovenije podoben)
# Metoda voditeljev

# 1)

X <- TURIZEM.EVROPA[2] %>% as.matrix() %>% scale()
drzave <- TURIZEM.EVROPA[, 1] %>% unlist()
razdalje <- TURIZEM.EVROPA[, 2] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
# plot(dendrogram,
#      labels = TURIZEM.EVROPA$Drzava,
#      ylab = "višina",
#      main = NULL)

# izračun kolen:
r = hc.kolena(dendrogram)
diagram.kolena(r)
# za kolena predlaga: 2, 4

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

turizem.evropa.skupine <- diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

# V poročilo gre:
turizem.evropa.skupine

# pogledamo s katerimi državami je slovenija v skupini:
tabela.turizem.evropa.skupine <- tibble(TURIZEM.EVROPA$Drzava, skupine) %>%
  filter(skupine == 1)
# drzave s katerimi je Slovenije v skupini:
drzave.slovenija.skupine <- tabela.turizem.evropa.skupine$`TURIZEM.EVROPA$Drzava`%>% 
  unlist()
drzave.slovenija.skupine
# [1] "Albania"              "Andora"               "Belorusija"          
# [4] "Belgija"              "Bosna in Herzegovina" "Bulgaria"            
# [7] "Ciper"                "Estonija"             "Finska"              
# [10] "Islandija"            "Irska"                "Latvija"             
# [13] "Lihtenstajn"          "Litva"                "Luksemburg"          
# [16] "Malta"                "Moldavija"            "Monako"              
# [19] "Crna gora"            "Nizozemska"           "Severna Makedonija"  
# [22] "Norveska"             "Portugalska"          "Romunija"            
# [25] "San Marino"           "Slovenija"            "Svedska"             
# [28] "Svica"                "Ukrajina" 

# ==============================================================================

# 2)

X <- TURIZEM.EVROPA.POVRSINA[2] %>% as.matrix() %>% scale()
drzave <- TURIZEM.EVROPA.POVRSINA[, 1] %>% unlist()
razdalje <- TURIZEM.EVROPA.POVRSINA[, 2] %>% dist()
dendrogram1  <- dist(X) %>% hclust(method = "ward.D")
# plot(dendrogram,
#      labels = TURIZEM.EVROPA.POVRSINA$Drzava,
#      ylab = "višina",
#      main = NULL)

# izračun kolen:
r1 = hc.kolena(dendrogram1)
diagram.kolena(r1)
# za kolena predlaga: 2, 4, 6

drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

# izberem število skupin:
k = 4

skupine <- TURIZEM.EVROPA.POVRSINA[, 2] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()

turizem.evropa.povrsina.skupine <- diagram.skupine(drzave.x.y, drzave.x.y$drzava, skupine, k)

# V poročilo gre:
turizem.evropa.povrsina.skupine

# pogledamo s katerimi državami je slovenija v skupini:
tabela.turizem.evropa.povrsina.skupine <- tibble(TURIZEM.EVROPA.POVRSINA$Drzava, skupine) %>%
  filter(skupine == 1)
# drzave s katerimi je Slovenije v skupini:
drzave.slovenija.skupine <- tabela.turizem.evropa.povrsina.skupine$`TURIZEM.EVROPA.POVRSINA$Drzava`%>% 
  unlist()
drzave.slovenija.skupine
# [1] "Albania"             "Bulgaria"            "Estonija"            "Nemcija"            
# [5] "Irska"               "Latvija"             "Litva"               "Crna gora"          
# [9] "Portugalska"         "Slovenija"           "Zdruzeno kraljestvo"


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
    values_to = "Število"
  ) %>%
  na.omit() %>%
  dplyr::select(Leto, "Število")
slovenija.turizem$Število <- as.integer(slovenija.turizem$Število)

# tabela in dva stolpca, prvi so leta, drugi pa število turističnih obiskov

# napovedovanje:
zamakni <- function(x, n){c(rep(NA, n), x)[1:length(x)]}

naredi.df <- function(x){
  data.frame(pricak  = x,
             pricak1  = zamakni(x, 1),
             pricak2 = zamakni(x, 2),
             pricak3 = zamakni(x, 3),
             pricak4 = zamakni(x, 4))
}

df <- naredi.df(slovenija.turizem$Število)
model = ranger(formula = pricak ~ ., data = df %>% drop_na())

n = nrow(df)

df2 = df
for(i in 1:3){
  df2 = naredi.df(c(df2$pricak, NA))
  napoved = predict(model, data = df2[n+i,])$predictions
  df2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi = df2[(n+1):(n+3),1]

# s temi podaki bom dopolnila tabelo slovenija.turizem
slovenija.turizem.z.napovednjo <- slovenija.turizem
slovenija.turizem.z.napovednjo$Leto <- as.integer(slovenija.turizem.z.napovednjo$Leto)
slovenija.turizem.z.napovednjo$Število <- as.integer(slovenija.turizem.z.napovednjo$Število)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi[i])
  slovenija.turizem.z.napovednjo[nrow(slovenija.turizem.z.napovednjo) + 1, ] <-
    list(novo_leto, nov_podatek)
}

Leto <- 1995:2023
napovedovanje.graf <- ggplot(slovenija.turizem.z.napovednjo, 
                             aes(x = Leto, y = Število)) +
  geom_line(color = "black") + 
  slovenija.turizem.z.napovednjo %>%
  filter(Leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = Leto, y = Število),
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

