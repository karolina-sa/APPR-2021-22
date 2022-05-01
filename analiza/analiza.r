# 4. faza: Napredna analiza podatkov

source("lib/libraries.r")

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
      color = "red"
    )+
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    )+
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
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
# za kolena predlaga: 2, 4, 6

drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

# izberem število skupin:
k = 6

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
# "Albania", "Bosnia and Herzegovina", "Cyprus", "Finland", "Iceland", 
# "Liechtenstein", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", 
# "North Macedonia", "San Marino", "Slovenia" 

# ==============================================================================

# 2)

X <- TURIZEM.EVROPA.POVRSINA[2] %>% as.matrix() %>% scale()
drzave <- TURIZEM.EVROPA.POVRSINA[, 1] %>% unlist()
razdalje <- TURIZEM.EVROPA.POVRSINA[, 2] %>% dist()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")
# plot(dendrogram,
#      labels = TURIZEM.EVROPA.POVRSINA$Drzava,
#      ylab = "višina",
#      main = NULL)

# izračun kolen:
r = hc.kolena(dendrogram)
diagram.kolena(r)
# za kolena predlaga: 2, 5

drzave.x.y <-
  as_tibble(razdalje %>% cmdscale(k = 2)) %>%
  bind_cols(drzave) %>%
  dplyr::select(drzava = ...3, x = V1, y = V2)

# izberem število skupin:
k = 5

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
# Albanija, Bulgarija, Estonija, Nemčija, Gričija, Irska, Latvija, Litva, Črna gora,
# Portugalska, Združeno kraljevsto


# ==============================================================================
# ==============================================================================


