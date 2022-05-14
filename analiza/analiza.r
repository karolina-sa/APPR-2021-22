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
# ==============================================================================

# NAPOVEDNI MODEL
# priprava tabele

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
    values_to = "Število turistov"
  ) %>%
  na.omit() %>%
  dplyr::select(Leto, "Število turistov")



# ==============================================================================



