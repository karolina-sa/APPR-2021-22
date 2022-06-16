# 3. faza: Vizualizacija podatkov

source("lib/libraries.r", encoding="UTF-8")
source("lib/uvozi.zemljevid.r")

# ==============================================================================
# GRAFIČNA ANALIZA
# ==============================================================================

# prenočitev po regijah

prenocitev.regije.graf <- ggplot(prenocitve.regije %>% filter(Drzava != "Skupaj"),
                                 aes(x = Regija, y = Prenocitve, fill = Drzava)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(y = "Število prenočitev", fill = "Kdo je prenočil") +
  ggtitle("Prenočitve slovencev in tujcev po regijah") +
  theme_bw() +
  scale_fill_manual(labels = c("Prenočitve slovencev", "Prenočitve tujcev"), 
                    values = c("#3288BD", "#ABDDA4"))  

prenocitev.regije.graf

# ==============================================================================

# nastanitvena doba po regijah (šteje pod napredno analizo)

nastanitvena.doba.regije.graf <- ggplot(nastanitvena.doba.regije,
                                        cex.axis = 0.5) +
  aes(x = Mesec, y = StDni, color = Drzava) +
  geom_jitter(size = 0.7) + 
  scale_color_manual(values = c('lightsalmon','firebrick1')) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, size=0.7) +
  facet_wrap(.~ Regija) +
  labs(
    x = "Mesec",
    y = "Nastanitevna doba (v dneh)",
    color = "Država"
  ) +
  ggtitle("Nastanitvena doba v dneh po regijah v določenem mesecu") +
  guides(fill=guide_legend(title = "Tip turista")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8),
        strip.background = element_rect(colour="black", fill="grey95")) +
  scale_x_continuous("Leto", labels = as.character(c(1:12)), breaks = c(1:12)) 

nastanitvena.doba.regije.graf

# ==============================================================================

# motivi prihoda v Slovenijo

cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE

motivi.prihoda.graf <- ggplot(motivi.prihoda[motivi.prihoda$Drzava == "Skupaj" & 
                                               motivi.prihoda$Presoja != "Pomembnost",]) +
  aes(x = "", y = StGlasov, fill = Presoja) +
  geom_bar(width = 1, stat = "identity") +
  cp +
  facet_wrap(.~ Motiv, ncol = 5, scales="free") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Pomembnost izbranih motivov tujim turistom za prihod v Slovenijo") +
  labs(
    x = "",
    y = "",
    fill = "Pomembnost motivov"
  ) +
  scale_fill_brewer(palette="Reds",
                    labels = c("Nepomemben", "Niti pomemben, \nniti nepomemben", "Pomemben")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        strip.background = element_rect(colour="black", fill="grey95"))

motivi.prihoda.graf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

motivi.prihoda.pomembnost.graf <- ggplot(motivi.prihoda[motivi.prihoda$Drzava != "Skupaj" &
                                                          motivi.prihoda$Presoja == "Pomembnost",]) +
  aes(x = Drzava, y = StGlasov, fill = Drzava) +
  geom_bar(stat = "identity") +
  facet_wrap(.~ Motiv, ncol = 5) +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) +
  labs(
    x = "Država",
    y = "Pomembnost",
    fill = "Država opazovanja"
  ) +
  ggtitle("Pomembnost izbranih motivov po državah") +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_rect(colour="black", fill="grey95")
  )

motivi.prihoda.pomembnost.graf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# analiza motiva igralništva za Italijo in Avstrijo

cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE

igralnistvo.italija.avstrija.graf <- ggplot(motivi.prihoda[motivi.prihoda$Drzava %in% c("Italija", "Avstrija") &
                          motivi.prihoda$Presoja != "Pomembnost" &
                            motivi.prihoda$Motiv == "Igre na srečo",]) +
  aes(x = "", y = StGlasov, fill = Presoja) +
  geom_bar(width = 1, stat = "identity") +
  cp +
  facet_wrap(.~ Drzava, ncol = 2, scales="free") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(
    x = "",
    y = "",
    fill = "Pomembnost igralništva"
  ) +
  ggtitle("Pomembnost igralništva v Avstriji in Italiji") +
  scale_fill_brewer(palette="Reds",
                    labels = c("Nepomemben", "Niti pomemben, \nniti nepomemben", "Pomemben")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        strip.background = element_rect(colour="black", fill="grey95"))

igralnistvo.italija.avstrija.graf

# ==============================================================================

# stevilo zaposlenih v turizmu

stevilo.zaposlenih.graf <- ggplot(stevilo.zaposlenih, 
                                  mapping = aes(x = Leto, y = Stevilo, fill = Storitve)) +
  geom_col() +
  ggtitle("Število zaposlenih v Sloveniji v turizmu (v tisočih)") +
  guides(fill=guide_legend(title = "Tip zaposlitve v turizmu")) +
  scale_fill_brewer(palette="Spectral") +
  ylab("Število") +
  theme_bw()
  
stevilo.zaposlenih.graf

# ==============================================================================

# turizem BDP

BDP.turizem.graf <- ggplot(BDP.turizem) + 
  aes(x = Leto, y = Stevilo, group=1) +
  geom_line() +
  geom_point() +
  labs(
    y = "Število"
  ) +
  theme_bw() +
  ggtitle("BDP od turizma v Sloveniji") 

BDP.turizem.graf

# ==============================================================================

# izdatki za turizem

izdatki.graf <- ggplot(data=IZDATKI, aes(x = Leto, y = Stevilo, color = Vrsta)) +
  geom_point() +
  geom_line(aes(group = Vrsta), size=1) +
  scale_fill_brewer(palette="Spectral") +
  labs(
    y = "Število",
    color = "Čigavi izdatki"
  ) +
  theme_bw() +
  ggtitle("Izdatki za turizem")

izdatki.graf

# sestava potrošnje tujcev v Sloveniji

sestava.izdatkov.graf1 <- ggplot(data=sestava.turisticne.potrosnje.tujcev.v.sloveniji,
                                aes(x = Leto, y = Izdatek, color = Storitve)) +
  geom_point() +
  geom_line(size=1) +
  scale_fill_brewer(palette="Spectral") +
  labs(
    color = "Storitev izdatka"
  ) +
  theme_bw() + 
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")

sestava.izdatkov.graf1

# sestava potrošnje z boxplot

sestava.izdatkov.graf2 <- ggplot(data=sestava.turisticne.potrosnje.tujcev.v.sloveniji,
                                 aes(x = Storitve, y = Izdatek)) +
  geom_boxplot(fill="grey95", notch = FALSE) +
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji") +
  theme_bw() +
  labs(caption = "Pojasnilo: Rdeči kvadatki predstavljajo povprečje posamene vrste izdatkov.")

sestava.izdatkov.graf2

# ==============================================================================

# Prenočitve Slovencev v tujini

prenocitve.slovencev.v.tujini <- 
  ggplot(prenocitve.letno[! prenocitve.letno$DRŽAVA %in% c("Država - SKUPAJ", "DOMAČI", "TUJI"),],
         aes(x = reorder(DRŽAVA, Stevilo), y = Stevilo)) +
  coord_flip() +
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .33, alpha = .3
  ) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 15, 
    size = 1.5,
    color = "red",
    stroke = .8
  ) + 
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .33, shape = 1
  ) +
  xlab("Država prenočitve") +
  ylab("Število prenočitev") +
  ggtitle("Število prenočitev Slovencev po posameznih državah") +
  labs(caption = "Pojasnilo: Črne pikice so podatki med letom 2010 in 2020. 
       Kvadratki oranžne barve predstavljajo povprečje črnih pikic.") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) 

prenocitve.slovencev.v.tujini




# ==============================================================================
# PROSTORSKA ANALIZA (ZEMLJEVIDI)
# ==============================================================================

# ZEMLJEVID EVROPE, KI PRIKAZUJE PRIHODE IZ RAZLIČNIH DRŽAV:

# priprava tabel za evropski zemljevid:

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
                       long < 50 & long > -30 & lat > 35 & lat < 85)

names(vec.let.skupaj)[1] <- "ADMIN"
drzave.prihodi <- vec.let.skupaj[vec.let.skupaj$Kaj == "Prihodi",] %>%
  group_by(ADMIN) %>%
  summarize(Prihodi = mean(Stevilo, na.rm = TRUE))

drzave.prenocitve <- vec.let.skupaj[vec.let.skupaj$Kaj == "Prenocitve",] %>%
  group_by(ADMIN) %>%
  summarize(Prenocitve = mean(Stevilo, na.rm = TRUE))

options("scipen"=100, "digits"=4)
zemljevid.prihodi <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Prihodi) +
  geom_polygon(data = drzave.prihodi %>% right_join(zemljevid, by = "ADMIN"),
               color = "grey38", size = 0.2) +
  xlab("") +
  ylab("") +
  ggtitle("Število turistov iz Evropskih držav, ki so prišli v Slovenijo") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral",
                       na.value="white")

zemljevid.prihodi

zemljevid.prenocitve <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Prenocitve) +
  geom_polygon(data = drzave.prenocitve %>% right_join(zemljevid, by = "ADMIN"),
               color = "grey38", size = 0.2) +
  xlab("") +
  ylab("") +
  ggtitle("Število turistov iz Evropskih držav, ki so prenočili v Sloveniji") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral",
                       na.value="white") 

zemljevid.prenocitve

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

zemljevid_svet <-
  uvozi.zemljevid(
    "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    "ne_50m_admin_0_countries",
    mapa = "zemljevidi",
    pot.zemljevida = "",
    encoding = "UTF-8"
  ) %>%
  fortify() %>% 
  filter(long < 170 & long > -170 & lat > -52 & lat < 83)

options("scipen"=100, "digits"=4)
zemljevid.svet.prihodi <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Prihodi) +
  geom_polygon(data = drzave.prihodi %>% right_join(zemljevid_svet, by = "ADMIN"),
               color = "grey38", size = 0.2) +
  xlab("") +
  ylab("") +
  ggtitle("Obiskanost Slovenije s strani tujih turistov") +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral",
                       na.value="white")

zemljevid.svet.prihodi

# ta zemljevid ni tako pregleden, zato ga bom v poročilu "približala" samo na Evropo


