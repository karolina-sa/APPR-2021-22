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
  ylab("Število prenočitev") +
  ggtitle("Prenočitve slovencev in tujcev po regijah") +
  scale_fill_discrete(name = "Legenda", labels = c("Prenočitve slovencev", "Prenočitve tujcev")) + 
  theme_bw() +
  scale_fill_manual(values = c('#ABDDA4','#3288BD')) 

prenocitev.regije.graf

# ==============================================================================

# nastanitvena doba po regijah

nastanitvena.doba.regije.graf <- ggplot(nastanitvena.doba.regije,
                                        cex.axis = 0.5) +
  aes(x = Mesec, y = StDni, color = Drzava) +
  geom_jitter(size = 0.7) + 
  scale_color_manual(values = c('lightsalmon','firebrick1')) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, size=0.7) +
  facet_wrap(.~ Regija) +
  xlab("Mesec") +
  ylab("Nastanitvena doba (v dneh)") +
  ggtitle("Nastanitvena doba v dneh po regijah v določenem mesecu") +
  guides(fill=guide_legend(title = "Tip turista")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8)) +
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
  xlab("") +
  ylab("") +
  scale_fill_brewer(palette="OrRd") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

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
  xlab("Država") +
  ylab("Pomembnost") +
  ggtitle("Pomembnost izbranih motivov po državah") +
  guides(fill=guide_legend(title = "Država opazovanja")) +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
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
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("") +
  ylab("") +
  ggtitle("Pomembnost igralništva v Avstriji in Italiji") +
  scale_fill_brewer(palette="OrRd") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())


igralnistvo.italija.avstrija.graf

# ==============================================================================

nastanitveni.obrat.regije.graf <- ggplot(nastanitveni.obrat.regije[nastanitveni.obrat.regije$Regija != "SLOVENIJA",]) +
  aes(x = Mesec, fill = Tip) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  facet_wrap(.~ Regija) + 
  ylim(0,6.5) +
  ggtitle("Obiskanost regij glede na nastanitveni obrat po regijah") +
  guides(fill=guide_legend(title = "Tip nastanitve")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8)) +
  scale_x_continuous("Leto", labels = as.character(c(1:12)), breaks = c(1:12)) +
  scale_fill_manual(values = c('#ABDDA4','#FDAE61', '#3288BD'))  

nastanitveni.obrat.regije.graf

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
  theme_bw() +
  ggtitle("BDP od turizma v Sloveniji") 

BDP.turizem.graf

# ==============================================================================

# izdatki za turizem

izdatki.graf <- ggplot(data=IZDATKI, aes(x = Leto, y = Stevilo, color = Vrsta)) +
  geom_point() +
  geom_line(aes(group = Vrsta), size=1) +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  ggtitle("Izdatki za turizem")

izdatki.graf

# sestava potrošnje tujcev v Sloveniji

sestava.izdatkov.graf1 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
                                aes(x = Leto, y = Izdatek, color = Storitve)) +
  geom_point() +
  geom_line(size=1) +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() + 
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")

sestava.izdatkov.graf1

# sestava potrošnje z boxplot

sestava.izdatkov.graf2 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
                                 aes(x = Storitve, y = Izdatek)) +
  geom_boxplot(fill="grey98", notch = FALSE) +
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="coral") +
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
    color = "coral",
    stroke = .8
  ) + 
  ggbeeswarm::geom_quasirandom(
    size = 1, width = .33, shape = 1
  ) +
  xlab("Število prenočitev") +
  ylab("Država prenočitve") +
  ggtitle("Število prenočitev Slovencev po državah") +
  labs(caption = "Pojasnilo: Črne pikice so podatki med letom 2010 in 2020. 
       Kvadratki oranžne barve predstavljajo povprečje črnih pikic.") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) 

prenocitve.slovencev.v.tujini

# # ==============================================================================
# 
# # Število prenočitev v Sloveniji po letih
# 
# stevilo.prenocitev.letno.graf <- ggplot(data = stevilo.prenocitev.letno,
#                                         mapping = aes(x = Leto, y = Stevilo)) +
#   geom_bar(stat = "identity",
#            fill = "gray70") +
#   scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
#   ylab("Število prenočitev") +
#   ggtitle("Število prenočitev v Sloveniji po letih") 
# 
# stevilo.prenocitev.letno.graf +
#   stevilo.prenocitev.letno %>%
#     filter(Leto == 2020) %>%
#   geom_bar(
#     mapping = aes(x = Leto, y = Stevilo),
#     stat = "identity",
#     fill="steelblue2"
#   )
# 
# stevilo.prenocitev.letno.boxplot.graf <- ggplot(data = stevilo.prenocitev.letno,
#                                                 aes(y = Stevilo)) +
#   geom_boxplot() +
#   scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# 
# stevilo.prenocitev.letno.boxplot.graf
# 
# 
# 
# # ==============================================================================
# 
# # vpliv izobazbe na odločitve potovanja
# izobrazba1.graf <- ggplot(odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                         "Osnovnošolska ali manj",],
#                          aes(x = Leto, y = Stevilo, color = Odhod)) +
#   geom_point() + 
#   geom_line() + # NE DELA KER MISSING VALUES 
#   ylim(0, 700) +
#   geom_text(data = odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                   "Osnovnošolska ali manj",] 
#             %>% filter(Leto == last(Leto)), 
#             aes(label = Odhod, 
#                 x = Leto + 1, 
#                 y = Stevilo, 
#                 color = Odhod)) + 
#   coord_cartesian(clip = 'off') +
#   ggtitle("Osnovnošolska ali manj") +
#   theme(legend.position = "none",
#         plot.margin = margin(0, 1.5, 0, 0, "cm"))
# 
# izobrazba1.graf
# 
# izobrazba2.graf <- ggplot(odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                          "Srednješolska",],
#                           aes(x = Leto, y = Stevilo, color = Odhod)) +
#   geom_point() + 
#   geom_line(aes(group = Odhod)) + 
#   ylim(0, 700) +
#   geom_text(data = odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                   "Srednješolska",] 
#             %>% filter(Leto == last(Leto)), 
#             aes(label = Odhod, 
#                 x = Leto + 1, 
#                 y = Stevilo, 
#                 color = Odhod)) + 
#   coord_cartesian(clip = 'off') +
#   ggtitle("Srednješolska") +
#   theme(legend.position = "none",
#         plot.margin = margin(0, 1.5, 0, 0, "cm"))
# 
# izobrazba2.graf
# 
# izobrazba3.graf <- ggplot(odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                          "Višješolska, visokošolska ali več",],
#                           aes(x = Leto, y = Stevilo, color = Odhod)) +
#   geom_point() + 
#   geom_line(aes(group = Odhod)) +
#   ylim(0, 700) +
#   geom_text(data = odlocitev.glede.na.izobrazbo[odlocitev.glede.na.izobrazbo$Izobrazba == 
#                                                   "Višješolska, visokošolska ali več",] 
#             %>% filter(Leto == last(Leto)), 
#             aes(label = Odhod, 
#                 x = Leto + 1, 
#                 y = Stevilo, 
#                 color = Odhod)) + 
#   coord_cartesian(clip = 'off') +
#   scale_x_continuous(breaks = scales::pretty_breaks(10)) +
#   theme(legend.position = "none",
#         plot.margin = margin(0, 1.5, 0, 0, "cm")) +
#   ggtitle("Višješolska, visokošolska ali več")
# 
# izobrazba3.graf
# 
# izobrazba.graf <- grid.arrange(izobrazba1.graf, izobrazba2.graf, izobrazba3.graf, ncol=3)
# 
# izobrazba.graf



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

# priprava tabel

names(vec.let.samo.prave.drzave)[1] <- "ADMIN"
drzave.prihodi <- vec.let.samo.prave.drzave[vec.let.samo.prave.drzave$Kaj == "Prihodi",] %>%
  group_by(ADMIN) %>%
  summarize(Prihodi = mean(Število, na.rm = TRUE))

drzave.prenocitve <- vec.let.samo.prave.drzave[vec.let.samo.prave.drzave$Kaj == "Prenočitve",] %>%
  group_by(ADMIN) %>%
  summarize(Prenocitve = mean(Število, na.rm = TRUE))

options("scipen"=100, "digits"=4)
zemljevid.prihodi <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Prihodi) +
  geom_polygon(data = drzave.prihodi %>% right_join(zemljevid, by = "ADMIN")) +
  xlab("") +
  ylab("") +
  ggtitle("Število turistov iz Evropskih držav, \nki so prišli v Slovenijo") +
  coord_fixed(ratio = 2) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral")

zemljevid.prihodi

zemljevid.prenocitve <- ggplot() +
  aes(x = long, y = lat, group = group, fill = Prenocitve) +
  geom_polygon(data = drzave.prenocitve %>% right_join(zemljevid, by = "ADMIN")) +
  xlab("") +
  ylab("") +
  ggtitle("Število turistov iz Evropskih držav, \nki so prenočili v Sloveniji") +
  coord_fixed(ratio = 2) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral")

zemljevid.prenocitve


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
  geom_polygon(data = drzave.prihodi %>% right_join(zemljevid_svet, by = "ADMIN")) +
  xlab("") +
  ylab("") +
  ggtitle("Število turistov, ki so prišli v Slovenijo") +
  coord_fixed(ratio = 2) +
  theme_bw() +
  labs(caption = "Podatki so pridobljeni kot povprečje med leti 2018 in 2022") +
  theme(plot.caption.position = "plot") +
  scale_fill_distiller(palette = "Spectral")

zemljevid.svet.prihodi


