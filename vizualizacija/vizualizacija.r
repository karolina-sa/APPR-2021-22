# 3. faza: Vizualizacija podatkov

source("lib/libraries.r", encoding="UTF-8")

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
  scale_fill_manual(values=c('dodgerblue4','lightskyblue'))

prenocitev.regije.graf

# ==============================================================================

# nastanitvena doba po regijah

nastanitvena.doba.regije.graf <- ggplot(nastanitvena.doba.regije) +
  aes(x = Mesec, y = StDni, color = Drzava) +
  geom_jitter() + 
  scale_color_manual(values = c('dodgerblue4','lightskyblue')) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  facet_wrap(.~ Regija) +
  xlab("Mesec") +
  ylab("Nastanitvena doba (v dneh)") +
  ggtitle("Nastanitvena doba v dneh po regijah v določenem mesecu") +
  guides(fill=guide_legend(title = "Tip turista")) 

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
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Pomembnost izbranih motivov tujim turistom za prihod v Slovenijo") +
  xlab("") +
  ylab("")
  
motivi.prihoda.graf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

motivi.prihoda.pomembnost.graf <- ggplot(motivi.prihoda[motivi.prihoda$Drzava != "Skupaj" &
                                                          motivi.prihoda$Presoja == "Pomembnost",]) +
  aes(x = Drzava, y = StGlasov, fill = Drzava) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#CEE2FF", "#B3D1F8", "#8AB6E9", "#4890D1", "#005F9C")) +
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
  guides(fill=guide_legend(title = "Država opazovanja"))


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
  ggtitle("Pomembnost igralništva v Avstriji in Italiji")

igralnistvo.italija.avstrija.graf

# ==============================================================================

nastanitveni.obrat.regije.graf <- ggplot(nastanitveni.obrat.regije[nastanitveni.obrat.regije$Regija != "SLOVENIJA",]) +
  aes(x = Mesec, fill = Tip) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette="BuPu") +
  facet_wrap(.~ Regija) + 
  ylim(0,6.5) +
  ggtitle("Obiskanost regij glede na nastanitveni obrat po regijah") +
  guides(fill=guide_legend(title = "Tip nastanitve")) +
  theme(panel.background = element_blank())

nastanitveni.obrat.regije.graf

# ==============================================================================

# 
# # odhod slovencev v tujino
# 
# odhod.slovencev.graf <- ggplot(odhod.slovencev.v.tujino.nocitve) + 
#   aes(x = Leto, y = Število, color = Država, shape = Vrsta) + 
#   geom_point() + 
#   ggtitle("Obisk tujih držav s strani slovencev")
# 
# odhod.slovencev.graf 
# 
# # ==============================================================================
# 
# # izdatki za turizem
# 
# izdatki.graf <- ggplot(data=IZDATKI, aes(x = Leto, y = Stevilo, color = Vrsta)) +
#   geom_point() +
#   geom_line(aes(group = Vrsta)) +
#   ggtitle("Izdatki za turizem")
# 
# izdatki.graf
# 
# # sestava potrošnje tujcev v Sloveniji
# 
# sestava.izdatkov.graf1 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI, 
#                                 aes(x = Leto, y = Izdatek, color = Storitve)) +
#   geom_point() + 
#   geom_line() + 
#   ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")
# 
# sestava.izdatkov.graf1
# 
# # sestava potrošnje z boxplot
# 
# sestava.izdatkov.graf2 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
#                                  aes(x = Storitve, y = Izdatek)) +
#   geom_boxplot(fill="steelblue2", alpha=0.2) +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")
# 
# sestava.izdatkov.graf2 
# 
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
# 
# # uvoz, priprava zemljevida:
# 
# source("lib/uvozi.zemljevid.r", encoding="UTF-8")
# 
# zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
#                              "SVN_adm1", encoding="UTF-8") %>% fortify()
# 
# # za ujemanje preimenujemo stolpec
# colnames(zemljevid)[12]<- "Regija"
# 
# # moje regije: Gorenjska, Goriška, Jugovzhodna Slovenija, Koroška, Obalno-kraška, 
# #   Osrednjeslovenska, Podravska, Pomurska, Posavska, Primorska-notranjska, Savinjska
# 
# # regije v zemljevidu: Gorenjska, Goriška, Jugovzhodna Slovenija, Koroška, Obalno-kraška, 
# #   Osrednjeslovenska, Podravska, Pomurska, Spodnjeposavska, Notranjsko-kraška, Savinjska
# 
# # popravilo razlike v poimenovanju:
# zemljevid$Regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', zemljevid$Regija)
# zemljevid$Regija <- gsub('Spodnjeposavska', 'Posavska', zemljevid$Regija)
# 
# # ==============================================================================
# 
# # predstavitev števila prenočitev po regijah
# 
# # 'popravilo' tabele:
# prenocitve.regije.tuji.letno <- prenocitve.regije[prenocitve.regije$Drzava == "Tuji"
#                                                   & prenocitve.regije$Leto == "2019-12",]
# 
# prenocitve.zemljevid <- ggplot() +
#   geom_polygon(data = right_join(prenocitve.regije.tuji.letno, zemljevid, by = "Regija"),
#                aes(x = long, y = lat, group = group, fill = Prenocitve))+
#   ggtitle("Povprečno število prenočitev po regijah") + 
#   theme(axis.title=element_blank(), axis.text=element_blank(), 
#         axis.ticks=element_blank(), panel.background = element_blank(),
#         plot.title = element_text(hjust = 0.5)) +
#   scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
#   labs(fill="Prenocitve") +
#   geom_path(data = right_join(prenocitve.regije.tuji.letno, zemljevid,
#                               by = "Regija"), 
#             aes(x = long, y = lat, group = group), 
#             color = "white", size = 0.1)
# 
# prenocitve.zemljevid
# 
# # ==============================================================================
# 
