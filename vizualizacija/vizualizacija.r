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
  guides(fill=guide_legend(title = "Tip turista")) +
  theme_bw()

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
  theme_bw()

nastanitveni.obrat.regije.graf

# ==============================================================================

# odhod slovencev v tujino

# NI V REDU GRAF!!!!!!

prenocitve.po.drzavah.letno.graf <- ggplot(prenocitve.po.drzavah.letno) +
  aes(x = Leto, y = Stevilo, color = DRŽAVA) +
  geom_point()

prenocitve.po.drzavah.letno.graf

# ==============================================================================

# stevilo zaposlenih v turizmu

stevilo.zaposlenih.graf <- ggplot(stevilo.zaposlenih, 
                                  mapping = aes(x = Leto, y = Stevilo, fill = Storitve)) +
  geom_col() +
  ggtitle("Število zaposlenih v Sloveniji v turizmu (v tisočih)") +
  guides(fill=guide_legend(title = "Tip zaposlitve v turizmu"))
  
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
  scale_color_hue(h = c(180, 270)) +
  theme_bw() +
  ggtitle("Izdatki za turizem")

izdatki.graf

# sestava potrošnje tujcev v Sloveniji

sestava.izdatkov.graf1 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
                                aes(x = Leto, y = Izdatek, color = Storitve)) +
  geom_point() +
  geom_line(size=1) +
  scale_color_hue(h = c(150, 300)) +
  theme_bw() + 
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")

sestava.izdatkov.graf1

# sestava potrošnje z boxplot

sestava.izdatkov.graf2 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
                                 aes(x = Storitve, y = Izdatek)) +
  geom_boxplot(fill="grey90", notch = FALSE) +
  stat_summary(fun=mean, geom="point", shape=15, size=2, color="red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji") +
  theme_bw() +
  labs(caption = "Pojasnilo: Rdeči kvadatki predstavljajo povprečje posamene vrste izdatkov.")

sestava.izdatkov.graf2

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

names(vec.let.samo.prave.drzave)[1] <- "region"
drzave.prihodi <- vec.let.samo.prave.drzave[vec.let.samo.prave.drzave$Kaj == "Prihodi",] %>%
  group_by(region) %>%
  summarize(Prihodi = mean(Število, na.rm = TRUE))
  
drzave.prenocitve <- vec.let.samo.prave.drzave[vec.let.samo.prave.drzave$Kaj == "Prenočitve",] %>%
  group_by(region) %>%
  summarize(Prenocitve = mean(Število, na.rm = TRUE))

world_map <- subset(map_data("world"), region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                             "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                             "Croatia", "Cyprus", "Czechia","Denmark","Estonia","Finland", 
                                             "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                             "Ireland", "Italy","Kazakhstan", "Kosovo", "Latvia","Liechtenstein", 
                                             "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                             "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                             "San Marino","Serbia","Slovakia","Slovenia","Spain",
                                             "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican", "Russia"))

# zemljevida: 

drzave.prihodi_map <- left_join(world_map, drzave.prihodi,
                        by = "region")
zemljevid.prihodi <- ggplot(drzave.prihodi_map,
                            aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Prihodi), color = "black") + 
  scale_fill_gradient(low = "gray86", high = "black") +
  theme_void() +
  coord_fixed(ratio=1.5, xlim = c(-15,180), ylim = c(35,80)) +
  ggtitle("Število turistov iz Evropskih držav, ki so prišli v Slovenijo \n(povprečje od leta 2018 do 2022)")

zemljevid.prihodi

# ===

drzave.prenocitve_map <- left_join(world_map, drzave.prenocitve,
                                by = "region")
zemljevid.prenocitve <- ggplot(drzave.prenocitve_map,
                            aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Prenocitve), color = "black") + 
  scale_fill_gradient(low = "gray86", high = "black") +
  theme_void() +
  coord_fixed(ratio=1.5, xlim = c(-15,180), ylim = c(35,80)) +
  ggtitle("Število turistov iz Evropskih držav, ki so prenočili v Sloveniji \n(povprečje od leta 2018 do 2022)") 

zemljevid.prenocitve









