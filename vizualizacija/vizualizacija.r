# 3. faza: Vizualizacija podatkov

source("C:/Users/Uporabnik/OneDrive/Namizje/SOLA/2.letnik/ANALIZA PODATKOV S PROGRAMOM R/APPR-2021-22/lib/libraries.r")

# ==============================================================================
# GRAFIČNA ANALIZA
# ==============================================================================

# odhod slovencev v tujino
# Število potovanj slovencev po letih. Vsaka država svoja barva;
#                                      oblika glede na zasebno ali poslovno potovanje
odhod.slovencev.graf <- ggplot(odhod.slovencev.v.tujino.nocitve) + 
  aes(x = Leto, y = Število, color = Država, shape = Vrsta) + 
  geom_point() + 
  ggtitle("Obisk tujih držav s strani slovencev")

odhod.slovencev.graf 

# ==============================================================================

# izdatki za turizem
# Izdatki slovencev in tujcev za turizem v sloveniji (in tujini) po letih.
#               Trije različno obarvani geom_line grafi

izdatki.graf <- ggplot(data=IZDATKI, aes(x = Leto, y = Stevilo, color = Vrsta)) +
  geom_point() +
  geom_line(aes(group = Vrsta)) +
  ggtitle("Izdatki za turizem")

izdatki.graf

# sestava potrošnje tujcev v Sloveniji

sestava.izdatkov.graf1 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI, 
                                aes(x = Leto, y = Izdatek, color = Storitve)) +
  geom_point() + 
  geom_line() + 
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")

sestava.izdatkov.graf1

# sestava potrošnje z boxplot

sestava.izdatkov.graf2 <- ggplot(data=SESTAVA.TURISTICNE.POTROSNJE.TUJCEV.V.SLOVENIJI,
                                 aes(x = Storitve, y = Izdatek)) +
  geom_boxplot(fill="steelblue2", alpha=0.2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Sestava izdatkov tujcev za turizem v Sloveniji")

sestava.izdatkov.graf2 

# ==============================================================================

# Število prenočitev v Sloveniji po letih

stevilo.prenocitev.letno.graf <- ggplot(data = stevilo.prenocitev.letno,
                                        mapping = aes(x = Leto, y = Stevilo)) +
  geom_bar(stat = "identity",
           fill = "gray70") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  ylab("Število prenočitev") +
  ggtitle("Število prenočitev v Sloveniji po letih") 

stevilo.prenocitev.letno.graf +
  stevilo.prenocitev.letno %>%
    filter(Leto == 2020) %>%
  geom_bar(
    mapping = aes(x = Leto, y = Stevilo),
    stat = "identity",
    fill="steelblue2"
  )


  
# ==============================================================================
# PROSTORSKA ANALIZA (ZEMLJEVIDI)
# ==============================================================================

# uvoz, priprava zemljevida:

source("C:/Users/Uporabnik/OneDrive/Namizje/SOLA/2.letnik/ANALIZA PODATKOV S PROGRAMOM R/APPR-2021-22/lib/uvozi.zemljevid.r", encoding="UTF-8")

zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", encoding="UTF-8") %>% fortify()

# za ujemanje preimenujemo stolpec
colnames(zemljevid)[12]<- "Regija"

# moje regije: Gorenjska, Goriška, Jugovzhodna Slovenija, Koroška, Obalno-kraška, 
#   Osrednjeslovenska, Podravska, Pomurska, Posavska, Primorska-notranjska, Savinjska

# regije v zemljevidu: Gorenjska, Goriška, Jugovzhodna Slovenija, Koroška, Obalno-kraška, 
#   Osrednjeslovenska, Podravska, Pomurska, Spodnjeposavska, Notranjsko-kraška, Savinjska

# popravilo razlike v poimenovanju:
zemljevid$Regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', zemljevid$Regija)
zemljevid$Regija <- gsub('Spodnjeposavska', 'Posavska', zemljevid$Regija)

# ==============================================================================

# predstavitev števila prenočitev po regijah

# 'popravilo' tabele:
prenocitve.regije.tuji.letno <- prenocitve.regije[prenocitve.regije$Drzava == "Tuji",] %>%
  select(Regija, Stevilo) %>%
  group_by(Regija) %>%
  summarise("Prenocitve" = mean(Stevilo))

prenocitve.zemljevid <- ggplot() +
  geom_polygon(data = right_join(prenocitve.regije.tuji.letno, zemljevid, by = "Regija"),
               aes(x = long, y = lat, group = group, fill = Prenocitve))+
  ggtitle("Povprečno število prenočitev po regijah") + 
  theme(axis.title=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  labs(fill="Prenocitve") +
  geom_path(data = right_join(prenocitve.regije.tuji.letno, zemljevid,
                              by = "Regija"), aes(x = long, y = lat, 
                                                  group = group), 
            color = "white", size = 0.1)

prenocitve.zemljevid

# ==============================================================================

