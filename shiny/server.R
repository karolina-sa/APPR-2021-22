library(shiny)

# uvoz, priprava zemljevida:

source("lib/uvozi.zemljevid.r", encoding="UTF-8")

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


shinyServer(
  function(input, output)
  {
    output$graf <- renderPlot({
      narisi.zemljevid(input$drzava, input$mesec)
    })
  }
)

narisi.zemljevid <- function(drzava, mesec)
{
  list <- as.list(strsplit(as.character(mesec), '-')[[1]])
  mesec.popravljen <- paste(list[1], "-", list[2], sep="")
  
  # 'popravilo' tabele:
  prenocitve.regije.letno <- 
    prenocitve.regije[prenocitve.regije$Drzava == drzava
                      & prenocitve.regije$Leto == mesec.popravljen, ]
  
  prenocitve.zemljevid <- ggplot() +
    geom_polygon(data = right_join(prenocitve.regije.letno, zemljevid, by = "Regija"),
                 aes(x = long, y = lat, group = group, fill = Prenocitve))+
    theme(axis.title=element_blank(), axis.text=element_blank(), 
          axis.ticks=element_blank(), panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    labs(fill="Prenočitve") +
    geom_path(data = right_join(prenocitve.regije.letno, zemljevid,
                                by = "Regija"), 
              aes(x = long, y = lat, group = group), 
              color = "white", size = 0.1)
  
  print(prenocitve.zemljevid)
  
}