# Analiza podatkov s programom R - 2021/22

## Tematika & priprava projekta

V zadnjih dveh letih je bilo veliko govora o turizmu, predvsem v povezavi s turističnimi boni in posledicami epidemije. Velikokrat sem se vprašala, koliko slovencev sploh obiskuje slovenske turistične obrate, ali raje gredo v tujino ali ostanejo doma in ali se je res tako drastično poslabšalo stanje v turizmu, da potrebujemo turistične bone. Prav tako me je zanimalo katere regije so najbolj obiskane s strani tujcev in katere s strani Slovencev. Slednja vprašanja so motivirala izibro teme projektne naloge.

Projektne naloge sem se lotila z zbiranjem podatkov. Večino podatkov sem dobila na strani [Statističnega urada Republike Slovenije](https://pxweb.stat.si/SiStat/sl), nakateri pa so tudi s strani [I FEEL SLOVENIJA](https://www.slovenia.info/en), [The World Bank](https://data.worldbank.org/indicator) in [Wikipedie](https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area). Če v poročilu vir podatkov ni izrecno naveden, so podatki iz [Statističnega urada Republike Slovenije](https://pxweb.stat.si/SiStat/sl). Pridobljeni podatki so treh oblik: `.csv`, `.xlsx` in `.html`.

Po tem ko sem pridobila podatke sem se lotila uvoza. Uvozila sem 22 različnih podatkov, nato sem jih uredila in naprej združevala v tabele in pri nekaterih tudi računala povprečja, vsote in podobno. Tako sem v grobem dobila 11 različnih tabel. Vsebina dobljenih tabel:

* `turizem.evropa` vsebuje podatke Evropskih držav. Za vsako državo imamo podatek o BDP ter številu turistov, ki so državo obiskali glede na njeno površino,
* `prenocitve.regije` zajema mesečne podatke o prenočitvah v posameznih regijah od leta 2019 do leta 2022. Podatke lahko ločimo glede na to ali veljajo za tujce ali Slovence, ki so prenočili v Sloveniji,
* `nastanitvena.doba.regije` prav tako vsebuje mesečne podatke, o dolžini nastanitvene dobe v posamezni regiji, ločeno za Slovence in tujce,
* `prenocitve.letno` prikazuje podatke o letnem številu prenočitev Slovencev po tujih državah od leta 2010 do leta 2020,
* `stevilo.zaposlenih` vsebuje podatke o letnem številu zaposlenih v različnih turističnih storitvah v letu 2012, 2014, 2015, 2017, 2019 in 2020,
* `BDP.turizem` zajema podatke o gibanju BDP v slovenskem turizmu v letih 2012, 2014, 2015, 2017, 2019 in 2020,
* `motivi.prihoda` obsega podatke o pomembnosti izbranih motivov za obisk Slovenije s strani tujcev,
* `izdatki` je tabela, ki jo sestavljajo letni podatki o izdatkih slovencev in tujcev za turizem v Sloveniji in tujini,
* `sestava.turisticne.potrosnje.tujcev.v.sloveniji` vsebuje letne podatke o izdatkih tujcev v Sloveniji za različne turistične storitve,
* `vec.let.skupaj` združuje podatke o letnem številu prihodov in prenočitev turistov iz posameznih držav med leti 2018 in 2020 ter
* `slovenci.prenocitve` obsega mesečne podatke o številu prenočitev Slovencev v Sloveniji med leti 2010 in 2021.

Sledila je faza vizualizacije in napredne analize, v katerih sem tabele, urejene v fazi uvoza, dodelala in jih še dodatno prilagodila, da sem lažje zrisala grafične prikaze. V fazi viuzalizacije sem se odločala med različnimi tipi grafičnih prikazov. Za posamezne podatke sem izbrala grafični prikaz, ki se mi je zdel najbolj primeren. Pri vizualizaciji sem naredila tudi nekaj zemljevidov, med drugim tudi zemljevid Slovenije s `shiny` aplikacijo. V fazi napredne analize sem naredila dva grafa linearne regresije, s pomočjo funkcije `geom_smooth`. Glavni del napredne analize, ki se v poročilu nahaja čisto na koncu, pa obsega napovedne modele in razvrščanje v skupine. V obeh sem izbirala med različnimi modeli in svoje odločitve podkrepila z različnimi grafičnimi prikazi in funkcijami.

Vzporedno s fazo vizualizacije in napredne analize sem sestavljala poročilo, v katerem sem ugotovitve, pridobljene predvsem iz grafičnih prikazov, nazorno opisala in ponekod dodala tudi svoje mnenje. 

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
