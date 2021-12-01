# Analiza podatkov s programom R - 2021/22


## Tematika

V projektu bom analizirala turizem v  Sloveniji: povprečno nastanitveno dobo, obiskanost nastanitvenih obratov, število prenočitev, število zaposlenih v turizmu, motive prihoda, izbiro turističnih destinacij v Sloveniji, uporabljena prevozna sredstva, potrošnjo turistov ter faktorje, ki vplivajo na odločitev potovanja. 

Vsi podatki so bili pridobljeni na [Statističnem uradu Republike Slovenije](https://pxweb.stat.si/SiStat/sl) in so bili v [Program R](https://www.r-project.org/) uvoženi v `.html` ali `.csv` obliki.

Analizo sem razdelila v šest sklopov, ki se med seboj povezujejo, vendar bodo predstavljeni v različnih tabelah:

1. **Mesečni pregled glede na regije** <br/>

Podatki po stolpcih: regija, mesec, nastanitvena_doba, nastanitveni_obrat, število_prenočitev, povprečni_mesečni_dohodek <br/>
Predvidena analiza: obiskanost posameznih regij glede na mesec/letni čas, v katere regije najraje zahajajo Slovenci in v katere tujci, kakšna je povprečna nastanitvena doba, kdo ima daljšo nastanitveno dobo - tujci ali Slovenci, nastanitvena doba glede na oddaljenost tuje države, kateri so najbolj popularni nastanitveni obrati, pogledamo lahko ali ima število turistov vpliv na dohodek zaposlenih.

2. **Večletni pregled za celotno državo** <br/>

Podatki po stolpcih: leto, število_prenočitev, število_zaposlenih <br/>
Predvidena analiza: število zaposlenih v odvisnosti od števila prenočitev. Prav tako lahko analiziramo drastični upad povpraševanja po turističnih dejavnosti v letu 2020, zaradi epidemije. Opazujemo lahko povpraševanje Slovencev po slovenskem turizmu, glede na pojav turističnih bonov in subvencij.

3. **Razlogi za prihod tujcev v Slovenijo ter uporabljeno prevozno sredstvo** <br/>

Podatki po stolpcih: država, motvi_prihoda, prevozno_sredstvo <br/>
Predvidena analiza: najpomembnejši razlog obiska Slovenije, najpogostejše prevozno sredstvo - glede na oddaljenost tuje države

4. **Države, ki jih obiskujejo Slovenci** <br/>

Podatki po stolpcih: država, število_slovenskih_turistov <br/>
Predvidena analiza: katere so najbolj obiskane države s strani Slovencev

5. **Izdatki tujcev ter Slovencev za turizem** <br/>

5.1. Izdatki tujcev ter Slovencev za turizem <br/>
Podatki po stolpcih: leto, potrošnja_tujcev_v_Slo, potrošnja_slovencev_v_Slo, potrošnja_slovencev_v_tujini <br/>
Predvidena analiza: opazujemo lahko koliko Slovenci 'cenijo' slovenski turizem; kako se razlikujejo potrošnje med seboj, kako so se spreminjale v preteklih letih

5.2. Sestava potrošnja tujcev v Sloveniji <br/>
Podatki po stolpcih: leto, sestava_potrošnje <br/>
Predvidena analiza: na čem največ potrošijo tuji turisti v Sloveniji

6. **Kaj vpliva na odločitve potovanja** <br/>

Podatki po stolpcih: dejavnik, leto, šli_na_potovanje, niso_šli_na_potovanje, šli_poslovno <br/>
Predvidena analiza: glede na starost, izobrazbo ter velikost gospodinjstva lahko primerjamo kdo si privošči več potovanj ter kdo največ potuje poslovno. 


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
