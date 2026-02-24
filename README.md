# EA FC 26 Player Rating Prediction --- Data Science Project

## Opis projekta

Ovaj projekat predstavlja primenu metoda nauke o podacima i mašinskog
učenja nad skupom podataka o fudbalerima iz igre EA FC 26. Cilj projekta
je analiza karakteristika igrača i predviđanje njihove ukupne ocene
(Overall rating --- OVR) na osnovu atributa performansi.

Projekat je realizovan u okviru kursa Uvod u nauku o podacima na
Prirodno-matematičkom fakultetu.

------------------------------------------------------------------------

## Cilj projekta

Glavni ciljevi rada su:

-   analiza strukture i karakteristika skupa podataka
-   eksplorativna analiza podataka (EDA)
-   čišćenje i priprema podataka
-   izdvajanje relevantnih prediktora
-   treniranje i evaluacija modela mašinskog učenja
-   poređenje performansi različitih modela
-   predviđanje Overall ocene igrača

------------------------------------------------------------------------

## Skup podataka

Dataset sadrži informacije o fudbalerima, uključujući:

-   demografske karakteristike (Age, Height, Weight)
-   tehničke atribute (Finishing, Passing, Dribbling, Vision i dr.)
-   tip kartice igrača (CardType)
-   poziciju igrača
-   fizičke i mentalne karakteristike
-   ukupnu ocenu igrača (OVR) kao ciljnu promenljivu

------------------------------------------------------------------------

## Podela podataka

Podaci su podeljeni u dva podskupa:

-   napadači (ST, RW, LW)
-   vezni igrači

Razlog podele je različit uticaj atributa na performanse igrača u
zavisnosti od njihove pozicije, što omogućava preciznije modeliranje i
interpretaciju rezultata.

------------------------------------------------------------------------

## Exploratory Data Analysis (EDA)

U okviru eksplorativne analize izvršeno je:

-   univarijantna analiza distribucije atributa
-   bivarijantna analiza u odnosu na ciljnu promenljivu
-   multivarijantna analiza
-   korelaciona analiza numeričkih promenljivih
-   analiza nedostajućih vrednosti
-   detekcija ekstremnih vrednosti
-   transformacija kategorijskih promenljivih

Cilj EDA faze bio je razumevanje strukture podataka i identifikacija
najvažnijih prediktora.

------------------------------------------------------------------------

## Priprema podataka

Tokom pripreme podataka izvršene su sledeće transformacije:

-   uklanjanje nepotrebnih kolona (url, card)
-   obrada nedostajućih vrednosti
-   uklanjanje atributa specifičnih za golmane
-   standardizacija visine i težine
-   konverzija kategorijskih promenljivih u factor tip
-   filtriranje nelogičnih i ekstremnih vrednosti

------------------------------------------------------------------------

## Modeli mašinskog učenja

Testirani su sledeći modeli:

-   linearna regresija
-   random forest
-   XGBoost
-   LASSO regresija

Modeli su trenirani i evaluirani odvojeno nad skupom napadača i skupom
veznih igrača.

------------------------------------------------------------------------

## Evaluacione metrike

Performanse modela procenjivane su pomoću sledećih metrika:

-   RMSE (Root Mean Squared Error)
-   MAE (Mean Absolute Error)
-   R² (koeficijent determinacije)

------------------------------------------------------------------------

## Rezultati

Na osnovu poređenja performansi modela, random forest model je pokazao
najbolje rezultate za oba skupa podataka. Model je ostvario najmanju
grešku predikcije i najveću vrednost koeficijenta determinacije, što
ukazuje na najbolje prepoznavanje odnosa između atributa igrača i
njihove ukupne ocene.

------------------------------------------------------------------------

## Tehnologije

Projekat je realizovan korišćenjem sledećih tehnologija i biblioteka:

-   R
-   RStudio
-   ggplot2
-   dplyr
-   ranger (Random Forest)
-   xgboost
-   glmnet (LASSO regresija)
-   ggcorrplot

------------------------------------------------------------------------

## Pokretanje projekta

### Instalacija biblioteka

Pre prvog pokretanja potrebno je instalirati potrebne biblioteke:

``` r
install.packages(c(
  "tidyverse",
  "ggplot2",
  "dplyr",
  "ranger",
  "xgboost",
  "glmnet",
  "ggcorrplot"
))
```

### Pokretanje skripte

Projekat se pokreće izvršavanjem glavne skripte:

``` r
source("Seminarski.R")
```

Skripta izvršava:

-   učitavanje podataka
-   pripremu i čišćenje podataka
-   eksplorativnu analizu
-   treniranje modela
-   evaluaciju modela
-   prikaz rezultata

------------------------------------------------------------------------

## Struktura repozitorijuma

data/ EAFC26.csv

scripts/ Seminarski.R

plots/ (generisani grafici)

README.md

------------------------------------------------------------------------
## Metodologija

Projekat prati standardni proces nauke o podacima:

1.  razumevanje podataka
2.  čišćenje podataka
3.  eksplorativna analiza
4.  feature engineering
5.  modelovanje
6.  evaluacija modela
7.  interpretacija rezultata

------------------------------------------------------------------------

## Produkcija i testiranje modela

Model testiramo tako što pokrenemo skriptu produkcija.r
``` r
source("produkcija.R")
```

Nakon toga, u konzoli ukucamo komandu: 
```
pr("produkcija.R") %>% pr_run(port=8000)
```

Nakon pokretanja date komande, otvara se Swagger meni, gde se mogu testirati najbolji model za napadača i vezne igrače, unošenjem adekvatnih vrednosti.
Model Napadači:
Numeričke vrednosti :
1. Finishing
2. Ball.Control
3. Volleys
4. Reactions
5. Dribbling
6. Stamina
7. Shot.Power
Kategorijska promenljiva:
AgeGroup ("Young", "Experienced", "Veteran")

Model Veznjaci:
Numeričke vrednosti :
1. Vision
2. Short.Passing
3. Ball.Control
4. Composure
5. Reactions
6. Long.Passing
7. Dribbling
8. Stamina
Kategorijska promenljiva:
AgeGroup ("Young", "Experienced", "Veteran")
------------------------------------------------------------------------
## Autori

Aleksa Đorđević 51-2022\
Nikola Živadinović 55-2022\
Bogdan Jevtić 59-2022
