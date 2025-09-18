# POBIERANIE DANYCH METEOROLOGICZNYCH Z IMGW I OGIMETU (SYNOP, SONDOWANIA ATMOSFERY)
# INSTALACJA PAKIET?W "IMGW" I "CLIMATE"

# w pierwszej kolejności ustawiamy katalog roboczy

setwd("/Users/marcin/Documents/Projects/uam-geo/climatology/lab4/data")

getwd() # sprawdzenie katalogu roboczego

dir()

# plot(b)
# wczytujemy/instalujemy potrzebne biblioteki


library(dplyr)
library(RCurl)
library(XML)
library(remotes)
library(climate)






# install.packages('imgw') # instalacja bezpo?rednio z repozytorium CRAN

# install_github("bczernecki/climate")# instalacja bezpo?rednio z repozytorium CRAN

library(climate)

imgw_meteo_abbrev
imgw_meteo_stations


###  PRZYKłADY UżYCIA POLECEń Z BIBLIOTEK CLIMATE

# Pobieramy metadane o stacjach w Polsce


polska <- stations_ogimet(country = "Poland", add_map = T)

head(polska)
tail(polska)
str(polska)
dim(polska)

polska[1, 3]


wysokosc <- polska[, ]
c(1, 2)
polska[c(1:10), c(1:2, 5)] # indeksowanie ramki danych (data frame)



df
nr_stacji



nowy <- polska[c(1, 5), c(1, 5)]

str(polska)

head(polska, 10)
tail(polska)

View(polska)

polska$alt

length(polska)
str(dane)


stacje <- polska$station_names
stacje



polska[1:31, c(1, 4:5)]


str(polska) # sprawdzenie struktury obiektu

head(polska)
tail(polska)

View(polska)

# wy?wietlamy 6 pierwszych wierszy utworzonego obiektu polska

head(polska)
tail(polska)

# sprawdzamy struktur? utworzonego obiektu polska

str(polska)

# wy?wietlamy wszystkie numery stacji

polska$wmo_id

# wy?wietlamy wszystkie nazwy stacji

polska$station_names

polska[c(1:10, 20:30), c(1:2, 5)]

# pobieramy średnie miesięczne wartości elementów meteo.
# za lata 1966-2022
# dla wszystkich stacji w Polsce

dane_mc <- meteo_imgw(
    interval = "monthly", rank = "synop", year = c(1966:2022),
    coords = T
)
?meteo_imgw
head(dane_mc)

colnames(dane_mc)

head(dane_mc)
# dane_mc <- meteo_imgw(interval="daily", rank="synop", year= c(2024),
#                        coords=F)

# dane_mc = meteo_ogimet(date = c("2024-10-03", "2024-07-09"),
#                  interval = "daily",
#                  coords = FALSE,
#                  station = 12330)
#
# dane_mc <- dane_mc[order(dane_mc$Date),]

head(dane_mc)
str(dane_mc)

imgw_meteo_abbrev

colnames(dane_mc)
str(dane_mc)

plot(dane_mc$t2m_mean_mon)

boxplot(dane_mc$t2m_mean_mon)

unique(dane_mc$station)

imgw_meteo_abbrev

head(dane_mc)
str(dane_mc)

colnames(dane_mc)

dane_mc$station

unique(dane_mc$station)

head(dane_mc)
dane_mc[, 8]

# przed zapisaniem ich na dysku lokalnym sprawdzamy wielko?? pliku

object.size(dane_mc)

library(pryr)

object_size(dane_mc)

# zapisujemy pobrane dane na dysku lokalnym

dane_mc <- dane_mc[, -39]

saveRDS(dane_mc, "dane_mc_polska.rds")

dir()

# w taki spos?b mo?emy go wczyta?


mmp <- readRDS("dane_mc_polska.rds") # mean month poland (nadajmy obiektowi now? nazw?)

head(mmp)
colnames(mmp)


unique(mmp$rr_monthly)
# sprawdzamy ile jest lat w obiekcie

unique(mmp$yy)

# wykonajmy przyk?adowe ryciny

colnames(mmp)

boxplot(mmp$tmax_abs) # w tej formie przydatna jedynie do sprawdzenia
hist(mmp$t2m_mean_mon,
    breaks = 50,
    main = "Histogram temperatury powietrza",
    xlab = "Temperatura powietrza [°C]",
    ylab = "Liczba dni"
)

# zakresu danych bo mamy tutaj miesi?ce z lat 1966-2018
# i wszystkie stacje
unique(mmp$station) # czyli 72




unique(mmp[, c("station", "X", "Y")])


head(mmp)
t(polska)

library(dplyr)

colnames(mmp)


# poprawiamy od razu podwójne nazwy wszystkich stacji
# LUB DOPISUJEMY X Y JEżELI BRAKUJE

mmp$station[mmp$station == "POZNAŃ-ŁAWICA"] <- "POZNAŃ"

mmp$station[mmp$station == "KOŁOBRZEG-DŹWIRZYNO"] <- "KOŁOBRZEG"
mmp$station[mmp$station == "WROCŁAW-STRACHOWICE"] <- "WROCŁAW"
mmp$station[mmp$station == "WARSZAWA-OKĘCIE"] <- "WARSZAWA"
mmp$station[mmp$station == "ŁÓDŹ-LUBLINEK"] <- "ŁÓDŹ"
mmp$station[mmp$station == "KATOWICE-MUCHOWIEC"] <- "KATOWICE"

polska
mmp$X[mmp$station == "BYDGOSZCZ"] <- 18.006
mmp$Y[mmp$station == "BYDGOSZCZ"] <- 53.106

mmp$X[mmp$station == "HALA GĄSIENICOWA"] <- 20.0057
mmp$Y[mmp$station == "HALA GĄSIENICOWA"] <- 49.2441


# plot kontrolny
head(mmp)
plot(mmp$t2m_mean_mon)


# przefiltrujmy dane (mmp) tak aby została tylko stacja w Poznaniu,

unique(mmp$station)
colnames(mmp)

library(dplyr)

poznan_mc <- mmp %>% filter(station %in% c("POZNAŃ"))

head(poznan_mc)

plot(poznan_mc$yy, poznan_mc$t2m_mean_mon)

# sprawdzamy, czy polecenie dobrze zadziałało, można tak:

unique(poznan_mc$station)

unique(poznan_mc$yy)
head(poznan_mc)
str(poznan_mc)


# policzmy średnie miesięczne klimatologiczne (1966-2022)
# wartości temperatury powietrza w Poznaniu
head(poznan_mc)

poznan_avr_mc <- poznan_mc %>%
    group_by(mm) %>%
    summarise(t2 = mean(t2m_mean_mon))

poznan_avr_mc

# można też od razu policzyć oprócz średniej maksymalną i minimalną miesiączną
head(poznan_mc)

poznan_avr_mc <- poznan_mc %>%
    group_by(mm) %>%
    summarise(
        t2avr = mean(t2m_mean_mon),
        t2max = max(t2m_mean_mon),
        t2min = min(t2m_mean_mon),
        t2sd = sd(t2m_mean_mon)
    )

polska_avr_mc <- mmp %>%
    group_by(mm) %>%
    summarise(
        t2avr = mean(t2m_mean_mon),
        t2max = max(t2m_mean_mon),
        t2min = min(t2m_mean_mon),
        t2sd = sd(t2m_mean_mon)
    ) 

poznan_avr_mc_opad <- poznan_mc %>%
    group_by(mm) %>%
    summarise(
        p2avr = mean(rr_monthly),
        p2max = max(rr_monthly),
        p2min = min(rr_monthly),
        p2sd = sd(rr_monthly)
    )

polska_avr_mc_opad <- mmp %>%
    group_by(mm) %>%
    summarise(
        p2avr = mean(rr_monthly),
        p2max = max(rr_monthly),
        p2min = min(rr_monthly),
        p2sd = sd(rr_monthly)
    )

poznan_avr_mc
polska_avr_mc

# conflict_prefer("select", "dplyr")

poznan_avr_mc

## TWORZYMY TABELĘ GOTOWĄ DO PUBLIKACJI/RAPORTU
round(poznan_avr_mc, 1)
round(polska_avr_mc, 1)

install.packages("kableExtra", dependencies = T)
library(kableExtra)

round(poznan_avr_mc, 1) %>%
    kableExtra::kable("html",
        row.names = T,
        col.names = c("mc", "średnia", "maksimum", "minimum", "sd"),
        align = "c", caption = "Tabela 1. średnia, maksymalna, minimalna oraz odchylenie
        standardowe średniej miesięcznej temperatury powietrza w latach 1966-2018 w Poznaniu"
    ) %>%
    kableExtra::column_spec(column = 2:6, width = "4cm", color = "black") %>%
    kable_styling()

round(poznan_avr_mc_opad, 1) %>%
    kableExtra::kable("html",
        row.names = T,
        col.names = c("mc", "średnia", "maksimum", "minimum", "sd"),
        align = "c", caption = "Tabela 1. średnia, maksymalna, minimalna oraz odchylenie
        standardowe opadu miesięcznego w latach 1966-2018 w Poznaniu"
    ) %>%
    kableExtra::column_spec(column = 2:6, width = "4cm", color = "black") %>%
    kable_styling()

round(polska_avr_mc, 1) %>%
    kableExtra::kable("html",
        row.names = T,
        col.names = c("mc", "średnia", "maksimum", "minimum", "sd"),
        align = "c", caption = "Tabela 1. średnia, maksymalna, minimalna oraz odchylenie
        standardowe średniej miesięcznej temperatury powietrza w latach 1966-2018 w Polsce"
    ) %>%
    kableExtra::column_spec(column = 2:6, width = "4cm", color = "black") %>%
    kable_styling()

round(polska_avr_mc_opad, 1) %>%
    kableExtra::kable("html",
        row.names = T,
        col.names = c("mc", "średnia", "maksimum", "minimum", "sd"),
        align = "c", caption = "Tabela 1. średnia, maksymalna, minimalna oraz odchylenie
        standardowe opadu miesięcznego w latach 1966-2018 w Polsce"
    ) %>%
    kableExtra::column_spec(column = 2:6, width = "4cm", color = "black") %>%
    kable_styling()

head(poznan_avr_mc)

# plot ze średnimi klimatologicznymi wartościami dla miesięcy

plot(poznan_avr_mc$mm, poznan_avr_mc$t2avr,
    ylim = c(-10, 28), xlim = c(1, 12),
    type = "l", col = "black", lty = 1,
    xlab = "miesiąc",
    ylab = "Temperatura (\u00B0C)",
    main = "Poznań - średnia, maksymalna i minimalna
             temperatura powietrza"
)

lines(poznan_avr_mc$t2max, col = "red", lty = 2)
lines(poznan_avr_mc$t2min, col = "blue", lty = 3)
lines(poznan_avr_mc$t2sd, col = "green", lty = 4)

legend(3, -.5,
    legend = c("maksymalna", "średnia", "minimalna", "sd"), box.lty = 1,
    col = c("red", "black", "blue", "green"), lty = 1:4, cex = 1.2
)

grid(length(poznan_avr_mc$t2avr), 10)

# nie rysuje nam wszystkich etykiet i znacznik?w osi x, wi?c poprawmy:

plot(poznan_avr_mc$mm, poznan_avr_mc$t2avr,
    xaxt = "n", ylim = c(-10, 25), # xaxt - bez etykiet x
    type = "l", col = "black", lty = 1,
    xlab = "miesiąc",
    ylab = "Temperatura (\u00B0C)",
    main = "Poznań - średnia, maksymalna i minimalna
             temperatura powietrza"
)
# axis(side=1, at = seq(1,12,1), labels=month.abb, las=2)
axis(side = 1, at = seq(1, 12, 1), labels = seq(1, 12, 1), cex.axis = 0.8)
lines(poznan_avr_mc$t2max, col = "red", lty = 2)
lines(poznan_avr_mc$t2min, col = "blue", lty = 3)
lines(poznan_avr_mc$t2sd, col = "green", lty = 4)
legend(3, -1,
    legend = c("maksymalna", "średnia", "minimalna", "sd"), box.lty = 0,
    col = c("red", "black", "blue", "green"), lty = 1:4, cex = 0.7
)
grid(length(poznan_avr_mc$t2avr), 10)

plot(polska_avr_mc$mm, polska_avr_mc$t2avr,
    xaxt = "n", ylim = c(-10, 25), # xaxt - bez etykiet x
    type = "l", col = "black", lty = 1,
    xlab = "miesiąc",
    ylab = "Temperatura (\u00B0C)",
    main = "Polska - średnia, maksymalna i minimalna
             temperatura powietrza"
)
# axis(side=1, at = seq(1,12,1), labels=month.abb, las=2)
axis(side = 1, at = seq(1, 12, 1), labels = seq(1, 12, 1), cex.axis = 0.8)
lines(polska_avr_mc$t2max, col = "red", lty = 2)
lines(polska_avr_mc$t2min, col = "blue", lty = 3)
lines(polska_avr_mc$t2sd, col = "green", lty = 4)
legend(3, -1,
    legend = c("maksymalna", "średnia", "minimalna", "sd"), box.lty = 0,
    col = c("red", "black", "blue", "green"), lty = 1:4, cex = 0.7
)
grid(length(polska_avr_mc$t2avr), 10)

plot(poznan_avr_mc_opad$mm, poznan_avr_mc_opad$p2avr,
    xaxt = "n", ylim = c(0, 200), # xaxt - bez etykiet x
    type = "l", col = "black", lty = 1,
    xlab = "miesiąc",
    ylab = "Opad (mm)",
    main = "Poznań - średni, maksymalny i minimalny opad"
)
# axis(side=1, at = seq(1,12,1), labels=month.abb, las=2)
axis(side = 1, at = seq(1, 12, 1), labels = seq(1, 12, 1), cex.axis = 0.8)
lines(poznan_avr_mc_opad$p2max, col = "red", lty = 2)
lines(poznan_avr_mc_opad$p2min, col = "blue", lty = 3)
lines(poznan_avr_mc_opad$p2sd, col = "green", lty = 4)
legend(3, 150,
    legend = c("maksymalny", "średni", "minimalny", "sd"), box.lty = 0,
    col = c("red", "black", "blue", "green"), lty = 1:4, cex = 0.7
)
grid(length(poznan_avr_mc_opad$p2avr), 10)

plot(polska_avr_mc_opad$mm, polska_avr_mc_opad$p2avr,
    xaxt = "n", ylim = c(0, 700), # xaxt - bez etykiet x
    type = "l", col = "black", lty = 1,
    xlab = "miesiąc",
    ylab = "Opad (mm)",
    main = "Polska - średni, maksymalny i minimalny opad"
)
# axis(side=1, at = seq(1,12,1), labels=month.abb, las=2)
axis(side = 1, at = seq(1, 12, 1), labels = seq(1, 12, 1), cex.axis = 0.8)
lines(polska_avr_mc_opad$p2max, col = "red", lty = 2)
lines(polska_avr_mc_opad$p2min, col = "blue", lty = 3)
lines(polska_avr_mc_opad$p2sd, col = "green", lty = 4)
legend(3, 150,
    legend = c("maksymalny", "średni", "minimalny", "sd"), box.lty = 0,
    col = c("red", "black", "blue", "green"), lty = 1:4, cex = 0.7
)
grid(length(polska_avr_mc_opad$p2avr), 10)



dev.off()
# mo?na to lepiej pokaza? boxpotem
head(poznan_mc)

mmp

boxplot(poznan_mc$t2m_mean_mon ~ poznan_mc$mm,
    xlab = "Miesiące", xaxt = "n", las = 1,
    main = "Poznań - średnie miesięczne temperatury powietrza",
    ylab = expression("Temperatura (" * ~ degree * C * ")")
)


axis(side = 1, at = seq(1, 12, 1), labels = month.abb, las = 2)
grid(13)


par(mfrow = c(1, 2)) # podział obszaru ryciny na dwa




# mo?na r?wnie? podzieli? okno i narysowa? oba wykresy obok siebie

par(mfrow = c(1, 2))


graphics.off()

# Średnia roczna - trendy -------------------------------------------------

# policzmy średnie roczne wartości temperatury powietrza w Poznaniu
# (1966-2021)

head(poznan_mc)

poznan_rok <- poznan_mc %>%
    group_by(yy) %>%
    summarise(t2 = mean(t2m_mean_mon))

poznan_rok
View(poznan_rok)

# narysujmy wykres:

plot(
    poznan_rok$yy,
    poznan_rok$t2,
    type = "l",
    col  = "blue",
    xlab = "Rok",
    ylab = "Temperatura powietrza [°C]",
    main = "Poznań - średnia roczna temperatura powietrza
  (1966-2020)"
)

grid()
head(poznan_rok)
unique(poznan_mc$yy)

model <- lm(t2 ~ yy, data = poznan_rok)

model

summary(model)

abline(model, col = "red", lwd = 2, lty = 2)

legend("topleft",
    legend = paste0(
        "y = ", round(coef(model)[2], 3), "x  ",
        round(coef(model)[1], 2)
    ), bty = "n"
)


# sortowanie wektora temperatury w celu ustalenie 10 najcieplejszych lat

head(poznan_rok, 10)

attach(poznan_rok)

najcieplejsze <- poznan_rok[order(-t2), ] # wektor temperatury malej?co (od najwy?szych)
najcieplejsze

najzimniejsze <- poznan_rok[order(t2), ] # wektor temperatury rosn?co (od najni?szych)
najzimniejsze

detach(poznan_rok)

# mo?na te? tak:
poznan_rok[(order(poznan_rok$t2, decreasing = F)), ]
poznan_rok[(order(poznan_rok$t2, decreasing = T)), ]

library(kableExtra)
# tabela 2 - najzimniejsze i njcieplejsze 10 lat

naj <- data.frame(najcieplejsze[1:10, c(1, 2)], najzimniejsze[1:10, c(1, 2)])
naj

round(naj, 1) %>%
    kableExtra::kable("html",
        row.names = T,
        col.names = c("rok", "najwyższa", "rok", "najniższa"),
        align = "c", caption = "Tabela 2. Lata z najwyższą i najniższą temperaturą powietrza w Poznaniu"
    ) %>%
    kableExtra::column_spec(column = 1:5, width = "1cm", color = "black") %>%
    #  row_spec(c(1,4), bold = T, color = "white", background = "#D7261E", hline_after = T)%>%
    row_spec(c(0, 10), extra_css = "border-bottom: 1.2px solid")


# kable_styling(bootstrap_options = "basic", full_width = F)


# por?wnujemy nasz wynik z uzyskanym w artykule 'Homogenization of air temperature...' Kolendowicz et al. dost?pny (Research Gate)
# https://www.researchgate.net/publication/326501510_Homogenization_of_air_temperature_and_its_long-term_trends_in_Poznan_Poland_for_the_period_1848-2016

# POLECENIE: na podstawie poznanego kodu policzy? trendy miesi?czne dla sezon?w
#
#
conflict_prefer("stats", "dplyr")
# (ROZWI?ZANIE ZADANIA ) TRENDY DLA SEZON?W


head(poznan_mc)

# conflict_prefer('filter','dplyr')

head(poznan_mc)

svg("nazwa.svg", width = 8, height = 5, pointsize = 12)


wiosna <- poznan_mc %>%
    filter(mm %in% c(3, 4, 5)) %>%
    group_by(yy) %>%
    summarise(t2 = mean(t2m_mean_mon))

head(wiosna)

plot(wiosna$yy,
    wiosna$t2,
    type = "l",
    col  = "blue",
    xlab = "Rok",
    ylab = "Temperatura powietrza [°C]",
    main = "Poznań - średnia roczna temperatura powietrza (1966 - 2020)"
)


model <- lm(t2 ~ yy, data = wiosna)
model

summary(model)

abline(model, col = "red", lwd = 2, lty = 2)

legend("topleft",
    legend = paste0(
        "y = ", round(coef(model)[2], 3), "x  ",
        round(coef(model)[1], 2)
    ), bty = "n"
)



dev.off()


# lato --------------------------------------------------------------------

sqrt(.3882)

lato <- poznan_mc %>%
    filter(mm %in% c(6, 7, 8)) %>%
    group_by(yy) %>%
    summarise(t2 = mean(t2m_mean_mon))

jesien <- poznan_mc %>%
    filter(mm %in% c(9, 10, 11)) %>%
    group_by(yy) %>%
    summarise(t2 = mean(t2m_mean_mon))

zima <- poznan_mc %>%
    filter(mm %in% c(1, 2, 12)) %>%
    group_by(yy) %>%
    summarise(t2 = mean(t2m_mean_mon))

head(zima)


### UWAGA!!! liczy? zim? jednak trzeba nieco inaczej. Poni?ej prawid?owy zapis, gdzie
### grudzie? musi nale?e? do roku nast?pnego (grupuj?c stosujemy ifelse i dwa warunki...)
unique(poznan_mc$yy)

zima <- poznan_mc %>%
    filter(mm %in% c(1, 2, 12)) %>%
    group_by(yy = ifelse(mm == 12, yy + 1, yy)) %>%
    summarise(t2 = mean(t2m_mean_mon))


head(zima)
tail(zima)
str(zima)

zima <- zima[c(-1, -57), ] # opuszczamy 1 i 56 wiersz (zima wyliczona z niepe?nych danych)
plot(zima$yy, zima$t2)

# dodajemy cz??? z modelem liniowym

### ?WICZENIE: NA PODSTAWIE POWY?SZEGO KODU WYKONA? PODOBNE OBLICZENIA
### DLA CA?EJ POLSKI, TZW. ?REDNIA OBSZAROWA (WYKRES LINIOWY, BOXPLOT
### I MODEL TRENDU TEMPERATURY Z WSZYSTKICH
### STACJI JEDNOCZE?NIE). PROPONUJ? POWY?SZY KOD SKOPIOWA? DO NOWEJ ZAK?ADKI I TAM
### ODPOWIEDNIO ZMODYFIKOWA?

#################################
### MAPY

install.packages('colorRamps', dependencies = T)
install.packages('colorspace', dependencies = T)
install.packages('mapview', dependencies = T)

library(akima)
library(raster)
library(fields)
library(colorRamps)
library(colorspace)
library(mapview)

install.packages("geodata", dependencies = T)
library(geodata)
# KORZYSTAMY Z WCZE?NIEJ POBRANYCH DANYCH (mmp - ?rednie miesi?czne
# dla wszystkich stacji w Polsce)

head(mmp)
unique(mmp$station)

# RYSUJEMY MAPY DLA NAJCIEPLEJSZEGO I NAJCH?ODNIEJSZEGO MIESI?CA
# obliczamy ?redni? dla danego miesi?ca, dla ka?dej stacji
# Przydadz? si? r?wnie? koordynaty x, y
#

# POBIERAM GRANIC? POLSKI DO KT?REJ B?DZIEMY PRZYCINA? EKSTRAPOLOWAN?
# MAP?

granica <- gadm(country = "POL", level = 2, path = tempdir())
plot(granica)

# UWAGA: wykluczy?em Bydgoszcz i Gda?sk, mo?na te? bez ?NIE?KI I KASPROWEGO

head(mmp)
unique(mmp$station)

# wybieramy stacje z kompletem danych

stacje_l_mc <- mmp %>%
    group_by(station) %>%
    summarise(liczba_mc = n()) %>%
    dplyr::filter(liczba_mc > 400)

plot(stacje_l_mc$liczba_mc)
stacje_l_mc

stacje_l_mc[["station"]] # wektor nazw stacji do filtrowania
# wa?ne aby u?y? polecenia complete.cases
# (usuwanie ca?ych wierszy w razie braku jakiejkolwiek warto?ci)
dane2 <- data.frame(mmp[complete.cases(mmp$t2m_mean_mon), c(3:7, 12)])
dane2

# dla miesi?cy
mmp_styczen <- dane2 %>%
    group_by(station, mm, Y, X) %>%
    summarise(t2 = mean(t2m_mean_mon)) %>%
    dplyr::filter(!station %in% c("ŚNIEŻKA", "KASPROWY WIERCH")) %>%
    dplyr::filter(mm == 6 & station %in% stacje_l_mc[["station"]])
# dla sezon?w

head(mmp_styczen, 12)
plot(mmp_styczen$t2)
#
x <- (mmp_styczen$X) # ewentualnie x=unique(europa2$lon)
y <- (mmp_styczen$Y) # ewentualnie y=unique(europa2$lat)
z <- na.omit(mmp_styczen$t2)

## spline interpolation

install.packages("akima", dependencies = T)
library(akima)
akima.spl <- interp(x, y, z,
    linear = F, xo = seq(14, 24.3, .01),
    yo = seq(49, 54.9, .01), extrap = T, duplicate = "mean"
)



# POBIERAM GRANIC? POLSKI DO KT?REJ B?D? EKSTRAPOLOWA?

library(mapview)
library(geodaData)
library(geodata)
library(raster)

granica <- geodata::gadm('GADM', country='POL', level=0)
granica <- as(granica, "Spatial")

image.plot(akima.spl, breaks=breaks, col=colors)
map("world", add=T)

plot(granica, add=T)
map(pol, col="red", fill=TRUE, add=TRUE)


# Mask and crop
library()

x_masked <- mask(x, granica)
x_masked_cropped <- crop(x_masked, granica)

r       <- raster(akima.spl)
plot(r)
r.m     <- mask(r, granica)
plot(r.m, col= matlab.like(45), breaks=seq(-10,25,1))
plot(granica, add=T)


r.m
siatka_n <- raster(granica) # zamieniam na raster
str(siatka_n)
siatka_n

head(akima.spl, 5)
# RYSOWANIE MAPY

# mo?na zamieni? wyinterpolowan? warstw? na raster i maskowa? wszystko poza
#  granic? Polski

rast <- raster(akima.spl)

raster_mask <- mask(rast, granica)

plot(raster_mask, col = matlab.like(28), legend = F, breaks = seq(-5, 23, 1))

image.plot(raster_mask, zlim = c(-4, 23), legend.only = F, col = matlab.like(28))

contour(raster_mask, col = grey(.23), cex = 1, legend = F, breaks = seq(-4, 23, 1), add = T)

plot(granica, border = "grey33", add = T, lwd = 1)

grid(col = "grey83")

points(x, y, type = "p", cex = .5)
text(x, y, round(z, 1), cex = .7, offset = .2, pos = 1)




##
# Tworzenie diagramu klimatycznego Waltera

library(climate)
library(dplyr)
library(climatol)
library(conflicted)

head(poznan_mc)
str(poznan_mc)

df2 <- poznan_mc %>% dplyr::select(station:t2m_mean_mon, rr_monthly)

head(df2)

monthly_summary <- df2 %>%
    group_by(mm) %>%
    summarise(
        opad = sum(rr_monthly) / n_distinct(yy),
        tmax = mean(tmax_abs, na.rm = TRUE),
        tmin = mean(tmin_abs, na.rm = TRUE),
        tmin_abs = min(tmin_abs, na.rm = TRUE)
    )

# tavg = mean(t2m_mean_mon, na.rm = TRUE))
data(datcli)
diagwl(datcli, est = "Example station", alt = 100, per = "1961-90", mlab = "en")

head(monthly_summary)

monthly_summary <- as.data.frame(t(monthly_summary[, -1])) # [, c(5,2,3,4)]))
monthly_summary <- round(monthly_summary, 1)
colnames(monthly_summary) <- month.abb
print(monthly_summary)

# create plot with use of the "climatol" package:
climatol::diagwl(monthly_summary,
    mlab = "pl", est = "POZNA?", alt = 86,
    per = "1991-2020", p3line = F
)





# inny spos?b rysowania mapy
akima.spl
zmin <- min(akima.spl$z, na.rm = TRUE)
zmax <- max(akima.spl$z, na.rm = TRUE)
breaks <- pretty(c(zmin, zmax), 50)
breaks <- pretty(seq(-15, 25, 1), 100)
colors <- matlab.like(length(breaks) - 1) # cyan2yellow diverge_hsv
with(akima.spl, image.plot(x, y, z, breaks = breaks, col = colors))
with(akima.spl, contour(x, y, z, levels = seq(round(zmin, -10), round(zmax, 10), 1), labcex = 1, add = TRUE), lwd = .01, ltp = 2)
# with(akima.spl,contour(x,y,z,breaks=breaks ,labcex=1, add=TRUE), lwd=.01, ltp=2)

with(akima.spl, map("world", add = T, resolution = 0, boundary = T, interior = T, col = "blue", lwd = 0.6))
box()
grid()



# jeszcze inny spos?b rysowania mapy (ggplot)
str(akima.spl)
akima_df <- data.frame(lon = x, lat = y, z = z)

head(akima_df)
str(akima_df)

ggplot() +
    geom_raster(data = akima_df, aes(x = lon, y = lat, fill = z), interpolate = T)
# geom_sf(data = spData::world, fill = "grey",alpha=0, col = "black")+
# geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
borders("world", colour = "grey26", xlim = c(14, 24), ylim = c(49, 55)) +
    coord_sf(xlim = c(14, 24), ylim = c(49, 55)) +
    # scale_color_jco(name = "Argo float")+
    theme_bw() +
    theme(
        legend.position = c(.927, .743),
        legend.background = element_rect(colour = 1),
        legend.key.width = unit(.75, "lines"),
        legend.text = element_text(size = 8, colour = 1),
        axis.text = element_text(colour = 1, size = 12)
    ) +
    geom_label(aes(x = -10, y = 25, label = "a")) +
    labs(x = NULL, y = NULL) +
    scale_fill_gradientn(name = "t", colours = oce::oceColors9A(12)) +
    scale_x_continuous(breaks = seq(14, 24, 2)) +
    scale_y_continuous(breaks = seq(49, 55, 2)) #+
facet_wrap(~dzien, ncol = 1)










image(r.m, col = matlab.like(35), legend = T, breaks = seq(-10, 25, 1))




### POPRAWMY MAP? ABY BY?A EKSTRAPOLOWANA DO GRANIC POLSKI

# POBIERAM GRANIC? POLSKI DO KT?REJ B?D? EKSTRAPOLOWA?
library(mapview)

granica <- getData("GADM", country = "POL", level = 0)

image.plot(akima.spl, breaks = breaks, col = colors)
map("world", add = T)

plot(granica, add = T)
map(pol, col = "red", fill = TRUE, add = TRUE)


# Mask and crop


x_masked <- mask(x, granica)
x_masked_cropped <- crop(x_masked, granica)

r <- raster(akima.spl)
plot(r)
r.m <- mask(r, granica)
plot(r.m, col = matlab.like(45), breaks = seq(-10, 25, 1))
plot(granica, add = T)


r.m
siatka_n <- raster(granica) # zamieniam na raster
str(siatka_n)
siatka_n




# IDW
P <- mmp_styczen[, 5]
P
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(P, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE # Create SpatialPixel object
fullgrid(grd) <- TRUE # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata = grd, idp = 2.0)

# Convert to raster object then clip to Texas
r <- raster(P.idw)
r.m <- mask(r, W)

# Plot
tm_shape(r.m) +
    tm_raster(
        n = 10, palette = "RdBu", auto.palette.mapping = FALSE,
        title = "Predicted precipitation \n(in inches)"
    ) +
    tm_shape(P) + tm_dots(size = 0.2) +
    tm_legend(legend.outside = TRUE)







outline <- granica <- getData("GADM", country = "POL", level = 0)

image.plot(akima.spl, breaks = breaks, col = colors)
#   map("world", add=T)



library(maps)
library(mapdata)


library(maps)
library(mapdata)

# Explicitly state the name of the region ("Poland"):
image.plot(akima.spl, breaks = breaks, col = colors)
outline <- map("worldHires", regions = "Poland", exact = TRUE, plot = FALSE) # returns a list of x/y coords
str(outline)
outline
xrange <- range(outline$x, na.rm = TRUE) # get bounding box
xrange
yrange <- range(outline$y, na.rm = TRUE)
xbox <- xrange + c(-.2, .2)
ybox <- yrange + c(-.2, .2)

subset <- !is.na(outline$x)

xbox
ybox
polypath(c(outline$x[subset], NA, c(xbox, rev(xbox))),
    c(outline$y[subset], NA, rep(ybox, each = 2)),
    col = "white", rule = "evenodd"
)
box()
grid()


# Create Walter & Lieth climatic diagram based on downloaded data

library(climate)
library(dplyr)
library(climatol)

df <- meteo_imgw(interval = "monthly", rank = "synop", year = 1991:2020, station = "POZNA?")
df2 <- select(df, station:t2m_mean_mon, rr_monthly)

monthly_summary <- df2 %>%
    group_by(mm) %>%
    summarise(
        tmax = mean(tmax_abs, na.rm = TRUE),
        tmin = mean(tmin_abs, na.rm = TRUE),
        tavg = mean(t2m_mean_mon, na.rm = TRUE),
        opad = sum(rr_monthly) / n_distinct(yy)
    )

monthly_summary <- as.data.frame(t(monthly_summary[, c(5, 2, 3, 4)]))
monthly_summary <- round(monthly_summary, 1)
colnames(monthly_summary) <- month.abb
print(monthly_summary)

# create plot with use of the "climatol" package:
climatol::diagwl(monthly_summary,
    mlab = "en", est = "POZNA?", alt = NA,
    per = "1991-2019", p3line = F
)



# PRZYK?AD FUNKCJI
FnaC <- function(temperatura_f) {
    (temperatura_f - 32) / 1.8
}
DANE <- c(1:24, 2.2)
FnaC(DANE)
