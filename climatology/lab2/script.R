install.packages("climate", dependencies = T)
install.packages("pryr", dependencies = T)

library(climate)
library(pryr)

setwd("/Users/marcin/Documents/Projects/uam-geo/climatology/lab2/data") # Ustawiamy ścieżkę do folderu, w którym znajduje się skrypt


poland <- stations_ogimet(country = "Poland", add_map = T) # ogimet - źródło danych (sprawdzone)

# Wyświetlanie struktury dataframe'u lub wektora
str(poland)

poland$station_names

nowy <- poland[c(1, 5), c(1, 5)]
nowy

# Tworzenie wektoru danych
c(1, 2, "a", "b")

# Wybranie pierwszej kolumny
kolumny <- poland[, 1]
kolumny


wiersze <- poland[1, ]
wiersze


# Pierwsza i ostatnia kolumna
pierwsza_i_ostatnia <- poland[c(1:3), c(1, 5)]
pierwsza_i_ostatnia


# Długość wektora
dlugosc <- length(c(1, 2, 3, "a", "b"))
dlugosc


# Wybranie miesięcznych danych meteorologicznych z IMGW dla lat 1966-2022
dane_mc <- meteo_imgw(interval = "monthly", rank = "synop", year = c(1966:2022), coords = T)
dane_mc

# Nazwy kolumn
colnames(dane_mc)

# Wyświetlenie wykresu
plot(dane_mc$t2m_mean_mon)

# Wyświetlanie wykresu pudełkowego
boxplot(dane_mc$t2m_mean_mon)

# Wyświetlanie danych unikatowych
unique(dane_mc$station)

# Wyświetlanie rozmiaru danych
object_size(dane_mc)

# Zapis danych do pliku
saveRDS(dane_mc, file = "dane_mc.rds")

# Odczyt z pliku
mmp <- readRDS(file = "dane_mc.rds")

# Wyświetlanie histogramu
hist(mmp$tmax_abs)

# Wyświetlanie unikalnych wierszy z kolumn 'station', 'X', 'Y'
unique(mmp[, c("station", "X", "Y")])

# Zamiana nazwy stacji (np. Poznań-Ławica na Poznań)
mmp$station[mmp$station == "POZNAŃ-ŁAWICA"] <- "POZNAŃ"
mmp$station[mmp$station == "KOŁOBRZEG-DŹWIRZYNO"] <- "KOŁOBRZEG"
mmp$station[mmp$station == "WROCŁAW-STRACHOWICE"] <- "WROCŁAW"
mmp$station[mmp$station == "WARSZAWA-OKĘCIE"] <- "WARSZAWA"
mmp$station[mmp$station == "ŁÓDŹ-LUBLINEK"] <- "ŁÓDŹ"

mmp$X[mmp$station == "BYDGOSZCZ"] <- 18.006 # Uzupełnianie krotki X dla stacji Bydgoszcz
mmp$Y[mmp$station == "BYDGOSZCZ"] <- 53.106 # Uzupełnianie krotki Y dla stacji Bydgoszcz

mmp$X[mmp$station == "HALA GĄSIENICOWA"] <- 20.0057
mmp$Y[mmp$station == "HALA GĄSIENICOWA"] <- 49.2441

plot(mmp$tmax_abs)
