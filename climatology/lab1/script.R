# Komentarze robimy za pomocą '#'

install.packages("raster", dependencies=T) # Instalujemy pakiet 'raster'
install.packages("dplyr", dependencies=T)
install.packages("RCurl", dependencies=T)
install.packages("XML", dependencies=T)
install.packages("remotes", dependencies=T)
install.packages("climate", dependencies=T)

# install_github('bczernecki/climate' # Instalujemy pakiet z githuba

library(raster) # Ładujemy pakiet 'raster'
library(dplyr)
library(RCurl)
library(XML)
library(remotes)
library(climate)

# Ćwiczenia 1
setwd('/Users/marcin/Documents/Projects/UAM/klimatologia/cw1') # Ustawiamy ścieżkę do folderu, w którym znajduje się skrypt
getwd() # Sprawdzamy, czy ścieżka została ustawiona poprawnie
dir() # Wyświetlamy zawartość folderu

?raster # Tak wyświetlamy pomoc do funkcji

??climate # Tak wyświetlamy pomoc do pakietu

imgw_meteo_abbrev # Metadane meteorologiczne
imgw_meteo_stations # Dane meteorologiczne

poland <- stations_ogimet(country="Poland", add_map=T) # ogimet - źródło danych (sprawdzone)
poland[,2] # Wyświetlamy drugą kolumnę z danych
poland[1:5,] # Wyświetlamy pierwsze 5 wierszy
poland

dim(poland) # Rozmiar danych
head(poland, 10) # Wyświetlamy pierwsze 10 wierszy
tail(poland, 10) # Wyświetlamy ostatnie 10 wierszy

?stations_ogimet

c(1:100) # Tworzymy wektor od 1 do 100
