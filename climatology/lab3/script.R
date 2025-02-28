# Tytuł projektu: ZMIENNOŚĆ KLIMATU POZNANIA NA TLE KLIMATU POLSKI


install.packages("climate", dependencies = T)
install.packages("pryr", dependencies = T)
install.packages("dplyr", dependencies = T)

library(climate)
library(pryr)
library(dplyr)

setwd("/Users/marcin/Documents/Projects/uam-geo/climatology/lab3/data") # Ustawiamy ścieżkę do folderu, w którym znajduje się skrypt

mmp <- meteo_imgw(interval = "monthly", rank = "synop", year = c(1966:2022), coords = T)

mmp$station[mmp$station == "POZNAŃ-ŁAWICA"] <- "POZNAŃ"
mmp$station[mmp$station == "KOŁOBRZEG-DŹWIRZYNO"] <- "KOŁOBRZEG"
mmp$station[mmp$station == "WROCŁAW-STRACHOWICE"] <- "WROCŁAW"
mmp$station[mmp$station == "WARSZAWA-OKĘCIE"] <- "WARSZAWA"
mmp$station[mmp$station == "ŁÓDŹ-LUBLINEK"] <- "ŁÓDŹ"

mmp <- mmp[, -39] # Usunięcie zduplikowanej kolumny
poznan_mc <- mmp %>% filter(station %in% c("POZNAŃ"))
saveRDS(poznan_mc, file = "poznan_mc.rds")

View(poznan_mc) # Wyświetlenie danych w tabeli

poznan_avr_mc <- poznan_mc %>%
    group_by(mm) %>%
    summarise(
        t2avr = mean(t2m_mean_mon),
        t2min = min(t2m_mean_mon),
        t2max = max(t2m_mean_mon),
        t2std = sd(t2m_mean_mon)
    )
poznan_avr_mc

plot(poznan_avr_mc$mm, poznan_avr_mc$t2avr,
    ylim = c(-10, 28), xlim = c(1, 12),
    type = "l",
    xlab = "miesiąc",
    ylab = "Temperatura (\u00B0C)",
    main = "Poznań"
)

colnames(mmp)
