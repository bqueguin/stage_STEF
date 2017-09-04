library(reshape2)
library(ggplot2)
library(xlsx)


# Pre-processing ----------------------------------------------------------


piquer <- read.table("data/chambreMOREAC/MOREAC-piquer.txt",
                     sep = ";",
                     skip = 8,
                     header = F,
                     row.names = 1,
                     col.names = c("", "Date", "Heure", "Piq1", "Piq2", "Piq3", "Piq4", ""),
                     stringsAsFactors = F)[, -7]

piquer$Piq1 <- as.numeric(piquer$Piq1)
piquer$Piq2 <- as.numeric(piquer$Piq2)
piquer$Piq3 <- as.numeric(piquer$Piq3)
piquer$Piq4 <- as.numeric(piquer$Piq4)

piquer$Date <- as.POSIXct(strptime(paste(piquer$Date, piquer$Heure), format = "%d/%m/%Y %H:%M:%S"))
piquer <- piquer[, -2]


ambiance <- read.table("data/chambreMOREAC/MOREAC-ambiance.txt",
                       sep = ";",
                       skip = 8,
                       header = F,
                       row.names = 1,
                       col.names = c("", "Date", "Heure", "Amb1", "Amb2", "Amb3", "Amb4", ""),
                       stringsAsFactors = F)[, -7]

ambiance$Amb1 <- as.numeric(ambiance$Amb1)
ambiance$Amb2 <- as.numeric(ambiance$Amb2)
ambiance$Amb3 <- as.numeric(ambiance$Amb3)
ambiance$Amb4 <- as.numeric(ambiance$Amb4)

ambiance$Date <- as.POSIXct(strptime(paste(ambiance$Date, ambiance$Heure), format = "%d/%m/%Y %H:%M:%S"))
ambiance <- ambiance[, -2]


# Association piquer/ambiance ---------------------------------------------

moreac <- merge(ambiance, piquer, by = "Date")
moreac2 <- melt(data = moreac,
               id.vars = "Date",
               variable.name = "Sonde",
               value.name = "Temperature")

# Affichage

debut <- "2017-07-01 00:00:00" # min <- "2017-07-01 00:00:00"
fin <- "2017-07-19 11:10:00" # max <- "2017-07-19 11:10:00"

g1 <- ggplot(data = moreac2, aes(x = Date, y = Temperature, col = Sonde)) +
    geom_line(na.rm = T) +
    geom_hline(yintercept = -18, linetype = "dashed", col = "blue") +
    xlim(as.POSIXct(strptime(debut, format = "%Y-%m-%d %H:%M:%S")),
         as.POSIXct(strptime(fin, format = "%Y-%m-%d %H:%M:%S"))) +
    theme(legend.background = element_rect(fill = "lightblue",
                                           size = 0.5,
                                           linetype = "solid",
                                           colour = "darkblue"))

plot(g1)

write.xlsx(moreac, "jumo.xlsx")