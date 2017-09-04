rm(list = ls())

library(reshape2)
library(ggplot2)
library(xlsx)

mergeDF <- function(df1, df2){
    return(merge(df1, df2, all = TRUE))
}

readElec <- function(file, dateDebut, dateFin){
    elec <- read.xlsx(file, sheetIndex = 1)
    elec[, 2] <- as.numeric(as.character(elec[, 2]))
    elec$Date <- as.POSIXct(elec$Date, format = "%d/%m/%Y %H:%M")
    elec <- subset(elec, Date > dateDebut & Date < dateFin)[, 1:2]
    colnames(elec)[2] <- "Type"
    return(elec)
}

afficherGraphe <- function(fileTemperature,
                           fileElec,
                           cumElec = T,
                           doubleChan = rep(TRUE, length(fileTemperature)),
                           chanNames = 1:(length(fileTemperature)*2),
                           titre = "")
{
    if(length(fileTemperature) == 1) # Un seul fichier en entrée
    {
        mesure <- read.table(file = fileTemperature, header = T, sep = "\t", dec = ",", na.strings = "----")
        if(ncol(mesure) == 5 | doubleChan == F)
        {
            mesure <- mesure[, 2:3]
            colnames(mesure) <- c("Date", chanNames[1])
        }
        else
        {
            mesure <- mesure[, 2:4]
            colnames(mesure) <- c("Date", chanNames[1], chanNames[2])
        }
        mesure$Date <- as.POSIXct(strptime(mesure$Date, format = "%d/%m/%Y %H:%M:%S"))
    }
    
    else # Plusieurs fichiers en entrée
    
    {
        mesure <- read.table(file = fileTemperature[1], header = T, sep = "\t", dec = ",", na.strings = "----")
        if(ncol(mesure) == 5 | doubleChan[1] == F)
        {
            mesure <- mesure[, 2:3]
            colnames(mesure) <- c("Date", chanNames[1])
            j <- 2
        }
        else
        {
            mesure <- mesure[, 2:4]
            colnames(mesure) <- c("Date", chanNames[1], chanNames[2])
            j <- 3
        }
        mesure$Date <- as.POSIXct(strptime(mesure$Date, format = "%d/%m/%Y %H:%M:%S"))

        for(i in 2:length(fileTemperature))
        {
            tmp <- read.table(file = fileTemperature[i], header = T, sep = "\t", dec = ",", na.strings = "----")
            if(ncol(tmp) == 5 | doubleChan[i] == F)
            {
                tmp <- tmp[, 2:3]
                colnames(tmp) <- c("Date", chanNames[j])
                j <- j + 1
            }
            else
            {
                tmp <- tmp[, 2:4]
                colnames(tmp) <- c("Date", chanNames[j], chanNames[j + 1])
                j <- j + 2
            }
            tmp$Date <- as.POSIXct(strptime(tmp$Date, format = "%d/%m/%Y %H:%M:%S"))

            mesure <- merge(mesure, tmp, all = T)
        }
    }
    dateDebut <- as.character(min(mesure$Date))
    dateFin <- as.character(max(mesure$Date))
    mesure$Time <- as.numeric(difftime(mesure$Date, dateDebut, units = "hours"))
    mesure <- mesure[, -1]
    mesure <- melt(data = mesure,
                   id = "Time",
                   value.name = "Valeur",
                   variable.name = "Mesures")
    
    elec <- readElec(fileElec, dateDebut, dateFin)
    elec$Time <- as.numeric(difftime(elec$Date, dateDebut, units = "hours"))
    elec <- elec[, -1]
    if(cumElec){
        elec <- elec[order(elec$Time), ]
        elec$Type <- 1/6 * (cumsum(elec$Type))
        ylabel <- "Consommation électrique cumulée (en kWh) / Température (en °C)"
    }
    else{
        ylabel <- "Consommation électrique (en kW) / Température (en °C)"
    }
    
    final <- mergeDF(mesure, elec)
    levels(final$Mesures) <- c(levels(final$Mesures), "consoElec")
    final$Valeur[is.na(final$Mesures)] <- final$Type[is.na(final$Mesures)]
    final$Type[is.na(final$Mesures)] <- "Consommation électrique"
    final$Type[!is.na(final$Mesures)] <- "Température"
    final$Type <- as.factor(final$Type)
    final$Type <- factor(final$Type, levels = c("Température", "Consommation électrique"))
    final$Mesures[is.na(final$Mesures)] <- "consoElec"
    final <- final[!is.na(final$Valeur), ]
    
    
    
    ## CREATION GRAPHIQUE
    g <- ggplot(data = final, aes(x = Time, y = Valeur, col = Mesures, group = Mesures)) +
        geom_line(na.rm = T) +
        geom_point(shape = 20, size = 1.8, na.rm = T) +
        geom_hline(yintercept = -18, linetype = "dashed", col = "blue") +
        labs(x = "Temps (en heure)",
             y = "",#ylabel,
             title = titre,
             captions = paste("Données du", dateDebut, "au", dateFin)) +
        theme(legend.background = element_rect(fill = "lightblue",
                                               size = 0.5,
                                               linetype = "solid",
                                               colour = "darkblue")) +
        facet_grid(Type ~ ., scales = "free_y")
    
    print(g)
    return(elec)
}

files <- paste("data/Température/PONTIVY-27juil-", c("enr1.txt",
                                      "enr2.txt",
                                      "enr3.txt",
                                      "enr4.txt",
                                      "enr5.txt"),
               sep = "")

namePontivy <- c("1.1",
           "1.2",
           "2.1",
           "2.2",
           "3.1",
           "3.2",
           "4.1",
           "4.2",
           "5.1",
           "5.2")

nameMoreac <- c("1 - bas porte1",
                "2 - bas porte2",
                "3 - haut fond",
                "4 - haut porte2")


avecElec <- afficherGraphe(fileTemperature = files,
                           fileElec = "data/Electrique/LES_ESSARTS.xlsx",
                           chanNames = namePontivy,
                           #doubleChan = c(F, F, F, F),
                           titre = "Pontivy 27/07/2017; produit: raclette")

