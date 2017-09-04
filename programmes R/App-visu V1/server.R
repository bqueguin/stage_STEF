

# Packages ----------------------------------------------------------------

library(shiny)
library(reshape2)
library(ggplot2)
library(xlsx)
library(gridExtra)
library(lubridate)




shinyServer(function(input, output, session) {

    

# graphe elec -------------------------------------------------------------

output$graphe1 <- renderPlot({
    
    activeElec <- !is.null(input$elec)
    if(!activeElec){
        return(NULL)
    }
    else
    {
        elec <- read.xlsx(input$elec$datapath,
                          sheetIndex = 1,
                          header = F,
                          colClasses = c(NA, "numeric"))
        colnames(elec) <- c("Date", "Conso")
        elec$Date <- as.POSIXct(elec$Date, format = "%d/%m/%Y %H:%M:%S")
        elec[, 2:ncol(elec)] <- as.numeric(as.character(elec[, 2:ncol(elec)]))
        ylab <- "Consommation electrique instantanee (en kW)"
        if(input$consoCumul){
            elec <- elec[order(elec$Date), ]
            elec[, 2:ncol(elec)] <- 1/6 * cumsum(elec[, 2:ncol(elec)])
            ylab <- "Consommation electrique cumulee (en kWh)"
        }
        
        g1 <- ggplot(elec, aes(x = Date, y = Conso)) +
            geom_line(na.rm = T) +
            geom_point(shape = 20, size = 1.8, na.rm = T) +
            xlim(as.POSIXct(format(c(input$dates[1], input$dates[2]))) + c(3600*input$heureDebut, 3600*input$heureFin)) +
            labs(x = "Date",
                 y = ylab,
                 title = "",
                 captions = paste("Donnees du", input$dates[1], "au", input$dates[2])) +
            theme(legend.background = element_rect(fill = "lightblue",
                                                   size = 0.5,
                                                   linetype = "solid",
                                                   colour = "darkblue"))
        
        plot(g1)
    }
    
})


# graphe temperature ------------------------------------------------------

output$graphe2 <- renderPlot({

    activeTempe <- !is.null(input$temperature)

    if(!activeTempe){
        return(NULL)
    }
    else
    {
        if(grepl(".xlsx$", input$temperature$name[1])){
            temperature <- read.xlsx(file = input$temperature$datapath[1],
                                     sheetIndex = 1,
                                     header = F,
                                     colClasses = c(NA, "numeric"))

            colnames(temperature) <- c("Date", input$temperature$name[1])
            temperature$Date <- as.POSIXct(temperature$Date, format = "%d/%m/%Y %H:%M:%S")
        }else{

            temperature <- read.table(file = input$temperature$datapath[1],
                                      header = T,
                                      sep = "\t",
                                      dec = ",",
                                      na.strings = "----",
                                      colClasses = c("NULL", NA, "numeric", "numeric", "NULL", "NULL"),
                                      col.names = c(NA, "Date",
                                                    paste(input$temperature$name[1],"1"),
                                                    paste(input$temperature$name[1],"2"), NA, NA))
            temperature$Date <- as.POSIXct(temperature$Date, format = "%d/%m/%Y %H:%M:%S")
        }

        if(nrow(input$temperature) >= 2){
            i <- 2
            for(f in input$temperature$datapath[-1]){

                if(grepl(".xlsx$", input$temperature$name[i])){
                    temporaire <- read.xlsx(file = f,
                                            sheetIndex = 1,
                                            header = F,
                                            colClasses = c(NA, "numeric"))

                    colnames(temporaire) <- c("Date", input$temperature$name[i])
                    temporaire$Date <- as.POSIXct(temporaire$Date, format = "%d/%m/%Y %H:%M:%S")
                }else{

                    temporaire <- read.table(file = f,
                                             header = T,
                                             sep = "\t",
                                             dec = ",",
                                             na.strings = "----",
                                             colClasses = c("NULL", NA, "numeric", "numeric", "NULL", "NULL"),
                                             col.names = c(NA, "Date",
                                                           paste(input$temperature$name[i],"1"),
                                                           paste(input$temperature$name[i],"2"), NA, NA))
                    temporaire$Date <- as.POSIXct(temporaire$Date, format = "%d/%m/%Y %H:%M:%S")
                }
                i <- i + 1
                temperature <- merge(temperature, temporaire, all = T)
            }
        }

        temperature <- melt(temperature,
                            id.vars = "Date",
                            value.name = "Valeurs",
                            variable.name = "Mesures",
                            na.rm = T)
        g2 <- ggplot(data = temperature, aes(x = Date, y = Valeurs, col = Mesures)) +
            geom_line(na.rm = T) +
            geom_point(shape = 20, size = 1.8, na.rm = T) +
            geom_hline(yintercept = -18, linetype = "dashed", col = "blue") +
            xlim(as.POSIXct(format(c(input$dates[1], input$dates[2]))) + c(3600*input$heureDebut, 3600*input$heureFin)) +
            labs(x = "Date",
                 y = "Temperature",
                 title = "",
                 captions = paste("Donnees du", input$dates[1], "au", input$dates[2])) +
            theme(legend.background = element_rect(fill = "lightblue",
                                                   size = 0.5,
                                                   linetype = "solid",
                                                   colour = "darkblue"),
                  legend.position = "top")
        plot(g2)
    }
})



# # graphe combinee ---------------------------------------------------------
# 
# 
output$grapheComb <- renderPlot({
    activeElecComb <- !is.null(input$elecComb)
    activeTempeComb <- !is.null(input$temperatureComb)

    if(!activeElecComb | !activeTempeComb){
        return(NULL)
    }
    else
    {
        elecFileNameComb <- input$elecComb$datapath
        elec <- read.xlsx(elecFileNameComb,
                          sheetIndex = 1,
                          header = F,
                          colClasses = c(NA, "numeric"))
        colnames(elec) <- c("Date", "Conso")
        elec$Date <- as.POSIXct(elec$Date, format = "%d/%m/%Y %H:%M:%S")
        elec[, 2:ncol(elec)] <- as.numeric(as.character(elec[, 2:ncol(elec)]))
        ylab <- "Consommation electrique instantanee (en kW)"
        if(input$consoCumulComb){
            elec <- elec[order(elec$Date), ]
            elec[, 2:ncol(elec)] <- 1/6 * cumsum(elec[, 2:ncol(elec)])
            ylab <- "Consommation electrique cumulee (en kWh)"
        }

        g1Comb <- ggplot(elec, aes(x = Date, y = Conso)) +
            geom_line(na.rm = T) +
            geom_point(shape = 20, size = 1.8, na.rm = T) +
            xlim(as.POSIXct(format(c(input$dates[1], input$dates[2]))) + c(3600*input$heureDebut, 3600*input$heureFin)) +
            labs(x = "Date",
                 y = ylab,
                 title = "",
                 captions = paste("Donnees du", input$dates[1], "au", input$dates[2])) +
            theme(legend.background = element_rect(fill = "lightblue",
                                                   size = 0.5,
                                                   linetype = "solid",
                                                   colour = "darkblue"))

        if(grepl(".xlsx$", input$temperatureComb$name[1])){
            temperature <- read.xlsx(file = input$temperatureComb$datapath[1],
                                     sheetIndex = 1,
                                     header = F,
                                     colClasses = c(NA, "numeric"))

            colnames(temperature) <- c("Date", input$temperature$name[1])
            temperature$Date <- as.POSIXct(temperature$Date, format = "%d/%m/%Y %H:%M:%S")
        }else{

            temperature <- read.table(file = input$temperatureComb$datapath[1],
                                      header = T,
                                      sep = "\t",
                                      dec = ",",
                                      na.strings = "----",
                                      colClasses = c("NULL", NA, "numeric", "numeric", "NULL", "NULL"),
                                      col.names = c(NA, "Date",
                                                    paste(input$temperatureComb$name[1],"1"),
                                                    paste(input$temperatureComb$name[1],"2"), NA, NA))
            temperature$Date <- as.POSIXct(temperature$Date, format = "%d/%m/%Y %H:%M:%S")
        }

        if(nrow(input$temperatureComb) >= 2){
            i <- 2
            for(f in input$temperatureComb$datapath[-1]){

                if(grepl(".xlsx$", input$temperatureComb$name[i])){
                    temporaire <- read.xlsx(file = f,
                                            sheetIndex = 1,
                                            header = F,
                                            colClasses = c(NA, "numeric"))

                    colnames(temporaire) <- c("Date", input$temperatureComb$name[i])
                    temporaire$Date <- as.POSIXct(temporaire$Date, format = "%d/%m/%Y %H:%M:%S")
                }else{

                    temporaire <- read.table(file = f,
                                             header = T,
                                             sep = "\t",
                                             dec = ",",
                                             na.strings = "----",
                                             colClasses = c("NULL", NA, "numeric", "numeric", "NULL", "NULL"),
                                             col.names = c(NA, "Date",
                                                           paste(input$temperatureComb$name[i],"1"),
                                                           paste(input$temperatureComb$name[i],"2"), NA, NA))
                    temporaire$Date <- as.POSIXct(temporaire$Date, format = "%d/%m/%Y %H:%M:%S")
                }
                i <- i + 1
                temperature <- merge(temperature, temporaire, all = T)
            }
        }

        temperature <- melt(temperature,
                            id.vars = "Date",
                            value.name = "Valeurs",
                            variable.name = "Mesures",
                            na.rm = T)
        g2Comb <- ggplot(data = temperature, aes(x = Date, y = Valeurs, col = Mesures)) +
            geom_line(na.rm = T) +
            geom_point(shape = 20, size = 1.8, na.rm = T) +
            geom_hline(yintercept = -18, linetype = "dashed", col = "blue") +
            xlim(as.POSIXct(format(c(input$dates[1], input$dates[2]))) + c(3600*input$heureDebut, 3600*input$heureFin)) +
            labs(x = "Date",
                 y = "Temperature",
                 title = "",
                 captions = paste("Donnees du", input$dates[1], "au", input$dates[2])) +
            theme(legend.background = element_rect(fill = "lightblue",
                                                   size = 0.5,
                                                   linetype = "solid",
                                                   colour = "darkblue"),
                  legend.position = "top")

        grid.arrange(g1Comb, g2Comb, ncol = 1)
    }
})

#  
#     
#     
#    
#        
# 
# # Equivalent tonne congelee -----------------------------------------------
# 
# output$equivTon <- renderPrint({
#     tonne <- as.numeric(input$tonnage)
#     return(as.character(max(elec$Conso)/tonne))
# })
# 
# 
# # observeEvent(input$elec, {
# #     if(activeElec & !activeTempe){
# #         updateDateRangeInput(session,
# #                              "dates",
# #                              start = min(elec$Date),
# #                              end = max(elec$Date))
# #         updateSliderInput(session,
# #                           "heureDebut",
# #                           value = hour(min(elec$Date)))
# #         updateSliderInput(session,
# #                           "heureFin",
# #                           value = hour(max(elec$Date)))
# #     }
# #     if(!activeElec & activeTempe){
# #         updateDateRangeInput(session,
# #                              "dates",
# #                              start = min(temperature$Date),
# #                              end = max(temperature$Date))
# #         updateSliderInput(session,
# #                           "heureDebut",
# #                           value = hour(min(temperature$Date)))
# #         updateSliderInput(session,
# #                           "heureFin",
# #                           value = hour(max(temperature$Date)))
# #     }
# #     if(activeElec & activeTempe){
# #         updateDateRangeInput(session,
# #                              "dates",
# #                              start = min(c(elec$Date, temperature$Date)),
# #                              end = max(c(elec$Date, temperature$Date)))
# #         updateSliderInput(session,
# #                           "heureDebut",
# #                           value = hour(min(c(elec$Date, temperature$Date))))
# #         updateSliderInput(session,
# #                           "heureFin",
# #                           value = hour(max(c(elec$Date, temperature$Date))))
# #     }
# # },
# # once = TRUE)
})
