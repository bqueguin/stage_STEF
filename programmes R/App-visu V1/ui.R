library(shiny)

shinyUI(


    navbarPage("",
               
        # Panel 1: Combinee -------------------------------------------------------
               
        tabPanel("Combinee",
            sidebarLayout(
                sidebarPanel(
                    fileInput("elecComb",
                              "Selectionner le fichier des donnees de consommation electrique:"),
                    radioButtons("consoCumulComb", "Consommation electrique",
                                  c("Cumulee" = TRUE, "Instantanee" = FALSE)),
                    fileInput("temperatureComb",
                              "Selectionner les fichiers de temperature: ",
                               multiple = TRUE),
                    dateRangeInput("dates",
                                   "Dates:",
                                   weekstart = 1,
                                   language = 'fr',
                                   start = "2017-07-01"
                    ),
                    sliderInput("heureDebut", "heure debut", 0, 24, 0),
                    sliderInput("heureFin", "heure fin", 0, 24, 24)
                ),
                mainPanel(
                    plotOutput("grapheComb", height = "800px")
                )
            )
        ),

        # Panel 2 : elec ----------------------------------------------------------

        tabPanel("Consommation electrique",
            sidebarLayout(
                sidebarPanel(
                    fileInput("elec",
                              "Selectionner le fichier des donnees de consommation electrique:"),
                    radioButtons("consoCumul", "Consommation electrique",
                                 c("Cumulee" = TRUE, "Instantanee" = FALSE))
                ),
                mainPanel(
                    plotOutput("graphe1")
                )
            )
        ),


        # Panel 3: Temperature ----------------------------------------------------


        tabPanel("Temperature",
            sidebarLayout(
                sidebarPanel(
                    fileInput("temperature",
                              "Selectionner les fichiers de temperature: ",
                               multiple = TRUE)
                ),
                mainPanel(
                    plotOutput("graphe2")
                )
            )
        ),


        
        
        

        # Panel 4: Dates ----------------------------------------------------------

        tabPanel("Parametres",
            sidebarLayout(
                sidebarPanel(
                    dateRangeInput("dates",
                                   "Dates:",
                                    weekstart = 1,
                                    language = 'fr',
                                    start = "2017-07-01"
                                   ),
                    sliderInput("heureDebut", "heure debut", 0, 24, 0),
                    sliderInput("heureFin", "heure fin", 0, 24, 24)
                ),
                mainPanel(
                    plotOutput("graphe3")
                )
            )
        )
        
        
    )
)




