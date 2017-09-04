library(shiny)
library(dygraphs)

shinyUI(


    navbarPage("",

        # Panel 1 : elec ----------------------------------------------------------

        tabPanel("Consommation electrique",
            sidebarLayout(
                sidebarPanel(
                    fileInput("elec",
                              "Selectionner le fichier des donnees de consommation electrique:"),
                    radioButtons("consoCumul", "Consommation electrique",
                                 c("Cumulee" = TRUE, "Instantanee" = FALSE)),
                    uiOutput("condition")
                ),
                mainPanel(
                    plotOutput("graphe1")
                )
            )
        ),


        # Panel 2: Temperature ----------------------------------------------------


        tabPanel("Temperature",
            sidebarLayout(
                sidebarPanel(
                    fileInput("temperature",
                              "Selectionner les fichiers de temperature: ",
                               multiple = TRUE)
                    
                ),
                mainPanel(
                    plotOutput("graphe2",
                                dblclick = "graphe2_dblclick",
                                brush = brushOpts(
                                   id = "graphe2_brush",
                                   resetOnNew = TRUE)
                    )
                )
            )
        ),


        # Panel 3: Combinee -------------------------------------------------------

        tabPanel("Combinee",
            sidebarLayout(
                sidebarPanel(
                    fileInput("elecComb",
                              "Selectionner le fichier des donnees de consommation electrique:"),
                    radioButtons("consoCumulComb", "Consommation electrique",
                                 c("Cumulee" = TRUE, "Instantanee" = FALSE)),
                    fileInput("temperatureComb",
                              "Selectionner les fichiers de temperature: ",
                              multiple = TRUE)
                ),
                mainPanel(
                    plotOutput("grapheComb",height = "850px")
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
                                    start = Sys.Date() - 31),
                    sliderInput("heureDebut", "heure debut", 0, 24, 0),
                    sliderInput("heureFin", "heure fin", 0, 24, 24)
                ),
                mainPanel(
                    plotOutput("graphe3")
                )
            )
        ),



        # Panel 5: Donnees --------------------------------------------------------


        tabPanel("Data",
            mainPanel(
                dataTableOutput("mytable")
            )
        )
        
        
    )
)




