#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinythemes")
#install.packages("LiblineaR")
#install.packages("ggplot2")
#install.packages("caret")

library(shiny)
library(shinydashboard)
library(shinythemes)
library(caret)
library(LiblineaR)
library(readr)
library(ggplot2)

Dataset_R_Shiny <- read.csv(file = "Dataset_R_Shiny.csv")
# On crée des objets pour nos features et target. Les noms donnés sont extrêmement importants pour
# que le predict() fonctionne par la suite. Il faut les conserver.
x <- Dataset_R_Shiny$squareMeters # feature
y <- Dataset_R_Shiny$price # target
my_model <- lm(y ~ x) # fit du modèle
save(my_model , file = 'mon_modele_lineaire.rda') # Sauvegarde de notre modèle
load("mon_modele_lineaire.rda")    # Chargement de notre modèle

# Interface utilisateur :
ui <- fluidPage(
  
   dashboardPage(skin="black",
                 dashboardHeader(title=tags$em("Prédire le prix de l'immobilier à Paris",
                                               style="text-align:center;color:#006600;font-size:100%"),
                                 titleWidth = 800),
                 
                 dashboardSidebar(width = 250,
                                  sidebarMenu(
                                    br(),
                                    menuItem(tags$em("Faire une prédiction",style="font-size:120%"),icon=icon("prediction"),tabName="prediction"),
                                    menuItem(tags$em("Téléverser des données",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                    menuItem(tags$em("Télécharger les prédictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                                    
                                    
                                  )
                 ),
                 
                 dashboardBody(
                   tabItems(
                     
                     tabItem(tabName="prediction",
                             tags$h4("Choisissez une surface et obtenez une prédiction du prix.", style="font-size:150%"),
                             sliderInput("surface_input", "Surface (en m²) :",
                                         min = 1, max = 10000,
                                         value = 0.5, step = 1),
                             tags$h4("Prix prédit (en €) :", style="font-size:150%"),
                             uiOutput("slider_prediction_value"),
                             
                             br(),
                             br(),
                             br(),
                             br(),
                             br()
                             
                             ),
                     
                     
                     
                     
                     tabItem(tabName="data",
                             
                             
                             br(),
                             br(),
                             br(),
                             br(),
                             tags$h4("Cette page est une appli Shiny réalisée en R, elle repose sur un modèle de
                                     régression linéaire simple pour prédire les prix des appartements à Paris à 
                                     partir de leur surface en mètres carrés.", style="font-size:150%"),
                             
                             
                             br(),
                             
                             tags$h4("Afin de prédire avec ce modèle, téléversez vos données de test au format csv avec le bouton ci-dessous.", style="font-size:150%"),
                             
                             tags$h4("Ensuite, rendez-vous sur la section", tags$span("Télécharger les prédictions",style="color:red"),
                                     tags$span("dans la barre latérale pour récupérer l'output du modèle."), style="font-size:150%"),
                             
                             br(),
                             br(),
                             br(),
                             column(width = 4,
                                    fileInput('file1', em('Charger des données en csv.',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                              accept=c('.csv')),
                                    
                                    uiOutput("sample_input_data_heading"),
                                    tableOutput("sample_input_data"),
                                    
                                    
                                    br(),
                                    br(),
                                    br(),
                                    br()
                             ),
                             br()
                             
                             ),
                     
                     
                     tabItem(tabName="download",
                             fluidRow(
                               br(),
                               br(),
                               br(),
                               br(),
                               column(width = 8,
                                      tags$h4("Après avoir téléversé des données de test, vous pouvez récupérer les prédictions au format csv en cliquant sur le bouton ci-dessous.", 
                                              style="font-size:200%"),
                                      br(),
                                      br()
                               )),
                             fluidRow(
                               
                               column(width = 7,
                                      downloadButton("downloadData", em('Télécharger les prédictions',style="text-align:center;color:blue;font-size:150%")),
                                      plotOutput('plot_predictions')
                               ),
                               column(width = 4,
                                      uiOutput("sample_prediction_heading"),
                                      tableOutput("sample_predictions")
                               )
                               
                             ))
                     )))
   
   )


# Logique du serveur :
server <- (function(input, output) {
  
  sliderValues <- reactive({
    
    # Le predict a besoin d'un data.frame avec le même nom pour la feature que lors du fit :
    as.character(predict(my_model,
                         data.frame(x = input$surface_input)))
    
  })
  
  output$slider_prediction_value <- renderText({
    sliderValues() # Affichage de la prédiction
  })  
  
  
  # Poids de requête serveur maximal, par défaut à 5MB, ici passé à 80MB, joue aussi sur l'upload
  # de fichiers :
  options(shiny.maxRequestSize = 800*1024^2)  

  
  output$sample_input_data_heading = renderUI({   # s'affiche si un fichier a été uploadé
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Donnée :')
    }
  })

  output$sample_input_data = renderTable({    # montrer un head de la data uploadée
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      keeps <- c("squareMeters","price")
      input_data = input_data[keeps]      
      input_data

      input_data$price = as.factor(input_data$price)
      head(input_data)
      
    }
  })

  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Prédiction en cours, veuillez patienter ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        keeps <- c("squareMeters","price")
        input_data = input_data[keeps]      
        
        colnames(input_data) = c("squareMeters", "price")
        
        input_data$price = as.factor(input_data$price )
        
        prediction = predict(my_model, input_data)
        
        input_data_with_prediction = cbind(input_data,prediction )
        input_data_with_prediction
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({  # S'affiche si de la donnée a été uploadée en csv
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Prédictions : ')
    }
  }
  
  )
  
  output$sample_predictions = renderTable({ # Affiche un head des prédictions 
    pred = predictions()
    head(pred)
    
  })
  
  output$plot_predictions = renderPlot({ #plot la pred, ne s'affiche que si il y a eu upload
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      
    pred = predictions()
    ggplot(pred, aes(x = squareMeters, y = prediction)) +
      geom_point(size = 4, shape = 19, alpha = 0.6)
    }
  })
  
  
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
})

# Couture des deux composants de l'app 
shinyApp(ui = ui, server = server)

