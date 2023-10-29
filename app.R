if(!require("jsonlite")){
  install.packages("jsonlite")
}
library(jsonlite)

if(!require("leaflet")){
  install.packages("leaflet")
}
library(leaflet)

if(!require("httr")){
  install.packages("httr")
}
library(httr)

if(!require("ggplot2")){
  install.packages("ggplot2")
}
library(ggplot2)

if(!require("shiny")){
  install.packages("shiny")
}
library(shiny)

if(!require("tidygeocoder")){
  install.packages("tidygeocoder")
}
library(tidygeocoder)

if(!require("geosphere")){
  install.packages("geosphere")
}
library(geosphere)

if(!require("dplyr")){
  install.packages("dplyr")
}
library(dplyr)

if(!require("DT")){
  install.packages("DT")
}
library(DT)

if(!require("shinydashboard")){
  install.packages("shinydashboard")
}
library(shinydashboard)

if(!require("shinyWidgets")){
  install.packages("shinyWidgets")
}
library(shinyWidgets)


# appel de la base de donnée originale
base <- 'https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=5de52836beec2f43de942c79725c1d693e8818bc'

raw_data <- GET(base)

# création du dataframe qui va être utilisé pour le reste de l'application R
df_base <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
df_base$lat <- df_base$position$lat
df_base$lng <- df_base$position$lng
order <- c("number", "contract_name", "name", "address", "position.lat", "position.lng", "banking", "bonus", "bike_stands", "available_bike_stands", "available_bikes", "status", "last_update")
df_2 <- df_base[, order]

df_code_post <- read.csv2(file = "df_code_post.csv", header = T, sep = ",")

df_final <- merge(x = df_2, y = df_code_post, by = "number", all.x = TRUE)
df_final <- df_final[, c("number", "contract_name", "name", "address", "postcode", "position.lat", "position.lng", "banking", "bonus", "bike_stands", "available_bike_stands", "available_bikes", "status", "last_update")]

# création de la carte
map_lyon <- leaflet() %>%
  addTiles()
map_lyon <- map_lyon %>%
  addCircleMarkers(data = df_final, 
                   lat = ~position.lat, 
                   lng = ~position.lng, 
                   radius = 15,
                   fillOpacity = .5,
                   stroke = TRUE,
                   popup = paste("Nom de la station : ", df_final$name, "<br>", 
                                 "Adresse : ", df_final$address, "<br>", 
                                 "Code postal : ", df_final$postcode, "<br>", 
                                 "Nombre de places de vélo disponibles : ", df_final$available_bike_stands, "<br>", 
                                 "Nombre de vélos : ", df_final$available_bikes),
                   clusterOptions = markerClusterOptions())



ui <- fluidPage(
  titlePanel("Application Vélo'V"),

# bouton pour rafraichir la page
  fluidRow(
    column(12, 
           align = "center",
           actionButton("bouton_reset", "Rafraîchir la page"))),
  
# création des onglets
  tabsetPanel(
    tabPanel("Carte",
             h2("Carte de Lyon avec les stations Vélo'V"),
             column(12, align = "center", mainPanel(map_lyon))),
    
    tabPanel("Statistiques",
             h2("Statistiques sur l'ensemble des stations"),
             br(), 
             p(HTML("Nombre de stations : ", nrow(df_final), "<br>"),
               HTML("Nombre de velos disponibles : ", toString(sum(df_final$available_bikes)), "<br>"),
               HTML("Nombre de places de velos disponibles : ", toString(sum(df_final$available_bike_stands)))),
             fluidRow(
               column(3,
                      h5("borne de paiement présente?")),
               column(3,
                      radioButtons("radio", "",
                                   choices = list("Oui" = 1, "Non" = 2), selected = 1))),
             textOutput("statistics_text"),
             br(), br(), br(),
             h2("Tableau de données sur chaque station"),
             fluidRow(
               sidebarPanel(width = 12,
                            pickerInput(
                              inputId = "select_cp",
                              label = "Selectionnez des codes postaux",
                              choices = c("69001", "69002", "69003", "69004", "69005",
                                          "69006", "69007", "69008", "69009", "69100",
                                          "69110", "69120", "69130", "69140", "69150",
                                          "69160", "69190", "69200", "69230", "69250",
                                          "69270", "69300", "69310", "69350", "69370",
                                          "69450", "69500", "69600", "69660", "69800"),
                              selected = "69001",
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE
                            ),
                            textOutput("selected_postcodes"))),
             fluidRow(DTOutput("tableau"))),
    
    tabPanel("Graphiques",
             h2("Graphique à barres"),
             fluidRow(
               sidebarPanel(width = 12,
                            selectInput("variable", "Choisissez une variable",
                                        choices = c("nb total de stands", "nb de stands disponibles", "nb de vélos disponibles"),
                                        selected = "nb total de stands"),
                            plotOutput("top_10"))),
             br(), br(), br(),
             h2("Graphiques circulaires"),
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow(
                              selectInput("select_cp_graph", "Sélectionnez un code postal",
                                                choices = unique(df_final$postcode),
                                                selected = unique(df_final$postcode)[1]),
                            fluidRow(
                              column(6,
                                     plotOutput("tri_a_plat_plot")),
                              column(6,
                                     plotOutput("bike_vs_stands_pie")))))),
             
             br(), br(), br(),
             fluidRow(
               sidebarPanel(width = 12,
                            selectInput("download_png",
                                        "Télécharger un graphique :",
                                        choices = c("top 10", "tri_a_plat_plot", "bike_vs_stands_pie")),
                            downloadButton("downloadData", "Télécharger le graphique sélectionné")))
    )
  )
)



server <- function(input, output, session) {
  
# code pour rafraîchir la page
  observeEvent(input$bouton_reset, {
    session$reload()
  })
  
# code permettant de choisir si l'on veut voir les stations possédant une borne de paiement ou non
  observe({
    selected_radio <- input$radio
    output$statistics_text <- renderText({
      if (selected_radio == 1) {
        stats_text <- paste("Nombre de stations possédant une borne de paiement : ", nrow(df_final[df_final$banking == TRUE, ]))
      } else {
        stats_text <- paste("Nombre de stations ne possédant pas une borne de paiement : ", nrow(df_final[df_final$banking == FALSE, ]))
      }
      return(stats_text)
    })
  })
  
# code permettant de sauvegarder les codes postaux sélectionnés pour le tableau ci-après
  output$selected_postcodes <- renderText({
    paste("Codes postaux sélectionnés : ", paste(input$select_cp, collapse = ", "))
  })

# code permettant de montrer la base de données (df_final) selon le ou les codes postaux sélectionnés précédemment
  output$tableau <- renderDT({
    filtre_df <- df_final[df_final$postcode == input$select_cp, ]
    hidden_columns <- c("contract_name", "last_update")
    datatable(filtre_df, options = list(pagelength = 5,
                                        columnDefs = list(
                                          list(visible = FALSE,
                                               targets = hidden_columns))))
  })
  
# code permettant de sélectionner le titre de la variable y selon le choix de l'utilisateur (voir code suivant)
  ylab_text <- reactive({
    switch(input$variable,
           "nb total de stands" = "Nombre total de stands",
           "nb de stands disponibles" = "Nombre de stands disponibles",
           "nb de vélos disponibles" = "Nombre de vélos disponibles"
    )
  })

  output$top_10 <- renderPlot({
# choix de l'utilisateur sauvegardé
    y_variable <- switch(input$variable,
                         "nb total de stands" = df_final$bike_stands,
                         "nb de stands disponibles" = df_final$available_bike_stands,
                         "nb de vélos disponibles" = df_final$available_bikes)
    
    IDandCP <- aggregate(y_variable, by = list(Category = df_final$postcode), FUN = sum)

# l'ordre des variables x et le titre de la variable y change selon le choix sauvegardé
    top_n(x = IDandCP, n = 10) %>% 
      ggplot() + 
      geom_bar(mapping = aes(x = reorder(Category, x), 
                             y = x, 
                             fill = x), 
               stat = "identity", 
               show.legend = F) +
      ggtitle("Top 10 du Grand Lyon") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("") + ylab(ylab_text()) +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "red")
  })
  
# code permettant de créer le diagramme circulaire des stations bonus et non-bonus selon le code postal sélectionné
  output$tri_a_plat_plot <- renderPlot({
# création du tri à plat
    selected_cp <- input$select_cp_graph
    filtered_df <- df_final[df_final$postcode == selected_cp, ]
    tri_a_plat_filtered <- table(filtered_df$banking)
    
# création du diagramme circulaire
    pie(
      x = tri_a_plat_filtered, 
      main = paste("Part des stations bonus (TRUE) et non-bonus (FALSE)", "\npour le code postal", selected_cp),
      col = c(rouge = "lightpink", vert = "lightgreen"),
      labels = paste(rownames(tri_a_plat_filtered), tri_a_plat_filtered)
    )
  })

  
# code permettant de créer le diagramme circulaire des vélos disponibles selon le code postal sélectionné
  output$bike_vs_stands_pie <- renderPlot({
# création et stockage des valeurs
    selected_cp <- input$select_cp_graph
    filtered_df <- df_final[df_final$postcode == selected_cp, ]
    
    total_stands_dispo <- sum(filtered_df$available_bike_stands)
    total_velos_dispo <- sum(filtered_df$available_bikes)
    
    Labels <- paste(c("Vélos disponibles", "Places de vélo disponibles"), 
                    c(total_velos_dispo, total_stands_dispo))
    Colours <- c("lightgoldenrod1", "lightsalmon")
    
# création du diagramme circulaire
    pie(c(total_velos_dispo, total_stands_dispo), labels = Labels, col = Colours,
        main = paste("Proportion des vélos disponibles et des places de vélo disponibles", "\npour le code postal", selected_cp))
  })

# Download all graphs button
  observeEvent(input$download_graphs, {
    lapply(c("top_10", "tri_a_plat_plot", "bike_vs_stands_pie"), function(graph_name) {
      downloadHandler(
        filename = function() {
          paste(graph_name, ".png")
        },
        content = function(file) {
          ggsave(file, plot = output[[graph_name]], device = "png")
        }
      )(NULL, session)
    })
  })
  
  # Download individual graph button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$download_png, '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = output[[input$download_png]], device = "png")
    }
  )
  
  #   # Bouton de téléchargement
  #   # A RECTIFIER!!!  fichier téléchargé ne se télécharge pas, et est en HTML
#  output$downloadData <- downloadHandler(
#    filename = function() {
#      paste(input$download_png, '.png', sep = '')
#    }, 
#    content = function(file) {
#      ggsave(file, plot = plotInput(), device = "png")
#    }
#  )
  
#  output$downloadData <- downloadHandler(
#    filename = function() {
#      paste(input$download_png, ".png")
#    },
#    content = function(file) {
#      png(file)
#      if (input$download_png == "top 10") {
#        print(top_10)
#      } else if (input$download_png == "flop 10") {
#        print(flop_10)
#      } else if (input$download_png == "stations bonus") {
#        print(pie_chart)
#      }
#      dev.off()
#    }
#  )
   
#    output$downloadData <- downloadHandler(
#      filename = function() {
#        paste(input$download_png, ".csv", sep = "")
#      },
#      content = function(file) {
#        write.csv(dataDownload(), file, row.names = FALSE)
#      })
#       output$download_png <- downloadHandler(
#         filename = function() {
#           "pie_chart.png"
#         },
#         content = function(file) {
#           png(file)
#           print(output$pie_chart)  # Utilisez le graphique pie_chart ici   #        dev.off()
#         }) 
}



shinyApp(ui = ui, server = server)



if(!require("rsconnect")){
  install.packages("rsconnect")
}


rsconnect::setAccountInfo(name='azago-agrange-flabissy',
                          token='1B7895B4F26C7F979756064421EB6F43',
                          secret='8rJ7nZVOCCI2AAfLaNGcw+ApaiV1tdbbWpOus6Gk')

library(rsconnect)
rsconnect::deployApp("https://www.shinyapps.io/azago-agrange-flabissy/MyVeloApp")

