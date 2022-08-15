library(shiny)
library(shinydashboard)

ui <- dashboardPage( 
  skin = "blue",
  dashboardHeader(title = "Equipo 8"),
  dashboardSidebar(),
  
  
  
  
  
  dashboardBody(
           
    
    
     mainPanel(
              tabsetPanel(type = c("pills"), id = "hidden_tabs",
                      tabPanel(name="uno", "Gráfica de barras",
                               
                               fluidRow(
                                  box(
                                   title = "Estas son las graficas Barra",
                                   plotOutput("grafico1", height = 550)   
                               
                                 )
                               )
                                ),# del TAB1
                      tabPanel(name="dos", "Probabilidades marginales",
                               
                               fluidRow(
                                 titlePanel("Gráficas de probabilidades marginales de anotar goles"), 
                                 selectInput("x", "Seleccione el equipo que desea ver la probabilidad de anorta gol(es) ",
                                             choices = c("Casa", "Visitante", "Conjuntas Casa-Visitante")),
                                 
                                 imageOutput("image2")
                                 #img(src = "POSTWORK3_1.png", 
                                  #   height = 350, width = 550)
                               )

                               ),
                      tabPanel(name="tres", "Resultados de partidos",
                               
                               
                               tabItem(tabName = "data_table",
                                       fluidRow(        
                                         titlePanel(h3("Resultados de partidos por fecha")),
                                         dataTableOutput ("data_table")
                                             )
                                       )
                               
                              ),
                      tabPanel(name="cuatro", "Imagenes de Factores")
                        )
                      )
                 )
         )

server <- function(input, output) { 
  
  output$grafico1 <- renderPlot({
    if (is.null(input$maximo) || is.null(input$color))
      return()
    visual(input$maximo,input$color)
    
  })

  
  output$image2 <- renderImage({
    
    if (is.null(input$x))
      return(NULL)
    
    if (input$x == "Casa") {
      return(list(
        src = "www/1.png",
        contentType = "image/png",
        alt = "Grafica Resultado 1"
      ))
    } else if (input$x == "Visitante") {
      return(list(
        src = "www/2.png",
        filetype = "image/png",
        alt = "Grafica Resultado 2"
      ))
    }
    
    else if (input$x == "Conjuntas Casa-Visitante") {
      return(list(
        src = "www/3.png",
        filetype = "image/png",
        alt = "Grafica Resultado 3"
      ))
    }
  
  
  })

  
  output$data_table <- renderDataTable( {
    
    #Aqui hay que cambiar para que lea el archivo en web
    #datos<-read.csv("D:/BEDU/Curso ciencia de datos/Fase 2/Proyecto Final R Eq 8/match.data.csv")
    datos<-read.csv("/home/nicky/WorkingDirectory/Proyecto-R-BEDU/match.data.csv")
    
    }, 
                                        options = list(aLengthMenu = c(5,10,15),
                                                       iDisplayLength = 5)
  )
  
  
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #output$uno <- renderText("Hola" ) #Titulo del main Panel
  #output$dos <- renderText("Hola" )  
  #output$tres <- renderDataTable( {iris},       #Data table
  #                       options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) ) 
#  output$cuatro <- renderTable({ data.frame(iris)})   # Data Frame
  
 # }

shinyApp(ui, server)