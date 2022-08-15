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
                                   title = "Histograma Basico",
                                   plotOutput("grafico1", height = 550)   
                               
                                 )
                               )
                                ),# del TAB1
                      tabPanel(name="dos", "Imagenes del Post work 3"),
                      tabPanel(name="tres", "Datos del fichero"),
                      tabPanel(name="cuatro", "I,agenes de Factores")
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

  
  
  output$uno <- renderText("Hola" ) #Titulo del main Panel
  output$dos <- renderText("Hola" )  
  output$tres <- renderDataTable( {iris},       #Data table
                         options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) ) 
  output$cuatro <- renderTable({ data.frame(iris)})   # Data Frame
  
  }

shinyApp(ui, server)