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
                      tabPanel(name="dos", "Imagenes del Post work 3",
                               
                               fluidRow(
                                 titlePanel("Gráficas resultado del Postwork 3"), 
                                 selectInput("x", "Selecciona la gráfica que desea ver",
                                             choices = c("1", "2", "3")),
                                 
                                 imageOutput("image2")
                                 #img(src = "POSTWORK3_1.png", 
                                  #   height = 350, width = 550)
                               )

                               ),
                      tabPanel(name="tres", "Datos del fichero"),
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
    
    if (input$x == "1") {
      return(list(
        src = "www/1.png",
        contentType = "image/png",
        alt = "Grafica Resultado 1"
      ))
    } else if (input$x == "2") {
      return(list(
        src = "www/2.png",
        filetype = "image/png",
        alt = "Grafica Resultado 2"
      ))
    }
    
    else if (input$x == "3") {
      return(list(
        src = "www/3.png",
        filetype = "image/png",
        alt = "Grafica Resultado 3"
      ))
    }
  
  
  
  
  })

  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #output$uno <- renderText("Hola" ) #Titulo del main Panel
  #output$dos <- renderText("Hola" )  
  #output$tres <- renderDataTable( {iris},       #Data table
  #                       options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) ) 
#  output$cuatro <- renderTable({ data.frame(iris)})   # Data Frame
  
 # }

shinyApp(ui, server)