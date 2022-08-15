library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Equipo 8"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficas de barras", tabName = "uno", icon = icon("chart-simple")),
      menuItem("Probabilidades marginales", tabName = "dos", icon = icon("chart-area")),
      menuItem("Resultados de partidos", tabName = "tres", icon = icon("table")),
      menuItem("Factores de ganancia", tabName = "cuatro", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      #Página 1
      tabItem(tabName="uno",
          h1("Gráficas de barras"),
          fluidRow(
            box(
              title = "Estas son las graficas Barra",
              plotOutput("grafico1", height = 550)   
            )
          )
      ),
      #Página 2
      tabItem(tabName="dos",
           h1("Gráficas de probabilidades marginales de anotar goles"),
           selectInput("x", "Seleccione el equipo que desea ver la probabilidad de anorta gol(es) ",
                       choices = c("Casa", "Visitante", "Conjuntas Casa-Visitante")),
           imageOutput("image2")
           #img(src = "POSTWORK3_1.png",
           #   height = 350, width = 550)
      ),
      #Página 3
      tabItem(tabName="tres",
          h1("Resultados de partidos por fecha"),
          dataTableOutput ("data_table")
      ),
      #Página 4
      tabItem(tabName = "cuatro",
          h1("Factores de ganancia promedio y máximo")
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
    datos<-read.csv("https://raw.githubusercontent.com/kotoromo/Proyecto-R-BEDU/main/match.data.csv")
    }, 
    options = list(aLengthMenu = c(5,10,15), iDisplayLength = 5)
  )

    
}
  
  #output$uno <- renderText("Hola" ) #Titulo del main Panel
  #output$dos <- renderText("Hola" )  
  #output$tres <- renderDataTable( {iris},       #Data table
  #                       options = list(aLengthMenu = c(10,20,50), iDisplayLength = 10) ) 
#  output$cuatro <- renderTable({ data.frame(iris)})   # Data Frame
  
 # }

shinyApp(ui, server)