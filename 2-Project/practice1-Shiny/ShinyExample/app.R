#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Иерархическая кластеризация"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "clusters", label = "Выберите число кластеров", value = 3),
      selectInput(inputId = "method", label = "Выберите тип ссылки", 
                  choices = c("средние расстояния"="average", 
                              "метод ближнего соседа"="single", 
                              "метод дальнего соседа"="complete"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    Arrests <-USArrests
    scaledArrests <- scale(Arrests[,1:4])
    hc.complete=hclust(dist(scaledArrests), method=input$method)
    plot(hc.complete, xlab="", sub="", cex=.9)
    num = input$clusters
    rect.hclust(hc.complete, k = num, border = 2:(num+1))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
