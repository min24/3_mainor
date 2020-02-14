library(shiny)

library(caret)
library(partykit)

load("appDataNew.Rdata")

ui <- fluidPage(
  titlePanel("Модель для предсказания статуса клиента"),
  sidebarLayout(
    sidebarPanel(
     selectInput("mod",
                 "Выберите модель",
                 choices = c("дерево", "логистическая регрессия")) 
      
    ),
    mainPanel(
      tableOutput("metrics"),
      plotOutput("tree")
    )
  )
)

getPrettyTable = function(accuracyTrain, accuracyTest){
  accTrain = accuracyTrain$overall["Accuracy"]
  otherTrain = accuracyTrain$byClass[c("Precision", "Recall", "F1")]
  train.tree = c(accTrain, otherTrain)
  accTest = accuracyTest$overall["Accuracy"]
  otherTest = accuracyTest$byClass[c("Precision", "Recall", "F1")]
  test.tree = c(accTest, otherTest)
  results = rbind(train.tree, test.tree)
  rownames(results) = c("Обучающая", "Тестовая")
  
  # не забываем последней строчкой вывести таблицу
  results
}

server <- function(input, output) {
  
  output$metrics <- renderTable({ 
    if (input$mod == "дерево"){
      currentModel = model.tree
    }else if(input$mod == "логистическая регрессия"){
      currentModel = model.log
    }
    # предсказываем
    predTrain.tree = predict( currentModel, credit.train)
    predTest.tree = predict( currentModel, credit.test)
    # собираем таблицу
    accuracyTrain.tree = confusionMatrix(predTrain.tree, credit.train$Status, positive = "good")
    accuracyTest.tree = confusionMatrix(predTest.tree, credit.test$Status, positive = "good")
    getPrettyTable(accuracyTrain.tree, accuracyTest.tree)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

