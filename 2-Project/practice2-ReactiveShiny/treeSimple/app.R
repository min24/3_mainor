library(shiny)

library(caret)
library(partykit)

load("appData.Rdata")

ui <- fluidPage(
  titlePanel("Модель для предсказания статуса клиента"),
  sidebarLayout(
    sidebarPanel(
     p("Здесь выбор модели") 
      
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
    # предсказываем
    predTrain.tree = predict(model.tree, credit.train)
    predTest.tree = predict(model.tree, credit.test)
    # собираем таблицу
    accuracyTrain.tree = confusionMatrix(predTrain.tree, credit.train$Status, positive = "good")
    accuracyTest.tree = confusionMatrix(predTest.tree, credit.test$Status, positive = "good")
    getPrettyTable(accuracyTrain.tree, accuracyTest.tree)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

