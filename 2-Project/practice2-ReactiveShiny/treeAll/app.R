library(shiny)

library(dplyr)
library(caret)
library(partykit)

credit = read.csv("~/shared/minor3_2019/1-MachineLearning/practice-04-recap-ensembles/CreditScore.csv")

ui <- fluidPage(
  titlePanel("Модель для предсказания статуса клиента"),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed",
                   "Установите set.seed",
                   min = 1,
                   value = 1325),
      selectInput("varnames",
                  "Выберите переменные для анализа",
                  choices = names(credit)[-1],  #удаляем Status из списка выбора
                  selected = c("Seniority"),
                  multiple = TRUE),   # для выбора нескольких элементов списка
      selectInput('model',
                  'выбор модели', 
                  choices = c('tree', 'logistic'),
                  multiple = F,
                  selected = 'tree')
      
    ),
    mainPanel(
      tableOutput("metrics"),
      plotOutput("tree")
    )
  )
)

server <- function(input, output) {
  
  dataSelected <- reactive({
    dplyr::select(credit, c(input$varnames, "Status")) 
  })
  
  trainTest = reactive({
    # обратите внимание: вместо currentData мы вызываем dataSelected()
    currentData = dataSelected()
    set.seed(input$seed)
    test_ind = createDataPartition(currentData$Status, p = 0.2, list = FALSE)
    test_ind # это наш результат -- индексы для разделения
  })
  
  modeling <- reactive({
    data = dataSelected()
    credit.test = data[trainTest(),]
    credit.train = data[-trainTest(),]
    if (input$model == 'tree'){
    model.tree = ctree(Status~., data = credit.train)
    model.tree} 
    else{
    model.tree = train(Status~., data = credit.train, method = "glm", family = binomial(link = "logit"))
    model.tree
    }
  })
  
  output$metrics <- renderTable({
   
     # т.к. у нас нет прямого доступа к credit.train и credit.test, то разбиваем еще раз
    data = dataSelected()
    credit.test = data[trainTest(),]
    credit.train = data[-trainTest(),]
    # предсказываем
    predTrain.tree = predict(modeling(), credit.train)
    predTest.tree = predict(modeling(), credit.test)
    # собираем таблицу
    accuracyTrain.tree = confusionMatrix(predTrain.tree, credit.train$Status, positive = "good")
    accuracyTest.tree = confusionMatrix(predTest.tree, credit.test$Status, positive = "good")
    accTrain = accuracyTrain.tree$overall["Accuracy"]
    otherTrain = accuracyTrain.tree$byClass[c("Precision", "Recall", "F1")]
    train.tree = c(accTrain, otherTrain)
    accTest = accuracyTest.tree$overall["Accuracy"]
    otherTest = accuracyTest.tree$byClass[c("Precision", "Recall", "F1")]
    test.tree = c(accTest, otherTest)
    results = rbind(train.tree, test.tree)
    rownames(results) = c("Обучающая", "Тестовая")
    
    # не забываем последней строчкой вывести таблицу
    results
      
      
      
      

  })
  
  output$tree <- renderPlot({ 
    plot(modeling())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

