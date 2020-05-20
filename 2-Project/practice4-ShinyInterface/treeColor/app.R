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
                  multiple = TRUE  # для выбора нескольких элементов списка
                  ),  
      sliderInput("border", 
                  label = "Выберите границы предупреждения",
                  min = 1, # не может быть меньше 1 переменной
                  max = ncol(credit)-1, # всего количество переменных в датасете
                  value = c(2, 8) # по уолчанию ставлю значения 2 и 8
                  )
      
    ),
    mainPanel(
      tableOutput("metrics"),
      htmlOutput("text")
    )
  )
)

server <- function(input, output) {
  
  output$metrics <- renderTable({ 
    # выбираем переменные и добавляем Status к данным
    currentData = dplyr::select(credit, c(input$varnames, "Status")) 
    # разделяем на тестовую-обучающую, заменив credit на currentData, и указав set.seed, выбранный пользователем
    set.seed(input$seed)
    test_ind = createDataPartition(currentData$Status, p = 0.2, list = FALSE)
    credit.test = currentData[test_ind,]
    credit.train = currentData[-test_ind,]
    # строим модель 
    model.tree = ctree(Status~., data = credit.train)
    # предсказываем
    predTrain.tree = predict(model.tree, credit.train)
    predTest.tree = predict(model.tree, credit.test)
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
  
  output$text <- renderUI({
    num = length(input$varnames) # чтобы не повторять много раз в коде
    textToDisplay = paste0("Выбрано переменных: ", num)
    
    # число выбранных переменных меньше левой границы слайдера или больше правой
    if (num < input$border[1] | num > input$border[2]) 
      p(textToDisplay, style = 'color:red')
    else 
      p(textToDisplay)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

