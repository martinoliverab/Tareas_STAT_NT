## Actividad 9 - Martín Olivera - CI 48454883

library(tidyverse)
library(shiny)
library(ggplot2)

## Parte 1

ui <- fluidPage(
    sliderInput(inputId = "cant",
                label = "tamaño muestral:",
                min = 1,
                max = 500,
                value = 30),
    plotOutput("hist")
)
server <- function(input, output){
    output$hist <- renderPlot({data <- data.frame(x = rnorm(input$cant)) %>% ggplot(aes(x)) + geom_histogram(binwidth = 0.5)
    print(data)
    })
}
shinyApp(ui, server)

## Parte 2

ui <- fluidPage(
    sliderInput(inputId = "cant",
                label = "tamaño muestral:", 
                min = 1, max = 500,
                value = 30),
    numericInput(inputId = 'media', 
                 label="Media", 
                 value = 0),
    numericInput(inputId = 'desvio', 
                 label = "SD", 
                 value = 1),
    plotOutput("hist")
)
server <- function(input, output){
    output$hist <- renderPlot({
        data2 <- data.frame(x = rnorm(input$cant, mean = input$media, sd = input$desvio))
        data <- data2 %>% ggplot(aes(x)) + geom_histogram(binwidth = 0.5)
        print(data)
    })
}
shinyApp(ui, server)


## Parte 3


ui <- fluidPage(
    sliderInput(inputId = "cant",
                label = "tamaño muestral:", min = 1, max = 500,
                value = 30), 
                selectInput('distri',
                'Distribución', c("Gamma","Normal")),
    plotOutput("hist")
)
server <- function(input, output){
    output$hist <- renderPlot({
        if (input$distri == "Normal") {
            data3 <- data.frame(x = rnorm(input$cant))
            data <- data3 %>%
                ggplot(aes(x)) + geom_histogram(binwidth = 0.5)
            print(data)
        }else{
            data4 <- data.frame(x = rgamma(n = input$cant, shape = 1 ))
            data <- data4 %>%
                ggplot(aes(x) ) + geom_histogram(binwidth = 0.5)
            print(data)
        }
    })
}
shinyApp(ui, server)