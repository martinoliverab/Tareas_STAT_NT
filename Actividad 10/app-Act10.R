## Actividad 10 - Martín Olivera - CI 48454883

library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Actividad 10"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "cant", label = "Tamaño muestral:", min = 1, max = 100, value = 50),
            selectInput(inputId = 'distri', label = 'Distribucion', c("Normal", "Gamma", "Ambas")),
            numericInput(inputId = 'bins', label = "Cantidad de Bins", min=1, value=10),
            img(src = "shiny.png", height = 100, width = 100)),
        mainPanel(
            tabsetPanel(
                tabPanel("Histograma", plotOutput("hist")),
                tabPanel("Plot",
                         fluidRow(
                             column(5,plotOutput('plot1')),
                             column(5,plotOutput('plot2'))
                         ),
                         fluidRow(
                             column(5,plotOutput('plot3')),
                             column(5,plotOutput('plot4')))),
                tabPanel("Resumen"),
                tabPanel("Tabla")
            )
        )
    )
)
server <- 
    function(input, output) { 
        
        datos <- reactive(
            if (input$distri=="Gamma") {rgamma(input$cant, shape=1)} else
                if (input$distri=="Normal") {rnorm(input$cant)} else {
                    gamma<-rgamma(input$cant, shape=1)
                    normal<-rnorm(input$cant)
                    gamma_normal<-data.frame(value=c(gamma, normal),tipo=c(rep("gamma", times=input$cant), rep("normal", times=input$cant)))
                })
        
        grafico <- reactive(
            if (input$distri=="Gamma" | input$distri=="Normal"){graf<-data.frame(x=datos()) %>% ggplot(aes(x)) + geom_histogram(bins=input$bins)
            graf} else {
                graf<-datos() %>% ggplot(aes(x=value)) + geom_histogram(bins=input$bins) + facet_wrap(vars(tipo))
                graf})
        
        
        output$hist <- renderPlot({grafico()})
        
        output$plot1 <- renderPlot({plot(grafico())})
        
        output$plot2 <- renderPlot({plot(grafico())})
        
        output$plot3 <- renderPlot({plot(grafico())})
        
        output$plot4 <- renderPlot({plot(grafico())})
        
    }

shinyApp(ui, server)

# Saqué el print para que queden bien conectadas las tabsetPanel
# Puntaje 9/10