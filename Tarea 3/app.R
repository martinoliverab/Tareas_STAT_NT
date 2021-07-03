library(ggplot2)
library(tidyverse)
library(shiny)
library(DT)
propinas <- read_csv("propina.csv")
ui <- fluidPage(
  titlePanel("Propina"),
  sidebarLayout(
    sidebarPanel(
      selectInput('color_variable', 'Variable en color',c("sexo", "fuma", "dia", "momento")),
      selectInput('dig', 'Cantidad de decimales',c(0,1,2)),
      actionButton("runif", "Mostrar"),
      actionButton("reset", "Ocultar")),
    mainPanel(
      tabsetPanel(
        tabPanel("Bivar",h2("ScatterPlot", align = "center"),plotOutput("scatter" ),dataTableOutput("tab")),
      tabPanel("Univar",h2("Gráfico de Barras", align = "center"),plotOutput("bar"))))))
server <- function(input, output){
  datos <- reactiveValues()
  observe({datos$reactive_color_variable <- input$color_variable})
  observe({datos$reactive_color_variable <- input$dig})
  salida <- reactiveValues(data = NULL)
  observeEvent(input$runif, { 
    salida$plot1 <-ggplot(data = propinas,aes(x = total, y = propina,colour = .data[[input$color_variable]]))+geom_point() + theme(aspect.ratio = 1) +scale_x_continuous(name ="Total cuenta") +scale_y_continuous(name = "Propina")
    salida$plot2 <-ggplot(propinas, aes(x=.data[[input$color_variable]])) + geom_bar() + labs(y = "Cantidad")
    salida$tab <- reactive({as.data.frame(propinas) %>%group_by(datos$reactive_color_variable) %>%summarise(media_propina = mean(propina),sd_propina = sd(propina),media_total = mean(total),sd_total = sd(total)) %>% mutate(across(where(is.double),function (x) {round(x, digits = 3)})) %>%  datatable()})})
  observeEvent(input$reset, {
    salida$plot1 <- NULL
    salida$plot2 <- NULL
    salida$tab <- NULL})    
  output$scat <- renderPlot({
    if (is.null(salida$plot1)) 
      return()
    plot(salida$plot1)})  
  output$bar <- renderPlot({
    if (is.null(salida$plot2)) 
      return()
    plot(salida$plot2)})
  output$tab <- renderDT({
    if (is.null(salida$tab)) 
      return()
    print(salida$tab())})}
shinyApp(ui, server)
  




