library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(ggpp)


# Define UI for application 
ui <- fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("Landing",
                       p(strong("The purpose of the project: "), 
                         "We hope that this project can help high school ",
                         "students who will choose undergraduate universities ",
                         "to know the tuition fees of various undergraduate ",
                         "universities, which is very important for students ",
                         "to choose."),
                       #br() element to introduce extra vertical spacing
                       br(),
                       p(strong("The source of the data: "),
                         "This app uses the average cost of undergraduate ", 
                         "college data from ", strong("Kaggle"),
                         p("https://www.kaggle.com/datasets/kfoster150/avg-cost-of-undergrad-college-by-state")),
                       br(),
                       p("Here is a small (random) sample of data:"),
                       br(),
                       tableOutput("table1")
              ),
              tabPanel("Plots",
                       sidebarLayout(
                         sidebarPanel(
                           p("You can analyze the  the average cost of ", 
                             "undergraduate college for different states. ", 
                             "Select the states you are interested in. You ",
                             "will see a monthly scatterplot and the ", 
                             "corresponding trend lines."),
                           br(),
                           br(),
                           checkboxInput(inputId = 'trend',
                                         label = 'Display trend(s)',
                                         value = FALSE),
                           br(),
                           radioButtons("color","Palette",
                                        c("Standard"="std","Set 2"="Set2")),
                           br(),
                           checkboxGroupInput("variable", 
                                              "Select the state(s) to display",
                                              c("Alabama","Alaska","Arizona",
                                                "Arkansas","California",
                                                "Colorado","Connecticut",
                                                "Delaware","District of Columbia",
                                                "Florida","Georgia","Hawaii",
                                                "Idaho","Illinois","Indiana",
                                                "Iowa","Kansas","Kentucky",
                                                "Louisiana","Maine","Maryland",
                                                "Massachusetts","Michigan",
                                                "Minnesota","Mississippi",
                                                "Missouri","Montana","Nebraska",
                                                "Nevada","New Hampshire",
                                                "New Jersey","New Mexico",
                                                "New York","North Carolina",
                                                "North Dakota","Ohio","Oklahoma",
                                                "Oregon","Pennsylvania",
                                                "Rhode Island","South Carolina",
                                                "South Dakota","Tennessee",
                                                "Texas","Utah","Vermont",
                                                "Virginia","Washington",
                                                "West Virginia","Wisconsin",
                                                "Wyoming"))
                         ),
                         # Show a plot of the generated distribution
                         mainPanel(
                           textOutput("total"),
                           plotOutput("Plot"),
                           
                         ))),
              tabPanel("Tables",
                       sidebarLayout(
                         sidebarPanel(
                           p("This panel shows the average value of different ",
                           "years/states/types/lengths/expenses."),
                           br(),
                           radioButtons("average","Average over:",
                                        c("Year"="year","State"="state",
                                          "Type"="type","Length"="length",
                                          "Expense"="expense"))
                         ),
                         mainPanel(
                           textOutput("range"),
                           tableOutput("table2")
                         ))),
              
              tabPanel("Plots2",
                         sidebarLayout(
                           sidebarPanel(
                             p("This panel shows the average price line chart of ",
                               "different years."),
                             br(),
                             radioButtons("color2","Palette",
                                          c("Standard"="std","Set 2"="Set2")),
                           ),
                           mainPanel(
                             plotOutput("Plot2")
                           ))),
              tabPanel("Conclusion",p("xxxxx"))
  ))


dataset <- read.csv("./nces330_20.csv", header = TRUE)

# Define server logic required
server <- function(input, output) {
  
  output$table1 <- renderTable(head(dataset[!duplicated(dataset$State),]))
  
  output$Plot <- renderPlot({
    data3 <- dataset %>% filter(State %in% input$variable)
    p <- ggplot(data3, aes(Year, Value))+
      geom_point(aes(color=State)) +
      labs(x="Year",y="Value",
           color = "State")
    if (input$trend){
      p <- p + geom_smooth(method = 'lm', formula = y ~ x, se = F)
    }
    if (input$color=="Set2"){
      p <- p + scale_color_brewer(palette = "Set2")}
    p
  })
  
  
  
  
  output$total <- renderText({
    df <- dataset %>% filter(State %in% input$variable)
    sum <- nrow(df)
    paste("Time period 2013 - 2021 . In total ",as.character(sum),
          "non-missing observations")
  })
  
  
  output$table2 <- renderTable(list({
    
    if (input$average=="year") {
      data <- dataset %>% group_by(Year) %>% summarize(mean=mean(Value))
    }
    if (input$average=="state") {
      data <- dataset %>% group_by(State) %>% summarize(mean=mean(Value))
    }
    if (input$average=="type") {
      data <- dataset %>% group_by(Type) %>% summarize(mean=mean(Value))
    }
    if (input$average=="length") {
      data <- dataset %>% group_by(Length) %>% summarize(mean=mean(Value))
    }
    if (input$average=="expense") {
      data <- dataset %>% group_by(Expense) %>% summarize(mean=mean(Value))
    }
    data
  }))
  
  output$range <- renderText({
    if (input$average=="year") {
      data <- dataset %>% group_by(Year) %>% summarize(mean=mean(Value))
    }
    if (input$average=="state") {
      data <- dataset %>% group_by(State) %>% summarize(mean=mean(Value))
    }
    if (input$average=="type") {
      data <- dataset %>% group_by(Type) %>% summarize(mean=mean(Value))
    }
    if (input$average=="length") {
      data <- dataset %>% group_by(Length) %>% summarize(mean=mean(Value))
    }
    if (input$average=="expense") {
      data <- dataset %>% group_by(Expense) %>% summarize(mean=mean(Value))
    }
    max <- max(data$mean)
    min <- min(data$mean)
    paste("Value range ",as.character(min),as.character(max))
  })
  
  output$Plot2 <- renderPlot({
    data4 <- dataset %>% group_by(Year) %>% summarize(mean=mean(Value))
    p <- ggplot(data4, aes(Year, mean))+
      geom_line() +
      labs(x="Year",y="Mean Value")
    if (input$color=="Set2"){
      p <- p + scale_color_brewer(palette = "Set2")}
    p
  }) 
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
