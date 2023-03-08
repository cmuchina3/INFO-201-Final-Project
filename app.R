library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(ggpp)


dataset <- read_delim("nces330_20.csv")
Datarows <- dataset %>% 
  select(Year, State, Type, Length, Expense, Value)
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
              tabPanel("Plot3",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "state",
                                       label = "Select a State:",
                                       choices = unique(dataset$State),
                                       selected = "Alabama"),
                           selectInput(inputId = "type",
                                       label = "Select a Type of Institution:",
                                       choices = unique(dataset$Type),
                                       selected = "Private")
                         ),
                         mainPanel(
                           plotOutput("cost_plot"),
                         )
                       )
              ),
              tabPanel("Conclusion",p("After exploring the data set, we find out 
                                      a few interesting observations:"),
                       br(),
                       p("Firstly, the average cost continuously increased each
                         year for each state starting from 2013, and there's a 
                         especially rapid increase after 2020. Secondly, the average
                         cost for District of Columbia appers to be the highest 
                         among the states, and the one for New York appears to be
                         the second highest. The average cost for New Jersey appers
                         to be the third highest, which is just a little bit higher
                         than that for California."),
                       br(),
                       p("Besides the average costs comparing between the states,
                         we can also see that among different types of schools,
                         the private ones have the highest cost, and the public 
                         in-state schools have the lowest cost, and is significantly
                         lower than the other two options."),
                       br(),
                       p("For a broader implications of the insight, the rapid increase
                         of the average cost between 2020 and 2021 might be due to 
                         the COVID-19 which had a huge effect on the global economy. 
                         However, the cost might stop increasing that fast given a
                         improvement presenting in the condition of the pendamic, but
                         it's highly possible that the average costs will still 
                         increase according to previous trends illustrated by the 
                         data set."),
                       br(),
                       p("Besides the trend of an increasing cost, we can also see
                         that the four highest cost all appeared in states or cities
                         that are highly developed, and the cost to maintain basic 
                         life requirement are high enough according to our knowledge. 
                         As a result, we can also conclude that the average cost of
                         schools is also depended on where the school is located."),
                       br(),
                       p("For the quality of the data, we think that the quality of 
                         this data set is highly resonable, this includes a range of 
                         values starting from 2013 to 2021, which covers a large
                         intervale. Also, this data set covers average costs from
                         different types of schools in different states, so I think 
                         this data gives us unbiased results, and I don't see any
                         issues with potentially harming certain population groups."),
                       br(),
                       p("Finally, if the projected can be further developed, I think
                         we can find a more detailed data set, which includes more
                         aspects of an average cost, or we can find a data set with
                         a longer interval, which will cover a larger range of 
                         values, and we can make plots which might help us see
                         a more detailed trend in the change of the average costs."))
  ))


##dataset <- read.csv("./nces330_20.csv", header = TRUE)

# Define server logic required
server <- function(input, output) {
  
  output$table1 <- renderTable(head(dataset[!duplicated(dataset$State),]))
  
  output$cost_plot <- renderPlot({
    filtered_data <- dataset %>% 
      filter(State == input$state, Type == input$type)
    ggplot(filtered_data, aes(x = Length, y = Value, fill = Expense)) +
      geom_col(position = "dodge") +
      ggtitle(paste0("Average Cost of Education in ", input$state, " (", input$type, ")")) +
      xlab("Length of Program") +
      ylab("Average Cost (USD)") +
      scale_fill_manual(values = c("#619CFF", "#FF619C")) +
      theme_minimal()
  })
  
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
