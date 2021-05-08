library(ggplot2)
library(caret)
# 
# tr <- read.csv('train.csv')
# Intrain <- createDataPartition(y = tr$Response,   # outcome variable
#                           p = .30,   # % of training data you want
#                           list = F)
# # create your partitions
# tr <- tr[Intrain,]  # training data set
# write.csv(tr,"Final_train.csv")
# te <-read.csv('test.csv') 

# te$id <- NULL
set.seed(999)
tr <- read.csv('Final_train.csv')
tr$id <-NULL
tr[,1] <- NULL
s <- c('Gender','Vehicle_Age',"Vehicle_Damage","Driving_License",
       "Previously_Insured","Region_Code")

for (i in 1:length(s)){
  tr[,s[i]] <- as.factor(tr[,s[i]])
  # te[,s[i]] <- as.factor(te[,s[i]])
}
##EDA

#*** Plot for response by vehicle age
p1 <- ggplot(tr, aes(Vehicle_Age,fill =  as.factor(Response)))
# add the layer
p1 <- p1 + geom_bar() 
# adding lables
p1 <- p1 + labs(y = "No. of customers", 
                title = "Response by Vehicle Age ", 
                caption="Car age greater than 2 years and between 1-2 years has the best response rate.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5))
# view figure
p1

#*** Plot for response by customer age
p2 <- ggplot(tr, aes(Age, fill = as.factor(Response)))
# add the layer
p2 <- p2 + geom_histogram()
# adding lables
p2 <- p2 + labs(y = "No. of customers", 
                title = "Response by customer age", 
                caption="Age group 30-50 has the highest response rate.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5))
# view figure
p2

#*** Plot for response by vehicle vintage

p3 <- ggplot(tr, aes(Vintage, fill = as.factor(Response)))
# add the layer
p3 <- p3 + geom_histogram()
# adding lables
p3 <- p3 + labs(y = "No. of customers", 
                title = "Response by Vintage", 
                caption="There isn't significant change in response rate by vintage.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5)) 
# view figure
p3



#*** Plot for response by Gender?
#*
p4 <- ggplot(tr, aes(Gender,fill = as.factor(Response)))
# add the layer
p4 <- p4 + geom_bar() 
# adding lables
p4 <- p4 + labs(y = "No. of customers", 
                x = "Gender", 
                title = "Customer response by Gender", 
                caption="There isn't significant difference between response rate among Male and Female gender.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5))
# view figure
p4


#*** Plot for response by Region?
#*
p5 <- ggplot(tr, aes(Region_Code,fill = Response))
# add the layer
p5 <- p5 + geom_bar() 
# adding lables
p5 <- p5 + labs(y = "No. of customers", 
                x = "Region", 
                title = "Customer response by region", 
                caption= "Region 28 has the highest positive response.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5))
# view figure
p5


#*** Plot for Tendency to Reinsure?
#*
p6 <- ggplot(tr, aes(Previously_Insured,fill = Response))
# add the layer
p6 <- p6 + geom_bar() 
# adding lables
p6 <- p6 + labs(y = "No. of customers", 
                x = "Previously Insured", 
                title = "Customer response among new vs returning customers", 
                caption="Tendency to buy car insurance is higher amoung customers who have had insurance previously.",
                fill = "Response") + theme(plot.title = element_text(hjust = 0.5), 
                                           plot.subtitle = element_text(hjust = 0.5))
# view figure
p6
#####################################################################################################
# Rshiny App

library(shiny)
library(fmsb)
library(shinydashboard)
library(shinythemes)


ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Insurance Cross-sell Prediction System"),
    windowTitle = ("Insurance Cross-sell Prediction System"),
    # fluidRow(
    #   column(4,""),
    #   column(4, img(src = "Insurance_Logo.png",
    #                 height = 150, align = "center")),
    #   column(4,"")),
  sidebarLayout(
  sidebarPanel(h2("Enter Customer Inputs Below:"),
  
  sliderInput(inputId = "Age", 
              label = "Age", 
              value = 25, min = 1, max = 100), 
  sliderInput(inputId = "Annual_Premium", 
              label = "Premium Amount", 
              value = 2000, min = 1, max = 50000),
  selectInput(inputId = "Vehicle_Age", 
              label = "Vehicle Age",
              choices = unique(tr$Vehicle_Age),
              selected = 1),
  selectInput(inputId = "Gender", 
              label = "Gender",
              choices = unique(tr$Gender),
              selected = 1),
  selectInput(inputId = "Region_Code", 
              label = "Region Code",
              choices = unique(tr$Region_Code),
              selected = 1),
  sliderInput(inputId = "Policy_Sales_Channel", 
              label = "Policy Sales Channel",
              value = 124, min = 1, max = 500),
  selectInput(inputId = "Vehicle_Damage", 
              label = "Is the vehicle damaged?",
              choices = unique(tr$Vehicle_Damage),
              selected = 1),
  selectInput(inputId = "Previously_Insured", 
              label = "Previously insured",
              choices = unique(tr$Previously_Insured),
              selected = 1),
  selectInput(inputId = "Driving_License", 
              label = "Has a driving license?",
              choices = unique(tr$Driving_License),
              selected = 1),
  sliderInput(inputId = "Vintage", 
               label = "No. of days from Last insurance", 
               value = 185, min = 1, max = 5000),
  actionButton("update", "Get Predictions")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Trend Analysis", 
               plotOutput("p1",width = "100%", height = "400px"),
               plotOutput("p2",width = "100%", height = "400px"),
               plotOutput("p3",width = "100%", height = "400px"),
               plotOutput("p4",width = "100%", height = "400px")
      ),
      tabPanel("Insurance Sales Prediction", 
               fluidRow(
                 h4("The model predictions are:"),
                 tableOutput("Pred")),
               fluidRow(
                 h4("Note: A value of 0 in model prediction means 
                    that the model has predicted that the given customer will 
                    not buy the insurance and otherwise if it is 1."))
      )
      )
    )
  )
  )

server <- function(input, output) {
  output$p1 <- renderPlot({p1})
  output$p2 <- renderPlot({p2})
  output$p3 <- renderPlot({p3})
  output$p4 <- renderPlot({p4})
  
  values <- reactiveValues()
  values$df <- tr[1:1500,1:11]
  observe({
    if(input$update > 0) {
  newLine <- isolate(c(input$Gender,input$Age, input$Driving_License, 
                       input$Region_Code, input$Previously_Insured, input$Vehicle_Age,
                       input$Vehicle_Damage, input$Annual_Premium,
                       input$Policy_Sales_Channel,input$Vintage))
  
  isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
  
  d <- as.data.frame({values$df})
  str(d)
  # d<- as.h2o(d)
  source("Predict.R")
  
  p <- predict1(d)
  
  p<- p %>%rename('Probability of not buying Insurance' = 'p0',
                  'Probability of buying Inusrance' = 'p1', 'Model Prediction' = 'predict')
  
  output$Pred <- renderTable(tail(p,1), include.rownames=F)
    }
  })
  # output$table <- DT::renderDataTable({as.data.frame(rnorm(input$num, sd = input$stdev))})
  
}

shinyApp(ui = ui, server = server)



