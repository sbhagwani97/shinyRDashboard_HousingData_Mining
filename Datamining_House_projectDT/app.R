library(shiny)
library(caret)
library(rpart)
library(party)
# Define UI for application that draws a histogram

ui <- fluidPage(
   
   # Application title
   titlePanel("Decision Tree Classifier for California Housing Dataset"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("long",
                     "Longitude",
                     value = -120
                     ),
         numericInput("lat",
                     "Latitude",
                    
                     value = 39
                    ),
         numericInput("pop",
                     "Population",
                    
                     value = 30
                    ),
         numericInput("mhv",
                     "Median House Value",
                     
                     value = 10000
                     ),
         numericInput("hholds",
                     "House Holds",
                     
                     value = 300
                    )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("predClass"),
         tableOutput("accmeasures")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$predClass <- renderTable({
    setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
    data<-read.csv('housing.csv')
    set.seed(2)
    tree_model<-rpart(ocean_proximity~longitude+latitude+population+median_house_value+households, data=data)
    pred_element<-data.frame(input$long, input$lat, input$pop, input$mhv, input$hholds)
    colnames(pred_element) <- c("longitude","latitude","population","median_house_value","households")
    pred<-predict(tree_model, newdata=pred_element, type='class')
    pred
   })
  output$accmeasures <- renderTable({
    setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
    data<-read.csv('housing.csv')
    set.seed(2)
    id<-sample(2, nrow(data), prob=c(0.3,0.7), replace=T)
    data_train<-data[id==1,]
    data_test<-data[id==2,]
    library(caret)
    library(rpart)
    tree_model<-rpart(ocean_proximity~longitude+latitude+population+median_house_value+households, data=data_train)
    pred<-predict(tree_model, newdata=data_test, type='class')
    confusionMatrix(pred, data_test$ocean_proximity)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

