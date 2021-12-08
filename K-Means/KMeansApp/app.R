library(shiny)

ui <- fluidPage(
   
   titlePanel("K-Means Based on House Value and Number of Households"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("kval",
                     "Choose number of clusters to visualise",
                     min = 2,
                     max = 5,
                     value = 3,
                     step=1)
      ),
      
      mainPanel(
         plotOutput("clusterplot")
      )
   )
)

server <- function(input, output) {
  setwd("C:/Users/SHIVAM BHAGWANI/Desktop/Winter Sem 2018-19/Data Mining/Data Mining Project")
  data<-read.csv("housing.csv")
  data$ocean_proximity<-NULL
  data<-na.omit(data)
   output$clusterplot <- renderPlot({
     results<-kmeans(data, input$kval)
     plot(data[c("median_house_value", "households")], col=results$cluster)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

