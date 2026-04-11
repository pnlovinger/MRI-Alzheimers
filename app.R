library(shiny)
library(bslib)
library(ggplot2)
data <- read.csv("alzheimers_disease_data.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(
  layout_columns(
    card(
      input_task_button("start", "Start Fit"),
      textOutput("Citation")
      ),
    card(
      
    )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Citation <-  renderText("Citation:
                                 Rabie El Kharoua. (2024). 🧠 Alzheimer's Disease Dataset 🧠 [Data set]. 
                                 Kaggle. https://doi.org/10.34740/KAGGLE/DSV/8668279")
}

# Run the application 
shinyApp(ui = ui, server = server)
