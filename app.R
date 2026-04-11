library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load Data
data <- read.csv("alzheimers_disease_data.csv")

# Rename Variables for better viewing
Mapper <- c(
  "Physical Activity in hours" = "PhysicalActivity",
  "Diet Quality" = "DietQuality",
  "Sleep Quality" = "SleepQuality",
  "Family History of Alzheimer's" = "FamilyHistoryAlzheimers",
  "Systolic Blood Pressure" = "SystolicBP",
  "Diastolic Blood Pressure" = "DiastolicBP",
  "Mini Mental State Evaluation" = "MMSE",
  "Total Cholesterol" = "CholesterolTotal",
  "Memory Complaints" = "MemoryComplaints",
  "Behavioral Problems" = "BehavioralProblems",
  "Personality Changes" = "PersonalityChanges",
  "Alzheimer's Diagnosis" = "Diagnosis"
  )
data <- data %>% rename(all_of(Mapper))

# Define button mapping
Outcomes = c("Age","BMI","Physical Activity in hours", "Diet Quality", 
                "Sleep Quality", "Systolic Blood Pressure", 
                "Diastolic Blood Pressure","Total Cholesterol"
                )
Exposures = c("Family History of Alzheimer's", "Memory Complaints", 
                 "Behavioral Problems", "Confusion", "Disorientation",
                 "Personality Changes", "Alzheimer's Diagnosis"
                 )

#Factor Exposure Variables appropiately
data <- data %>% mutate(across(Exposures, 
                        ~factor(., levels = c(0,1), labels = c("No", "Yes"))))
                        

# Define UI for application
ui <- fluidPage(
  layout_columns(
    card(
      radioButtons("exposure", "Exposure Variable:", choices = Exposures),
      radioButtons("outcome", "Outcome Variable", choices = Outcomes),
      input_task_button("start", "Start Fit"),
      textOutput("Citation")
      ),
    card(
      plotOutput("results")
    )
    )
  )

# Define server logic required
server <- function(input, output) {
  output$Citation <-  renderText("Citation:
                                 Rabie El Kharoua. (2024). 🧠 Alzheimer's Disease Dataset 🧠 [Data set]. 
                                 Kaggle. https://doi.org/10.34740/KAGGLE/DSV/8668279")
  simPlot <- eventReactive(
    input$start,
    ggplot(data, aes(x = Age, y = BMI)) + geom_point()
    )
  output$results <- renderPlot(simPlot())
}

# Run the application 
shinyApp(ui = ui, server = server)
