library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(NeuroDataSets)

# Load Data
data <- AD_biomarkers_tbl_df

# Rename Variables for better viewing
Mapper <- c(
  "Total Tau (pg/mL)" = "tau",
  "Thyroid Stimulating Hormone (log uIU/mL)" = "Thyroid_Stimulating_Hormone",
  "Transforming Growth Factor alpha (pg/mL)" = "TGF_alpha",
  "Cortisol (ng/mL)" = "Cortisol",
  "Myoglobin (log ng/mL)" = "Myoglobin",
  "Vascular Endothelial Growth Factor (pg/mL)" = "VEGF",
  "Complement C3 (log mg/mL)" = "Complement_3",
  "Follicle-Stimulating Hormone (ng/mL)" = "FSH_Follicle_Stimulation_Hormon",
  "Thrombopoietin (ng/mL)" = "Thrombopoietin",
  "IL-6 Receptor (ng/mL)" = "IL_6_Receptor",
  "Leptin (ng/mL)" = "Leptin",
  "Insulin (log uIU/mL)" = "Insulin",
  "Cognitive Status" = "Class",
  "Gender" = "male"
  )
data <- data %>% rename(all_of(Mapper))

# Define button mapping
Exposures = c("Total Tau (pg/mL)",
             "Thyroid Stimulating Hormone (log uIU/mL)",
             "Transforming Growth Factor alpha (pg/mL)",
             "Cortisol (ng/mL)",
             "Myoglobin (log ng/mL)",
             "Vascular Endothelial Growth Factor (pg/mL)",
             "Complement C3 (log mg/mL)",
             "Follicle-Stimulating Hormone (ng/mL)",
             "Thrombopoietin (ng/mL)",
             "IL-6 Receptor (ng/mL)",
             "Leptin (ng/mL)",
             "Insulin (log uIU/mL)"
                )

# Factor Gender Variable for mapping
data$Gender <-  factor(data$Gender, levels = c(0,1), labels = c("Female", "Male"))
            
            
# Redefine Genotype variable into risk level and factor
data$Genotype <-  as.character(data$Genotype)
for (i in 1:nrow(data)) {
  if (grepl("4", data$Genotype[i])) {
    data$Genotype[i] <- 2
  }
  else if (grepl("2", data$Genotype[i])) {
    data$Genotype[i] <- 0
  }
  else {
    data$Genotype[i] <- 1
  }
}
data$Genotype <- factor(data$Genotype, levels = c(0,1,2), labels = c("Decreased Risk", "Baseline Risk", "Increased Risk"))

# Define UI for application
ui <- fluidPage(
  layout_columns(
    card(
      radioButtons("exposure", "Select Biomarker:", choices = Exposures),
      checkboxInput("gender", "Factor by Gender?"),
      checkboxInput("genotype", "Factor by Genotypic Risk?"),
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
                                 Craig-Schapiro R, et al. Multiplexed immunoassay panel identifies novel CSF biomarkers for Alzheimer's disease diagnosis and prognosis. PLoS One. 2011 Apr 19;6(4):e18850. doi: 10.1371/journal.pone.0018850. PMID: 21526197; PMCID: PMC3079734.")
  simPlot <- eventReactive(
    input$start,
    {
      inputy <- input$exposure
      if (input$gender == TRUE) {
        if(input$genotype == TRUE) {
          data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(Gender ~ Genotype) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        }
        else {
          data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(Gender ~ .) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        }
      }
      else {
        if(input$genotype == TRUE) {
          data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(. ~ Genotype) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        }
        else {
          data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        }
      }
    }
   )
  output$results <- renderPlot(simPlot())
}

# Run the application 
shinyApp(ui = ui, server = server)
