library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(NeuroDataSets)
library(gtsummary)
library(effectsize)
library(rstatix)
library(coin)


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
  
    } else if (grepl("2", data$Genotype[i])) {
    data$Genotype[i] <- 0
  
    } else {
    data$Genotype[i] <- 1
  }
}
data$Genotype <- factor(data$Genotype, levels = c(0,1,2), labels = c("Decreased Risk", "Baseline Risk", "Increased Risk"))

auto_select_test <- function(df) {
  groups <- unique(df$group)
  if (length(groups) !=2) return ("wilcox")
  
  g1 <- df$value[df$group ==groups[1]]
  g2 <- df$value[df$group ==groups[2]]
  
  sw1 <- if(length(g1) >=3) shapiro.test(g1)$p.value else 0
  sw2 <- if(length(g2) >=3) shapiro.test(g2)$p.value else 0
  
  if(sw1 >0.05 && sw2 >0.05) "ttest" else "wilcox"
}
# Define UI for application
ui <- fluidPage(
  fluidRow(
    column(4,
      radioButtons("exposure", "Select Biomarker:", choices = Exposures),
      checkboxInput("gender", "Factor by Gender?"),
      checkboxInput("genotype", "Factor by Genotypic Risk?"),
      input_task_button("start", "Start Fit"),
      textOutput("Citation")
      ),
    column(8,
      plotOutput("results", height = "600"),
      br(),
      uiOutput("statsPanel")
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
      if (input$gender && input$genotype) {
          p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(Gender ~ Genotype) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        } else if (input$gender) {
          p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(Gender ~ .) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        } else if(input$genotype) { 
         p<- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            facet_grid(. ~ Genotype) +
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        } else {
         p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]])) + 
            stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
            stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
        }
      
      factors <- character(0)
        if (input$gender) factors <- c(factors,"Gender")
        if (input$genotype) factors <- c(factors,"Genotype")
      
      
      if  (length(factors) == 0) {
          return(list(plot = p, 
                      stats= NULL,
                      n_tests = 0,
                      n_wilcox = 0))
      }
      
      results_list <- lapply(unique(data$`Cognitive Status`), function(cs){
        cs_data <- data %>% filter(`Cognitive Status`== cs)
        
        lapply(factors,function(fac){
      
      df <- cs_data %>% 
        select(value = all_of(inputy), group =all_of(fac)) %>% 
        filter(!is.na(value), !is.na(group))
      
      gc <- df %>% count(group)
        if (nrow(gc) <2|| any(gc$n <2)) return(NULL)
      
      groups <- as.character(gc$group)
      
      if (length(groups) !=2) {
        pairs <- combn(groups,2, simplify =FALSE)
        rows <- lapply(pairs,function(pr){
          sub_df <-df %>% filter(as.character(group) %in% pr)
          sub_df$group <- droplevels(sub_df$group)
          res <-wilcox.test(value ~group, data=sub_df)
          eff <- as.numeric(wilcox_effsize(value ~group, data =sub_df)$effsize)
          data.frame(
            Factor =fac,
            `Cognitive Group`=cs,
            Comparison =paste(pr[1], "vs", pr[2]),
            Test ="Wilcoxon (pairwise)",
            n1 =sum(as.character(sub_df$group)==pr[1]),
            n2 =sum(as.character(sub_df$group)==pr[2]),
            statistic= round(res$statistic,3),
            p =res$p.value,
            effect= round(eff,3),
            `Effect Type` = "Rank-biserial r",
            check.names =FALSE,
            stringsAsFactors =FALSE
          )
        })
        return(bind_rows(rows))
      }
      
      chosen_test <- auto_select_test(df)
      
    if (chosen_test == "ttest") {
res <- t.test(value ~ group,data =df, var.equal = FALSE)
eff <- as.numeric(cohens_d(value ~ group,data =df)[[1]])
eff_label <- "Cohen's d"
test_label <- "Welch t-test (auto)"
} else{
res <-wilcox.test(value ~ group, data=df)
eff <- as.numeric(wilcox_effsize(value ~ group, data =df)$effsize)
eff_label <- "Rank-biserial r"
test_label<- "Wilcoxon (auto)"
}
      data.frame(
        Factor = fac,
        `Cognitive Group` = cs,
        Comparison = paste(groups[1], "vs",groups[2]),
        Test = test_label,
        n1 =gc$n[1],
        n2 =gc$n[2],
        statistic = round(res$statistic,3),
        p = res$p.value,
        effect = round(eff,3),
        `Effect Type` = eff_label,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
        })
      })
      
      clean <- Filter(Negate(is.null),do.call(c,results_list))
      
      if(length(clean) ==0) {
        return(list(plot     = p,
                    stats    = NULL,
                    n_tests  = 0,
                    n_ttest  = 0,
                    n_wilcox = 0))
      }
      
      results <-bind_rows(clean)
      
      results <- results %>% 
        mutate(p.adj =p.adjust(p,method = "BH"))
      
      results <- results %>% 
        mutate(
          Significance =case_when(
            p.adj <0.001 ~ "***",
            p.adj <0.01 ~ "**",
            p.adj <0.05 ~ "*",
            TRUE        ~ "ns"
          )
        )
      
      results <- results %>% 
        select(
          Factor,
          `Cognitive Group`,
          Comparison,
          Test,
          n1, n2,
          statistic,
          p,
          p.adj,
          Significance,
          effect,
          `Effect Type`
          )
      n_ttest <- sum(results$Test == "Welch t-test (auto)")
      n_wilcox <- sum(results$Test %in% c("Wilcoxon (auto)", "Wilcoxon (pairwise)"))
                     
      
     list (
       plot = p,
       stats =results,
       n_tests = nrow(results),
       n_ttest = n_ttest,
       n_wilcox = n_wilcox) 
      
    }) 
  
output$results <- renderPlot({
  req(simPlot())
  simPlot()$plot
   })

output$statsPanel <-renderUI({
  req(simPlot())
  res <-simPlot()
  
  
  
if (is.null(res$stats)) {
  return(p(class = "no-stats",
          "No stratification selected - tick Gender
           and/or Genotypic Risk to see statistics."))
}
  df <- res$stats
  
  meta <- tags$div(class = "stats-meta",
                   sprintf("Biomarker: %s | %d (test(s) run |%d Welch t-test | %d Wilcoxon| BH-adjusted p values",
                          input$exposure, res$n_tests, res$n_ttest, res$n_wilcox))
  tbl_rows <- apply (df,1, function(r){
    sig_class <-if (r[["Significance"]] =="ns") "sig-ns" else "sig-star"
    tags$tr(
      tags$td(r[["Factor"]]),
      tags$td(r[["Cognitive Group"]]),
      tags$td(r[["Comparison"]]),
      tags$td(r[["Test"]]),
      tags$td(r[["n1"]]),
      tags$td(r[["n2"]]),
      tags$td(r[["statistic"]]),
      tags$td(round(as.numeric(r[["p"]]), 4)),
      tags$td(round(as.numeric(r[["p.adj"]]), 4)),
      tags$td(class =sig_class, r[["Significance"]]),
      tags$td(r[["effect"]]),
      tags$td(r[["Effect Type"]])
    )
  })
  
  tbl <- tags$div(class = "table-responsive",
                  tags$table(class = "stats-table",
                      tags$thead(tags$tr(       
                      tags$th("Factor"), tags$th("Cognitive Group"), tags$th("Comparison"),
                      tags$th("Test"),tags$th("n1"), tags$th("n2"),
                      tags$th("Statistic"), tags$th("p"), tags$th("p.adj(BH)"),
                      tags$th("Sig."), tags$th("Effect Size"), tags$th("Effect Type")
                  )), 
        tags$tbody(tbl_rows)  
    )
  )
        tagList(
          tags$div(class = "stats-header", paste("Statistical Results -", input$exposure)),
        meta,
        tbl
      )
    })
}





# Run the application 
shinyApp(ui = ui, server = server)
