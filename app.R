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
library(DT)


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
           DTOutput("statsPanel")
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
        p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]], fill = `Cognitive Status`)) + 
          facet_grid(Gender ~ Genotype) +
          stat_summary(fun = "mean", geom = "bar", colour = 'black') +
          stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2) + 
          theme(legend.position = "none", text = element_text(size = 15))
      } else if (input$gender) {
        p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]], fill = `Cognitive Status`)) + 
          facet_grid(Gender ~ .) +
          stat_summary(fun = "mean", geom = "bar", colour = 'black') +
          stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2) + 
          theme(legend.position = "none", text = element_text(size = 15))
      } else if(input$genotype) { 
        p<- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]], fill = `Cognitive Status`)) + 
          facet_grid(. ~ Genotype) +
          stat_summary(fun = "mean", geom = "bar", colour = 'black') +
          stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2) + 
          theme(legend.position = "none", text = element_text(size = 15))
      } else {
        p <- data %>% ggplot(aes(x = `Cognitive Status`, y = .data[[inputy]], fill = `Cognitive Status`)) + 
          stat_summary(fun = "mean", geom = "bar", colour = 'black') +
          stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2) + 
          theme(legend.position = "none", text = element_text(size = 15))
      }
      
      factors <- character(0)
      if (input$gender) factors <- c(factors,"Gender")
      if (input$genotype) factors <- c(factors,"Genotype")
      
      
      if  (length(factors) == 0) {
        df <-data %>% 
          select(value = all_of(inputy), group =`Cognitive Status`) %>% 
          filter(!is.na(value), !is.na(group))
        
        gc <- df %>% count(group)
        
        if(nrow(gc) <2 || any(gc$n <2)) {
          return(list(plot =p, stats= NULL, n_tests =0, n_ttest =0, n_wilcox=0))
        }
        groups <- as.character(gc$group)
        
        if (length(groups) ==2) {
          chosen_test <- auto_select_test(df)
          if (chosen_test == "ttest") {
            res   <- t.test(value ~group, data =df, var.equal =FALSE)
            eff   <- as.numeric(effectsize::cohens_d(value ~group, data =df)[[1]])
            eff_label <- "Cohen's d"
            test_label <- "Welch t-test (auto)"
          } else {
            res    <- wilcox.test(value ~group, data=df)
            eff    <- as.numeric(wilcox_effsize(value ~group, data =df)$effsize)
            eff_label <- "Rank-biserial r"
            test_label <- "Wilcoxon (auto)"
          }
          
          results <- data.frame(
            Factor = "None",
            Stratum = "All",
            Comparison = paste(groups[1], "vs", groups[2]),
            Test = test_label,
            n1 =gc$n[1],
            n2 =gc$n[2],
            statistic = round(res$statistic, 3),
            p = res$p.value,
            effect = round(eff, 3),
            `Effect Type` = eff_label,
            check.names =FALSE,
            stringsAsFactors = FALSE
          )
        } else {
          pairs <-combn(groups,2, simplify= FALSE)
          rows <- lapply(pairs, function(pr) {
            sub_df     <- df %>% filter(as.character(group)%in% pr)
            sub_df$group <- droplevels(sub_df$group)
            res <- wilcox.test(value ~ group, data =sub_df)
            eff    <- as.numeric(wilcox_effsize(value ~group, data= sub_df)$effsize)
            data.frame(Factor ="None", "Stratum" = "All",
                       Comparison =paste(pr[1], "vs", pr[2]),
                       Test ="Wilcoxon (pairwise)",
                       n1 =sum(as.character(sub_df$group) ==pr[1]),
                       n2 = sum(as.character(sub_df$group) ==pr[2]),
                       statistic = round(res$statistic,3),p =res$p.value,
                       effect = round(eff,3), `Effect Type` = "Rank-biserial r",
                       check.names =FALSE, stringsAsFactors = FALSE)
          })
          results <- bind_rows(rows)
        }
        results <- results %>% 
          mutate(p.adj =p.adjust(p, method ="BH"), 
                 Significance =case_when(
                   p.adj < 0.001 ~ "***", p.adj <0.01 ~ "**",
                   p.adj < 0.05 ~ "*", TRUE ~"ns"))
        return(list(plot=p, stats=results,n_tests =nrow(results),
                    n_ttest=sum(results$Test =="Welch t-test (auto)"),
                    n_wilcox=sum(results$Test %in% c("Wilcoxon (auto)", "Wilcoxon (pairwise)"))))
      }
      
      results_list <- if (length(factors) ==2){
        fac1 <- factors[1]
        fac2 <- factors[2]
        
        lapply(unique(data[[fac1]]), function(lev1) {
          lapply(unique(data[[fac2]]), function(lev2) {  
            
            fac_data <- data %>% filter(.data[[fac1]] ==lev1, .data[[fac2]] ==lev2)
            df <- fac_data %>% 
              select(value =all_of(inputy), group = `Cognitive Status`) %>% 
              filter(!is.na(value), !is.na(group))
            
            gc <- df %>% count(group)
            if (nrow(gc) <2|| any(gc$n <2)) return(NULL)
            
            groups <- as.character(gc$group)
            stratum_label <- paste(lev1, "+", lev2)
            factor_label <- paste(fac1, "+", fac2)
            
            if (length(groups)== 2) {
              chosen_test <- auto_select_test(df)
              if(chosen_test =="ttest") {
                res <- t.test(value ~group, data=df, var.equal =FALSE)
                eff <- as.numeric(effectsize::cohens_d(value ~ group, data=df)[[1]])
                eff_label <- "Cohen's d"
                test_label <- "Welch t-test (auto)"
              } else {
                res <- wilcox.test(value ~group, data=df) 
                eff <- as.numeric(wilcox_effsize(value ~group, data =df)$effsize)
                eff_label <- "Rank-biserial r"
                test_label <- "Wilcoxon (auto)"
              }
              
              return(data.frame(
                Factor = factor_label,
                Stratum =stratum_label,
                Comparison =paste(groups[1], "vs", groups[2]),
                Test = test_label,
                n1 =gc$n[1],
                n2= gc$n[2],
                statistic = round(res$statistic,3),
                p = res$p.value,
                effect =round(eff,3),
                `Effect Type` = eff_label,
                check.names = FALSE,
                stringsAsFactors =FALSE
              ))
            }
            pairs <- combn(groups,2, simplify =FALSE)
            rows <- lapply(pairs,function(pr){
              sub_df <-df %>% filter(as.character(group) %in% pr)
              sub_df$group <- droplevels(sub_df$group)
              res <-wilcox.test(value ~group, data=sub_df)
              eff <- as.numeric(wilcox_effsize(value ~group, data =sub_df)$effsize)
              data.frame(
                Factor =factor_label,
                Stratum = stratum_label,
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
            bind_rows(rows)
          })
        })
        
      } else {
        
        lapply(factors,function(fac) {
          lapply(unique(data[[fac]]),function(fac_level) {
            
            fac_data <- data %>% filter(.data[[fac]] == fac_level)
            df <- fac_data %>% 
              select(value = all_of(inputy), group = `Cognitive Status`) %>% 
              filter(!is.na(value), !is.na(group))
            
            gc <- df %>%  count(group)
            if(nrow(gc) <2 || any(gc$n <2)) return(NULL)
            
            groups <-as.character(gc$group)
            
            if(length(groups) ==2) {
              chosen_test <- auto_select_test(df)
              if(chosen_test == "ttest") {
                res <- t.test(value ~group, data=df, var.equal =FALSE)
                eff <- as.numeric(effectsize::cohens_d(value~group, data=df)[[1]])
                eff_label <-"Cohen's d"
                test_label <- "Welch t-test (auto)"
              } else {
                res <- wilcox.test(value ~group, data=df)
                eff <- as.numeric(wilcox_effsize(value ~group, data=df)$effsize)
                eff_label <- "Rank-biserial r"
                test_label <- "Wilcoxon (auto)"
                
              }
              
              return(data.frame( 
                Factor = fac, Stratum =as.character(fac_level),
                Comparison =paste(groups[1], "vs", groups[2]),
                Test =test_label, n1=gc$n[1], n2= gc$n[2],
                statistic =round(res$statistic,3), p= res$p.value,
                effect = round(eff,3), `Effect Type`= eff_label,
                check.names = FALSE, stringsAsFactors = FALSE
              ))
            }
            
            pairs <- combn(groups, 2, simplify = FALSE)
            rows <- lapply(pairs, function(pr) {
              sub_df <- df %>%  filter(as.character(group) %in% pr)
              sub_df$group <- droplevels(sub_df$group)
              res <- wilcox.test(value ~ group, data =sub_df)
              eff <- as.numeric(wilcox_effsize(value ~group, data =sub_df)$effsize)
              data.frame(
                Factor = fac, Stratum = as.character(fac_level),
                Comparison = paste(pr[1], "vs", pr[2]),
                Test = "Wilcoxon (pairwise)",
                n1 = sum(as.character(sub_df$group) ==pr[1]),
                n2 = sum(as.character(sub_df$group)==pr[2]),
                statistic = round(res$statistic,3), p= res$p.value,
                effect =round(eff,3), `Effect Type`= "Rank-biserial r",
                check.names =FALSE, stringsAsFactors = FALSE
              )
            })
            bind_rows(rows)
          })
        })
      }
      
      clean <- Filter(Negate(is.null),do.call(c,results_list))
      
      if(length(clean) ==0) {
        return(list(plot     = p,
                    stats    = NULL,
                    n_tests  = 0,
                    n_ttest  = 0,
                    n_wilcox = 0))
      }
      
      results <-bind_rows(clean) %>% 
        mutate(p.adj =p.adjust(p,method = "BH"),
               Significance =case_when(
                 p.adj <0.001 ~ "***",
                 p.adj <0.01 ~ "**",
                 p.adj <0.05 ~ "*",
                 TRUE        ~ "ns"
               )
        )%>% 
        
        select(Factor,Stratum, Comparison, Test, n1, n2,
               statistic, p, p.adj, Significance, effect, `Effect Type`)
      
      list (
        plot = p,
        stats =results,
        n_tests = nrow(results),
        n_ttest = sum(results$Test == "Welch t-test (auto)"),
        n_wilcox = sum(results$Test %in% c("Wilcoxon (auto)", "Wilcoxon (pairwise)"))
      )
    }) 
  
  output$results <- renderPlot({
    req(simPlot())
    simPlot()$plot
  })
  
  captionRender <- eventReactive(
    input$start, {
      res <-simPlot()
      htmltools::tags$caption(
        style = "caption-side:top; font-weight:bold; font-size:14px; color: #1e3a5f;",
        paste("Statistical Results -", input$exposure, "|",
              res$n_tests, "test(s) |",
              res$n_ttest, "Welch t-test |",
              res$n_wilcox, "Wilcoxon |BH-adjusted p-values") 
        )
    }
  )
  
  output$statsPanel <-renderDT({
    req(simPlot())
    res <-simPlot()
    if (is.null(res$stats)) return(NULL)
    
    df <- res$stats
    
    df$p_adj_num <- df$p.adj
    
    df$p <-ifelse(df$p < 0.001, "<0.001", formatC(df$p, digits =4, format = "f"))
    df$p.adj <- ifelse(df$p.adj <0.001, "<0.001", formatC(df$p.adj, digits =4, format ="f"))
    
    datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      caption = captionRender(),
      options=list(
        pageLength =10,
        scrollX = TRUE,
        dom = "tip",
        order = list(list(8, "asc")),
        columnDefs = list(
          list(className = "dt-center",
               targets = c(4,5,6,7,8,9,10)),
          list(visible = FALSE, targets = which(names(df) == "p_adj_num") - 1)
          )
      )
    ) %>% 
      formatStyle(
        "Significance",
        color = styleEqual(
          c("***", "**", "*", "ns"),
          c("#b91c1c", "#c2410c", "#ca8a04", "#6b7280")
        ),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        "p.adj",
        valueColumns = "p_adj_num",
        backgroundColor = styleInterval(
          c(0.001, 0.01, 0.05),
          c("#fee232", "#fef3c7","#fefce8", "white" )
        )
      )
    
  })
  
  
  
} # Run the application 
shinyApp(ui = ui, server = server)
