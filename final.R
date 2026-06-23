if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)
if (!require("factoextra")) install.packages("factoextra", dependencies = TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)
if (!require("DT")) install.packages("DT", dependencies = TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(reshape2)
library(DT)

ui <- fluidPage(
  titlePanel(div("Startup Profit Predictor (Auto Filter by State)", 
                 style = "color:#0073e6; font-weight:bold")),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV Dataset", accept = c(".csv")),
      uiOutput("featureVarsUI"),
      uiOutput("stateSelectUI"),
      hr(),
      numericInput("clusters", "Number of clusters:", 3, min = 1, max = 10),
      actionButton("analyze", "Run Analysis", class = "btn btn-primary"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", DTOutput("dataTable"), verbatimTextOutput("summaryStats")),
        tabPanel("Correlation & Plots", plotOutput("corrPlot", height = "400px"), uiOutput("scatterUI"), plotOutput("scatterPlot")),
        tabPanel("Regression Model", verbatimTextOutput("regSummary"), plotOutput("predPlot")),
        tabPanel("Clustering", plotOutput("clusterPlot", height = "400px"), DTOutput("clusterTable")),
        tabPanel("Predict Profit", 
                 uiOutput("predictInputsFiltered"), 
                 actionButton("predictBtn", "Predict", class = "btn btn-success"), 
                 verbatimTextOutput("prediction"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df <- df %>%
      mutate(across(where(function(x) length(unique(x)) < 10 && !is.numeric(x)), as.factor))
    return(df)
  })
  
  output$featureVarsUI <- renderUI({
    req(dataset())
    choices <- setdiff(names(dataset()), "Profit")
    checkboxGroupInput("featureVars", "Select Predictor Variables (X):", choices = choices)
  })
  
  output$stateSelectUI <- renderUI({
    req(dataset(), input$featureVars)
    df <- dataset()
    if ("State" %in% input$featureVars && "State" %in% names(df)) {
      selectInput("selectedState", "Select a State for Filtering:", 
                  choices = unique(df$State), selected = unique(df$State)[1])
    }
  })
  
  output$dataTable <- renderDT({
    req(dataset())
    datatable(head(dataset(), 15), options = list(scrollX = TRUE))
  })
  
  output$summaryStats <- renderPrint({
    req(dataset())
    df <- dataset()
    numeric_cols <- df %>% select(where(is.numeric))
    stats <- lapply(numeric_cols, function(x) {
      c(Mean = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        Mode = { ux <- unique(x); ux[which.max(tabulate(match(x, ux)))] })
    })
    print(stats)
  })
  
  output$corrPlot <- renderPlot({
    req(dataset())
    df <- dataset()
    num_cols <- df %>% select(where(is.numeric))
    if (ncol(num_cols) < 2) {
      plot.new(); title("Not enough numeric columns for correlation"); return()
    }
    corr <- cor(num_cols, use = "complete.obs")
    melted <- melt(corr)
    ggplot(melted, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      labs(title = "Correlation Heatmap", x = "", y = "")
  })
  
  output$scatterUI <- renderUI({
    req(input$featureVars)
    selectInput("xvar", "Select X variable for scatter:", choices = input$featureVars)
  })
  
  output$scatterPlot <- renderPlot({
    req(dataset(), input$xvar)
    df <- dataset()
    targetVar <- "Profit"
    if (is.numeric(df[[input$xvar]]) && is.numeric(df[[targetVar]])) {
      ggplot(df, aes_string(x = input$xvar, y = targetVar)) +
        geom_point(color = "#0073e6") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        theme_minimal()
    } else {
      plot.new(); title("Selected variables are not numeric")
    }
  })
  
  model <- eventReactive(input$analyze, {
    req(input$featureVars)
    df <- dataset()
    targetVar <- "Profit"
    
    if ("State" %in% input$featureVars && !is.null(input$selectedState)) {
      df <- df %>% filter(State == input$selectedState)
      showNotification(paste("Model trained only on data from:", input$selectedState))
    }
    
    formula <- as.formula(paste(targetVar, "~", paste(setdiff(input$featureVars, "State"), collapse = " + ")))
    lm(formula, data = df)
  })
  
  output$regSummary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$predPlot <- renderPlot({
    req(model(), dataset())
    df <- dataset()
    targetVar <- "Profit"
    
    if ("State" %in% input$featureVars && !is.null(input$selectedState)) {
      df <- df %>% filter(State == input$selectedState)
    }
    
    df$pred <- predict(model(), newdata = df)
    ggplot(df, aes(x = pred, y = df[[targetVar]])) +
      geom_point(color = "#0073e6") +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      theme_minimal() +
      labs(title = "Predicted vs Actual", x = "Predicted", y = "Actual")
  })
  
  output$clusterPlot <- renderPlot({
    req(input$clusters, dataset())
    df <- dataset() %>% select(where(is.numeric))
    if (ncol(df) < 2) {
      plot.new(); title("Need at least two numeric columns for clustering"); return()
    }
    scaled_df <- scale(df)
    km <- kmeans(scaled_df, centers = input$clusters)
    fviz_cluster(km, data = scaled_df, geom = "point", ellipse.type = "norm") +
      labs(title = paste("K-Means Clustering (k =", input$clusters, ")"))
  })
  
  output$clusterTable <- renderDT({
    req(input$clusters, dataset())
    df <- dataset()
    num_df <- df %>% select(where(is.numeric))
    scaled_df <- scale(num_df)
    km <- kmeans(scaled_df, centers = input$clusters)
    df$Cluster <- km$cluster
    datatable(df, options = list(scrollX = TRUE))
  })
  
  output$predictInputsFiltered <- renderUI({
    req(input$featureVars, dataset())
    df <- dataset()
    vars <- setdiff(input$featureVars, "State")  # exclude State input
    lapply(vars, function(var) {
      if (is.numeric(df[[var]])) {
        numericInput(paste0("inp_", var), label = var, value = mean(df[[var]], na.rm = TRUE))
      } else {
        selectInput(paste0("inp_", var), label = var, choices = unique(df[[var]]))
      }
    })
  })
  
  output$prediction <- renderPrint({
    req(model())
    input$predictBtn
    isolate({
      df <- dataset()
      targetVar <- "Profit"
      
      if ("State" %in% input$featureVars && !is.null(input$selectedState)) {
        df <- df %>% filter(State == input$selectedState)
      }
      
      vars <- setdiff(input$featureVars, "State")
      newdata <- as.data.frame(lapply(vars, function(var) {
        val <- input[[paste0("inp_", var)]]
        if (is.factor(df[[var]])) factor(val, levels = levels(df[[var]]))
        else as.numeric(val)
      }))
      names(newdata) <- vars
      
      pred <- predict(model(), newdata)
      cat("Predicted Profit for", input$selectedState, "=", round(pred, 2))
    })
  })
}

shinyApp(ui, server)
