library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = "AI Tools Feedback Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-pie")),
      menuItem("Visualization", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Feedback Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    ## ---- CUSTOM CSS FOR DESIGN & CIRCLES ----
    tags$head(
      tags$style(HTML("
        
        /* Background */
        .content-wrapper {
          background: linear-gradient(to right, #8360c3, #2ebf91);
        }

        /* Card shadow */
        .box {
          border-radius: 15px;
          box-shadow: 0px 6px 15px rgba(0,0,0,0.2);
        }

        /* Circular value boxes */
        .circle-box {
          width: 180px;
          height: 180px;
          border-radius: 50%;
          text-align: center;
          padding-top: 35px;
          color: white;
          margin: auto;
          font-size: 20px;
          box-shadow: 0px 5px 12px rgba(0,0,0,0.3);
        }

        .circle-purple { background: #6a11cb; }
        .circle-green { background: #11998e; }
        .circle-yellow { background: #f7971e; }

        .circle-box i {
          font-size: 35px;
          margin-bottom: 10px;
        }

        h2,h3 {
          color: white;
          font-weight: bold;
        }
      "))
    ),
    
    tabItems(
      
      # PAGE 1 – Upload
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  width = 6, status = "primary", solidHeader = TRUE,
                  title = "Upload Dataset",
                  fileInput("file", "Upload CSV File", accept = ".csv")
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Instructions",
                  "Upload cleaned CSV file with columns:
                  AI_Tool and Rating"
                )
              )
      ),
      
      # PAGE 2 – Summary (CIRCULAR)
      tabItem(tabName = "summary",
              fluidRow(
                column(4, uiOutput("total_circle")),
                column(4, uiOutput("tool_circle")),
                column(4, uiOutput("rating_circle"))
              )
      ),
      
      # PAGE 3 – Visualization
      tabItem(tabName = "visual",
              fluidRow(
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "AI Tool Usage Count",
                  plotOutput("tool_plot", height = 300)
                )
              ),
              fluidRow(
                box(
                  width = 12, status = "warning", solidHeader = TRUE,
                  title = "Average Rating per AI Tool",
                  plotOutput("rating_plot", height = 300)
                )
              )
      ),
      
      # PAGE 4 – Table
      tabItem(tabName = "table",
              fluidRow(
                box(
                  width = 12, status = "primary", solidHeader = TRUE,
                  title = "Student Feedback Table",
                  DTOutput("feedback_table")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  ## ---- CIRCULAR SUMMARY UI ----
  
  output$total_circle <- renderUI({
    div(class = "circle-box circle-purple",
        icon("users"),
        h3(nrow(data())),
        "Total Feedback"
    )
  })
  
  output$tool_circle <- renderUI({
    div(class = "circle-box circle-green",
        icon("robot"),
        h3(length(unique(data()$AI_Tool))),
        "AI Tools Used"
    )
  })
  
  output$rating_circle <- renderUI({
    div(class = "circle-box circle-yellow",
        icon("star"),
        h3(round(mean(data()$Rating), 2)),
        "Avg Rating"
    )
  })
  
  # TOOL COUNT BAR CHART
  output$tool_plot <- renderPlot({
    ggplot(data(), aes(AI_Tool, fill = AI_Tool)) +
      geom_bar() +
      theme_minimal(base_size = 14) +
      labs(x = "AI Tool", y = "Usage Count") +
      theme(legend.position = "none")
  })
  
  # AVERAGE RATING CHART
  output$rating_plot <- renderPlot({
    df <- data() %>%
      group_by(AI_Tool) %>%
      summarise(Average_Rating = mean(Rating))
    
    ggplot(df, aes(AI_Tool, Average_Rating, fill = AI_Tool)) +
      geom_col() +
      theme_minimal(base_size = 14) +
      labs(x = "AI Tool", y = "Average Rating") +
      theme(legend.position = "none")
  })
  
  # DATA TABLE
  output$feedback_table <- renderDT({
    datatable(
      data(),
      options = list(pageLength = 5)
    )
  })
}

shinyApp(ui = ui, server = server)
