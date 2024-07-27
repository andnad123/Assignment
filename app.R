library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
      }
      .title-panel {
        background-color: #4CAF50;
        color: white;
        padding: 10px 20px;
        border-radius: 5px;
      }
      .sidebar {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0px 0px 10px 0px #aaaaaa;
      }
      .main-panel {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0px 0px 10px 0px #aaaaaa;
      }
    "))
  ),
  
  div(class = "title-panel",
      titlePanel("Flow Cytometry Data Visualization")),
  
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 fileInput("file", "Upload Excel File with 2 columns (the first with the group and the second with values)", accept = c(".xlsx")),
                 textInput("plotTitle", "Plot Title", value = "Bar Plot of Flow Cytometry Data"),
                 textInput("xAxisLabel", "X-axis Label", value = "Genotype"),
                 textInput("yAxisLabel", "Y-axis Label", value = "MFI"),
                 textInput("legendTitle", "Legend Title", value = "Group")
    ),
    
    mainPanel(class = "main-panel",
              plotOutput("histogramPlot")
    )
  )
)
server <- function(input, output, session) {
  flow_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    return(df)
  })
  
  output$histogramPlot <- renderPlot({
    df <- flow_data()
    req(df)
    
    groupVar <- names(df)[1]
    valueVar <- names(df)[2]
    
    aggregated_df <- df %>%
      group_by(!!sym(groupVar)) %>%
      summarise(
        mean_value = mean(!!sym(valueVar)),
        sem_value = sd(!!sym(valueVar)) / sqrt(n())
      )
    
    ggplot(aggregated_df, aes_string(x = groupVar, y = "mean_value", fill = groupVar)) +
      geom_col(width = 0.4, position = "dodge", color = "black", size = 0.5) +  
      geom_errorbar(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value), 
                    width = 0.2, position = position_dodge(0.4), color = "black") +
      theme_classic(base_size = 14) +
      labs(title = input$plotTitle,
           x = input$xAxisLabel,
           y = input$yAxisLabel) +
      scale_fill_manual(name = input$legendTitle, values = c("WT" = "#00BFC4", "KO" = "#F8766D")) +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
  })
}
shinyApp(ui = ui, server = server)