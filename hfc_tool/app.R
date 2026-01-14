#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

# UI
ui <- fluidPage(
  titlePanel("HFC Emissions Scoping Tool"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", 
                  "Select Country:", 
                  choices = sort(unique(edgar$name)),
                  selected = NULL),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Table", 
                 br(),
                 tableOutput("timeseries_table")),
        tabPanel("Emissions Over Time", 
                 br(),
                 plotOutput("line_plot", height = "600px")),
        tabPanel("Application Breakdown", 
                 br(),
                 plotOutput("treemap_app", height = "600px")),
        tabPanel("Sub-Application Breakdown", 
                 br(),
                 plotOutput("treemap_subapp", height = "600px"))
      ),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data processing
  scoping_data <- reactive({
    req(input$country)
    
    edgar %>%
      left_join(applications, by = "sub_application") %>%
      filter(name == input$country) %>%
      mutate(co2_eq = gwp * hfc_emissions) %>%
      group_by(year, application, sub_application) %>%
      summarize(mmt_co2_eq = sum(co2_eq, na.rm = TRUE) / 10^6, .groups = "drop") %>%
      ungroup()
  })
  
  # Create consistent color palette for applications
  app_colors <- reactive({
    req(scoping_data())
    apps <- unique(scoping_data()$application)
    n_apps <- length(apps)
    colors <- viridis(n_apps, option = "D")
    names(colors) <- apps
    colors
  })
  
  # Table output
  output$timeseries_table <- renderTable({
    scoping_data() %>%
      arrange(year, application, sub_application)
  })
  
  # Line plot
  output$line_plot <- renderPlot({
    data_for_plot <- scoping_data() %>%
      group_by(year, application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop")
    
    ggplot(data_for_plot, aes(x = year, y = mmt_co2_eq, color = application, group = application)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = app_colors()) +
      labs(
        title = paste("HFC Emissions Over Time -", input$country),
        x = "Year",
        y = "Emissions (MMT CO₂ equivalent)",
        color = "Application"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank()
      )
  })
  
  # Tree map by application
  output$treemap_app <- renderPlot({
    most_recent_year <- max(as.numeric(as.character(scoping_data()$year)), na.rm = TRUE)
    
    data_for_treemap <- scoping_data() %>%
      filter(year == most_recent_year) %>%
      group_by(application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop") %>%
      filter(mmt_co2_eq > 0)
    
    ggplot(data_for_treemap, aes(area = mmt_co2_eq, fill = application, label = application)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", size = 15, grow = TRUE) +
      scale_fill_manual(values = app_colors()) +
      labs(
        title = paste("HFC Emissions by Application -", input$country, "(", most_recent_year, ")"),
        fill = "Application"
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
      )
  })
  
  # Tree map by sub-application
  output$treemap_subapp <- renderPlot({
    most_recent_year <- max(as.numeric(as.character(scoping_data()$year)), na.rm = TRUE)
    
    data_for_treemap <- scoping_data() %>%
      filter(year == most_recent_year) %>%
      group_by(application, sub_application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop") %>%
      filter(mmt_co2_eq > 0)
    
    ggplot(data_for_treemap, aes(area = mmt_co2_eq, fill = application, 
                                 label = sub_application, subgroup = application)) +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "white", size = 3) +
      geom_treemap_text(colour = "white", place = "centre", size = 12, grow = TRUE) +
      scale_fill_manual(values = app_colors()) +
      labs(
        title = paste("HFC Emissions by Sub-Application -", input$country, "(", most_recent_year, ")"),
        fill = "Application"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
