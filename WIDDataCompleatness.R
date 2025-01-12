################################################################################
################################################################################
##################  Shiny App  ###############################################
################################################################################
################################################################################

#library(ggplot2)
library(plotly)
library(shiny)
#library(openxlsx)
#library(stringr) 
#library(purrr)

#--------------------------  Functions -----------------------------------------
# 4. Function to Set the height of the heatmap with variating widcodes or p
get_plot_height <- function(data, base_height = 300, row_height = 10) {
  # Define the possible columns
  possible_columns <- c("widcode", "p", "year")
  # Find which column exists in the data
  available_column <- possible_columns[possible_columns %in% colnames(data)]
  # Ensure exactly one column is available
  if (length(available_column) != 1) {
    stop("Exactly one of 'widcode', 'p', or 'year' must be present in the data.")
  }
  # Extract unique values from the available column
  n <- length(unique(data[[available_column]]))
  # Calculate and return the height
  return(base_height + n * row_height)
}

# 5. Function to Set the height of the heatmap with variating isos
get_plot_height2 <- function(data, base_height = 300, row_height = 10) {
  n <- nrow(unique(data[, "iso", drop = FALSE]))  # Count unique ISO codes
  return(base_height + n *  row_height)
}


# 6. Funciton for generting conditions for multiple  group selections
build_condition_string <- function(col_name4) {
  # Generate the condition string for each element in col_name4
  conditions <- paste0("get('", col_name4, "') == 1")
  
  # Combine all conditions with the '&' operator
  condition_string <- paste(conditions, collapse = " | ")
  
  return(condition_string)
}

#------------------------ App -------------------------------------------------


ui <- fluidPage(
  tags$style(HTML("
    .sidebar { 
      background-color: #28a745; /* Green background */
      color: white; /* White text for better contrast */
    }
    .sidebar .form-group, .sidebar label {
      color: white; /* Ensure text within the sidebar is white */
    }
    .top-controls {
      margin-bottom: 20px; /* Add spacing between controls and heatmaps */
    }
  ")),
  
  titlePanel(paste("Observation count in", file_name, ".dta")),
  
  # Sidebar layout with input and main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      # Shared input for Country Group
      selectInput("Country_group", "Country Group:", choices = region_columns),
      # Shared input for Widcode group
      checkboxGroupInput("F_group", "Fivelet Group for Widcodes:", choices = f_columns,
                         selected = f_columns[6:6]),
      # Shared input for P group
      checkboxGroupInput("P_group", "Fractile P group:", choices = p_columns,
                         selected = p_columns[1:1]),
      
    ),
    mainPanel(
      width = 9,
      div(
        class = "top-controls",
        # Top controls outside of sidebar layout
        fluidRow(
          column(
            width = 9, 
            sliderInput("year_range", "Select Year Range:",
                        min = min(data$year), max = max(data$year),
                        value = c(min(data$year), max(data$year)),
                        step = 10)
          ),
          column(
            width = 3, 
            selectInput("W_group", "Index Group for Widcodes:", choices = w_columns)
          )
        ),
        fluidRow(
          plotlyOutput("heatmap_year", height = "auto", width = "100%")
        ),
        fluidRow(
          plotlyOutput("heatmap_p", height = "auto", width = "100%")
        ),
        fluidRow(
          plotlyOutput("heatmap_w", height = "auto", width = "100%")
        )
      )
    )
  )
)


# Server function
server <- function(input, output) {
  
  # Data filteres
  filtered_data <- reactive({
    req(input$Country_group, input$P_group, input$W_group, input$F_group)
    col_name1 <- input$Country_group
    col_name2 <- input$P_group
    col_name3 <- input$W_group
    col_name4 <- input$F_group
    
    condition_string <- build_condition_string(col_name4)
    condition_string2 <- build_condition_string(col_name2)
    
    # Filter data
    subset(data, year >= input$year_range[1] & year <= input$year_range[2] &
             get(col_name1) == 1 & eval(parse(text = condition_string)) &
             get(col_name3) == 1 & eval(parse(text = condition_string2)))
  })
  
  # Count the observations in the dataset for each combination of iso and year
  countsy <- reactive({
    filtered_data() %>%
      group_by(iso, year) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  countsp <- reactive({
    filtered_data() %>%
      group_by(iso, p) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  countsw <- reactive({
    filtered_data() %>%
      group_by(iso, widcode) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  # Reactive function for filtered gridy
  # 1. grid Year
  filtered_gridy <- reactive({
    col_name1 <- input$Country_group
    subset(gridy, year >= input$year_range[1] & year <= input$year_range[2] & 
             get(col_name1) == 1 )
  })
  # 1. grid p
  filtered_gridp <- reactive({
    col_name1 <- input$Country_group
    col_name2 <- input$P_group
    condition_string2 <- build_condition_string(col_name2)
    subset(gridp, get(col_name1) == 1  & eval(parse(text = condition_string2)))
  })
  # 1. grid widcode
  filtered_gridw <- reactive({
    #req(input$Country_group, input$W_group, input$F_group) # Ensure inputs are non-null
    col_name1 <- input$Country_group
    col_name3 <- input$W_group
    col_name4 <- input$F_group
    condition_string3 <- build_condition_string(col_name3)
    condition_string4 <- build_condition_string(col_name4)
    subset(gridw, get(col_name1) == 1 & eval(parse(text = condition_string3)) 
                  & eval(parse(text = condition_string4)))
  })
  
  
  # Merge the grid with the counts to ensure all combinations are included
  resulty <- reactive({
    filtered_gridy() %>%
      left_join(countsy(), by = c("iso", "year")) %>%
      replace_na(list(count = NA))
  })
  
  resultp <- reactive({
    filtered_gridp() %>%
      left_join(countsp(), by = c("iso", "p")) %>% # First join with countsp()
      replace_na(list(count = NA)) %>%            # Handle unmatched rows
      left_join(p_orders, by = "p")               # Second join with p_orders
  })
  
  resultw <- reactive({
    filtered_gridw() %>%
      left_join(countsw(), by = c("iso", "widcode")) %>%
      replace_na(list(count = NA))
  })

  # Render the first heatmap
  output$heatmap_year <- renderPlotly({
    datay <- resulty()
    plot_height <- get_plot_height2(datay)
    plot_ly(
      data = datay,
      x = ~year,
      y = ~iso,
      z = ~count,
      type = "heatmap",
      colors = c( "steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Country: %{y}<br>",
        "Year: %{x}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Year",
        yaxis = list(title = "ISO Alpha-2"),
        plot_bgcolor = "crimson",       # Set the plot background color to red
        height = plot_height  # Apply dynamic height
      )
  })
  
  # Render the second heatmap
  output$heatmap_p <- renderPlotly({
    datap <- resultp()
    datap <- datap[order(datap$order_p), ]
    plot_height <- get_plot_height(datap)
    plot_ly(
      data = datap,
      x = ~iso,
      y = ~p,
      z = ~count,
      type = "heatmap",
      colors = c("steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Country: %{x}<br>",
        "P: %{y}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Fractile_Group",
        xaxis = list(title = "ISO Alpha-2"),
        yaxis = list(
          title = "P",
          categoryorder = "array",  # Force custom order
          categoryarray = datap$p  # Custom order for 'p' based on the data
        ),
        plot_bgcolor = "crimson", 
        height = plot_height  # Apply dynamic height
      )
  })
  
  # Render the third heatmap
  output$heatmap_w <- renderPlotly({
    dataw <- resultw()
    plot_height <- get_plot_height(dataw)
    plot_ly(
      data = dataw,
      x = ~iso,
      y = ~widcode,
      z = ~count,
      type = "heatmap",
      colors = c("steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Country: %{x}<br>",
        "Widcode: %{y}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Widcode_Group",
        yaxis = list(title = NULL),
        plot_bgcolor = "crimson", 
        height = plot_height  # Apply dynamic height
      )
  })
  

}

# Run the app
shinyApp(ui = ui, server = server)