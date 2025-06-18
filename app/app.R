library(shiny)
library(bslib)
library(querychat)
library(DT)
library(dplyr)
library(lubridate)
library(leaflet)
library(shinydashboard)
library(stringr)

source("generate-data.R")

data_description <- 
  readLines("txt/data_description.txt", warn = FALSE) %>% 
  paste(collapse = "\n")

greeting <- 
  readLines("txt/greeting.txt", warn = FALSE) %>% 
  paste(collapse = "\n")

extra_instructions <- 
  readLines("txt/extra_instructions.txt", warn = FALSE) %>% 
  paste(collapse = "\n")

format_for_display <- function(df) {
  df_display <- df %>%
    select(
      `Equipment ID` = equipment_id,
      `Product Line` = model_series,
      `Specific Model` = specific_model,
      `Category` = category,
      `Year Mfg` = year_manufactured,
      `Annual Hours` = annual_hours,
      `Total Hours` = total_hours,
      `Fuel Efficiency (mpg)` = fuel_efficiency,
      `Customer Satisfaction` = customer_satisfaction,
      `Purchase Price ($K)` = purchase_price_k,
      `State` = state,
      `County` = county,
      `Region` = region,
      `Customer ID` = customer_id,
      `Dealer ID` = dealer_id,
      `Intro Year` = intro_year,
      `Latitude` = latitude,
      `Longitude` = longitude
    ) %>%
    mutate(
      `Annual Hours` = format(`Annual Hours`, big.mark = ","),
      `Total Hours` = format(`Total Hours`, big.mark = ","),
      `Purchase Price ($K)` = paste0("$", format(`Purchase Price ($K)`, big.mark = ","))
    )
  
  df_display
}

querychat_config <- querychat_init(
  equipment_data,
  tbl_name = "equipment",
  greeting = greeting,
  data_description = data_description,
  extra_instructions = extra_instructions,
  create_chat_func = purrr::partial(
    ellmer::chat_anthropic, 
    model = "claude-sonnet-4-20250514"
  )
)

ui <- page_sidebar(
  title = "John Deere Product Line Analytics",
  theme = bs_theme(brand = "_brand.yml"),
  sidebar = querychat_sidebar("chat", width = 450),
  tags$head(
    tags$style(HTML("
      /* Fix DT filter dropdowns */
      .form-select, .form-control {
        background-color: white !important;
        color: #333 !important;
      }
      
      /* Fix shinydashboard value boxes */
      .small-box h3, .small-box .inner h3 {
        color: white !important;
        font-weight: bold !important;
      }
      
      .small-box p, .small-box .inner p {
        color: white !important;
      }
      
      /* Alternative value box selectors */
      .info-box-number, .info-box-text {
        color: white !important;
      }
      
      /* Round valueBox edges */
      .small-box {
        border-radius: 10px !important;
      }
    "))
  ),
  div(
    class = "alert alert-warning",
    style = "margin-bottom: 5px; border-radius: 8px;",
    tags$strong("Note: "), 
    "This application displays fictional data and is for demonstration purposes only."
  ),
  layout_columns(
    card(
      card_header("Equipment Locations"),
      leafletOutput("equipment_map", height = "400px")
    ),
    div(
      style = "padding: 15px;",
      valueBoxOutput("total_products", width = 12),
      br(),
      valueBoxOutput("avg_satisfaction", width = 12),
      br(),
      valueBoxOutput("total_fleet_size", width = 12),
      br(),
      valueBoxOutput("avg_market_share", width = 12)
    ),
    col_widths = c(9, 3)
  ),
  card(
    card_header(textOutput("current_title")),
    DTOutput("equipment_table")
  )
)

server <- function(input, output, session) {
  querychat <- querychat_server("chat", querychat_config)
  
  output$current_title <- renderText({
    title <- querychat$title()
    if (is.null(title) || title == "") {
      "Product Line Performance Data"
    } else {
      title
    }
  })
  
  output$equipment_table <- renderDT({
    datatable(
      format_for_display(querychat$df()),
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        columnDefs = list(
          list(width = '120px', targets = c(0)),  # Equipment ID
          list(width = '150px', targets = c(1, 2)),  # Product Line, Specific Model
          list(width = '100px', targets = c(3)),  # Category
          list(width = '80px', targets = c(4, 5, 6, 7, 8)),  # Performance metrics
          list(width = '120px', targets = c(9)),  # Purchase Price
          list(className = 'dt-center', targets = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17))  # Center most columns
        )
      ),
      filter = 'top',
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle(
        'Category',
        backgroundColor = styleEqual(
          c('Combine', 'Tractor', 'Track Tractor'),
          c('#e6f3ff', '#fff3e6', '#f0e6ff')
        )
      ) %>%
      formatStyle(
        'Customer Satisfaction',
        backgroundColor = styleInterval(c(3.0, 3.5, 4.0, 4.5), c('#f8d7da', '#fff3cd', '#d4edda', '#c3e6cb', '#a3d977'))
      )
  })
  
  output$equipment_map <- renderLeaflet({
    df <- querychat$df()
    
    # Color by product category
    category_colors <- c("Combine" = "blue", "Tractor" = "green", "Track Tractor" = "purple")
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,
        color = ~category_colors[category],
        popup = ~paste0(
          "<b>", equipment_id, "</b><br/>",
          "Product Line: ", model_series, "<br/>",
          "Model: ", specific_model, "<br/>",
          "Category: ", category, "<br/>",
          "Year: ", year_manufactured, "<br/>",
          "Customer Satisfaction: ", customer_satisfaction, "<br/>",
          "Fuel Efficiency: ", fuel_efficiency, " mpg<br/>",
          "State: ", state, ", ", county
        ),
        radius = 6,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLegend(
        "bottomright",
        colors = c("blue", "green", "purple"),
        labels = c("Combine", "Tractor", "Track Tractor"),
        title = "Equipment Category"
      )
  })
  
  output$total_products <- renderValueBox({
    df <- querychat$df()
    unique_series <- length(unique(df$model_series))
    
    valueBox(
      value = unique_series,
      subtitle = "Product Lines",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$avg_satisfaction <- renderValueBox({
    df <- querychat$df()
    avg_satisfaction <- round(mean(df$customer_satisfaction, na.rm = TRUE), 2)
    color <- if(avg_satisfaction >= 4.0) "green" else if(avg_satisfaction >= 3.5) "yellow" else "red"
    
    valueBox(
      value = avg_satisfaction,
      subtitle = "Avg Satisfaction",
      icon = icon("star"),
      color = color
    )
  })
  
  output$total_fleet_size <- renderValueBox({
    df <- querychat$df()
    total_machines <- nrow(df)
    
    valueBox(
      value = format(total_machines, big.mark = ","),
      subtitle = "Total Machines",
      icon = icon("tractor"),
      color = "green"
    )
  })
  
  output$avg_market_share <- renderValueBox({
    df <- querychat$df()
    # Calculate average fuel efficiency across all machines
    avg_efficiency <- round(mean(df$fuel_efficiency, na.rm = TRUE), 2)
    
    valueBox(
      value = paste0(avg_efficiency, " mpg"),
      subtitle = "Avg Fuel Efficiency",
      icon = icon("gas-pump"),
      color = "purple"
    )
  })
}

shinyApp(ui, server)
