pacman::p_load(tidyr, sf, dplyr, tmap, ggplot2, readr,
               shiny, shinydashboard, shinyWidgets, plotly)

accidents_thai <- read_rds("data/rds/accidents_thai.rds")

# thai_roads <- read_rds("data/rds/thai_roads.rds")

accidents_col <- c(
  "Province", "Weather Condition", "Road Description",
  "Slope Description", "Accident Categories", "Vehicle Categories",
  "Fatal Accident", "Year", "Day of Week", "Hour of Accident"
)

convert_variables <- function(x){
  return (case_when(
    x == "Province" ~ "province_en",
    x == "Weather Condition" ~ "weather_condition",
    x == "Road Description" ~ "road_description",
    x == "Slope Description" ~ "slope_description",
    x == "Road Description" ~ "road_description",
    x == "Accident Categories" ~ "accident_categories",
    x == "Vehicle Categories" ~ "vehicle_categories",
    x == "Fatal Accident" ~ "fatal_accident",
    x == "Year" ~ "year",
    x == "Day of Week" ~ "weekday",
    x == "Hour of Accident" ~ "accident_hr",
    x == "None" ~ "None"
  ))
}

ui <- dashboardPage(
  dashboardHeader(title = "Thailand Road Accidents Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Exploratory Data Analysis", tabName = "menu1", icon = icon("magnifying-glass-chart"),
               startExpanded = FALSE,
               menuSubItem("Basic Distribution", tabName = "basic_distribution"),
               menuSubItem("Ranking", tabName = "ranking"),
               menuSubItem("Spatial Points", tabName = "spatial_points"),
               menuSubItem("Road Network", tabName = "road_network")
               ),
      menuItem(HTML("1st Order Kernel<br/>Density Estimation"), tabName = "1kde", icon = icon("sort-amount-up")
               #menuSubItem("Ranking", tabName = "ranking")
               ),
      menuItem(HTML("2nd Order Kernel<br/>Density Estimation"), tabName = "2kde", icon = icon("map-marker")),
      menuItem(HTML("Network-constrained<br/>Spatial Point Patterns Analysis"), tabName = "nkde", icon = icon("road"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              p("This is the home page")),
      # Basic Distribution Tab
      tabItem(tabName = "basic_distribution",
              fluidRow(
                # Filters for Basic Distribution Tab
                column(4, selectInput(
                            inputId = "province_x",
                            label = "Province",
                            choices = c("All", unique(accidents_thai$province_en)), 
                            selected = "All",
                            multiple = FALSE)),
                column(4, selectInput(
                            inputId = "basic_x",
                            label = "X Variable", 
                            choices = accidents_col, 
                            selected = "Province")),
                column(4, selectInput("basic_proportion", "Proportion Variable", 
                                      choices = c("None",accidents_col[2:10]),
                                      selected = "None"))
              ),
              br(),
              plotlyOutput("basic_distribution_plot", height = "600px")
      ),
      
      # Ranking Tab
      tabItem(tabName = "ranking",
              p("This dashboard takes a moment to load... please be patient!"),
              fluidRow(
                # Filters for Ranking Tab
                column(3, selectInput("filter_by", "Rank By", choices = accidents_col)),
                column(5, uiOutput("n_slider"))
                ),
              plotlyOutput("ranking_plot", height = "600px")
      ),
      
      # Spatial Points Tab
      tabItem(tabName = "spatial_points",
              fluidRow(
                # Filters for Spatial Points Tab - Accident Related Filters
                column(4, selectInput("province_sp", "Province", 
                                      choices = c("All", unique(accidents_thai$province_en)),
                                      selected = "All")
                ),
                column(4, selectInput("weather_sp", "Weather Condition", 
                                      choices = c("Both", "yes", "no"),
                                      selected = "Both")
                ),
                column(4, selectInput("fatal_sp", "Fatal Accident", 
                                      choices = c("Both", "yes", "no"),
                                      selected = "Both")
                ),
                column(4, selectInput("vehicle_sp", "Vehicle Categories",
                                      choices = c("All", as.character(unique(accidents_thai$vehicle_categories))),
                                      )
                ),
                column(4, selectInput("accident_cause_sp", "Accident Categories",
                                      choices = c("All", as.character(unique(accidents_thai$accident_categories)))
                                      )
                )
              ),
              
              fluidRow(
                # Filters for Spatial Points Tab - Time/Road Filters
                column(4, sliderInput("year_sp", "Years", 
                                      min(accidents_thai$year), 
                                      max(accidents_thai$year),
                                      value = c(min(accidents_thai$year), 
                                                max(accidents_thai$year)),
                                      step = 1)
                ),
                column(4, selectInput("days_sp", "Day of week", 
                                      choices = unique(accidents_thai$weekday),
                                      multiple = TRUE,
                                      selected = unique(accidents_thai$weekday))
                ),
                column(4, selectInput("hour_sp", "Hour of Accident", 
                                      choices = unique(accidents_thai$accident_hr),
                                      multiple = TRUE,
                                      selected = unique(accidents_thai$accident_hr))
                ),
                column(4, selectInput("road_desc_sp", "Road Description", 
                                      choices = c("All", unique(accidents_thai$road_description)),
                                      )
                ),
                column(4, selectInput("slope_sp", "Slope Description", 
                                      choices = c("All", unique(accidents_thai$slope_description))
                                      )
                ),
                column(4, selectInput("dot_col_sp", "Colour Dots By", 
                                      choices = c("None", accidents_col)
                                      )
                )
              ),
              
              tmapOutput("spatial_points_map", height = "800px"),
              dataTableOutput("map_dataset")
      ),
      
      # Road Network Tab
      tabItem(tabName = "road_network",
              p("This takes too large of a dataset to run so I disabled it, and shinyapp only allows 1GB... any solutions for this?")
              # p("This dashboard takes a moment to load... please be patient!"),
              # fluidRow(
              #   # Filters for Road Network Tab
              #   column(4, selectInput("province_rn", "Province", 
              #                         choices = unique(thai_roads$Province),
              #                         selected = "Bangkok")
              #   ),
              #   column(4, selectInput("bridge_rn", "Bridge", 
              #                         choices = c("All", "yes", "no"),
              #                         selected = "All")
              #   ),
              #   column(4, selectInput("tunnel_rn", "Tunnel", 
              #                         choices = c("All", "yes", "no"),
              #                         selected = "All")
              #   )
              # ),
              # tmapOutput("road_network_map", height = "800px")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Server logic for Basic Distribution Tab
  
  # observeEvent(input$basic_x, {
  #   # Method 1
  #   updatePickerInput(session = session, inputId = "basic_proportion",
  #                     choices = "abc")
  # })
  
  # Province filter
  province_reactive <- reactive({
    if (input$province_x == "All") {
      accidents_province <- accidents_thai
    } else {
      accidents_province <- accidents_thai[accidents_thai$province_en == input$province_x,]
    }
  })
  
  # Basic Plots
  basic_plot <- reactive({
    
    bx_string <- convert_variables(input$basic_x)
    
    ggplot(province_reactive(), aes_string(x=convert_variables(input$basic_x))) + 
      geom_bar() +
      labs(title = "Accidents Distribution",
           x = input$basic_x,
           y = "Number of Accidents") +
      theme(title=element_text(size=20, face="bold"),
            axis.title=element_text(size=20),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  # Proportion Plots
  proportion_plot <- reactive({
    pct_string <- "proportion"
    
    abs_plot <- ggplot(province_reactive(), 
                       aes_string(x = convert_variables(input$basic_x), 
                                                  fill = convert_variables(input$basic_proportion))) + 
      geom_bar() +
      labs(title = "Number of Accidents and Proportions",
           x = input$basic_x,
           y = "Count") +
      theme(title=element_text(size=20, face="bold"),
        axis.title=element_text(size=20))
  
    prop_plot <- province_reactive() %>%
      group_by_(convert_variables(input$basic_x), convert_variables(input$basic_proportion)) %>%
      summarise(n = n()) %>%
      mutate(proportion = prop.table(n) * 100) %>%
      ggplot() +
      aes_string(x=convert_variables(input$basic_x), 
                y=pct_string,
                fill=convert_variables(input$basic_proportion)) +
      geom_bar(stat="identity") +
      labs(title = "Proportions Plot",
           x = input$basic_x,
           y = "Proportion") +
      theme(title=element_text(size=20, face="bold"),
            axis.title=element_text(size=20),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    subplot(abs_plot, prop_plot, nrows=2, shareX = TRUE, titleX = TRUE, titleY = TRUE)
  })
  
  # Switch statement between basic plots and proportion plots
  graphInput <- reactive({
    switch(input$basic_proportion,
           "None" = basic_plot(),
           proportion_plot()
    )
  })
  
  # Return the requested graph
  output$basic_distribution_plot <- renderPlotly({
    graphInput()
  })
  
  # Server logic for Ranking Tab

  # Output slider that take in max number of unique values of the selected column as max
  output$n_slider <- renderUI({
    sliderInput("myslider", "Slider text", 1,
                max(count(unique(accidents_thai %>% 
                                   select(convert_variables(input$filter_by)) %>% 
                                   st_drop_geometry()))),
                1,
                step = 1
                )
  })
  
  # Ranking by top x number
  accidents_topx <- reactive({
    
    accidents_thai %>%
    group_by_(convert_variables(input$filter_by)) %>%
    summarise(total_count=n()) %>%
    top_n(input$myslider, total_count) %>%
    .[,convert_variables(input$filter_by)] %>%
    pull(1)})
  
  output$ranking_plot <- renderPlotly({
    ggplot(accidents_thai[(accidents_thai[,convert_variables(input$filter_by)] %>% 
                             pull(1)) %in% accidents_topx(),],
           aes_string(x=convert_variables(input$filter_by))) + geom_bar() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste(c("Top", input$myslider, input$filter_by, "with most accidents"), collapse = " "),
           x = input$filter_by,
           y = "Number of Accidents")
  })
  
  # Server logic for Spatial Points Tab
  
  # Filter dataset to fit the map
  map_df_reactive <- reactive({
    # Setting up filters default
    province_filter <- TRUE
    weather_filter <- TRUE
    fatal_filter <- TRUE
    
    days_filter <- TRUE
    hour_filter <- TRUE
    vehicle_filter <- TRUE
    accident_cause_filter <- TRUE
    road_desc_filter <- TRUE
    slope_filter <- TRUE
    
    # Setting up filters conditions
    if (input$province_sp != "All"){
      province_filter <- accidents_thai$province_en %in% input$province_sp
    }
    
    if (input$weather_sp != "Both"){
      fatal_filter <- accidents_thai$weather_condition %in% input$weather_sp
    }
    
    if (input$fatal_sp != "Both"){
      weather_filter <- accidents_thai$fatal_accident %in% input$fatal_sp
    }
    
    if (input$vehicle_sp != "All"){
      vehicle_filter <- accidents_thai$vehicle_categories %in% input$vehicle_sp
    }
    
    if (input$accident_cause_sp != "All"){
      accident_cause_filter <- accidents_thai$accident_categories %in% input$accident_cause_sp
    }
    
    if (input$road_desc_sp != "All"){
      road_desc_filter <- accidents_thai$road_description %in% input$road_desc_sp
    }
  
    if (input$slope_sp != "All"){
      slope_filter <- accidents_thai$slope_description %in% input$slope_sp
    }
    
    year_filter <- accidents_thai$year >= input$year_sp[1] & accidents_thai$year <= input$year_sp[2]
    
    days_filter <- accidents_thai$weekday %in% input$days_sp
    
    hour_filter <- accidents_thai$accident_hr %in% input$hour_sp
    
    # Filtering dataset
    accidents_thai[province_filter & weather_filter & fatal_filter &
                   year_filter & days_filter & hour_filter &
                    vehicle_filter & accident_cause_filter &
                     road_desc_filter & slope_filter,]
  })
  
  # Data table for the filtered dataset
  output$map_dataset <-renderDataTable({
    map_df_reactive()
  })
  
  # Rendering the map based on filtered dataset
  output$spatial_points_map <- renderTmap({
    tmap_mode("view")
    
    # If no colour filter set
    if (input$dot_col_sp == "None"){
      tm_shape(map_df_reactive()) +
        tm_dots()
    } 
    
    # Else colour filter
    else {
      tm_shape(map_df_reactive()) +
        tm_dots(convert_variables(input$dot_col_sp))
    }
  })
  
  # # Server logic for Road Network Tab
  # 
  # # Filter Road Networks
  # rn_df_reactive <- reactive({
  #   # Setting up filters default
  #   bridge_filter <- TRUE
  #   tunnel_filter <- TRUE
  #   
  #   # Setting up filters conditions
  #   province_rn_filter <- thai_roads$Province %in% input$province_rn
  #   
  #   if (input$bridge_rn != "All"){
  #     bridge_filter <- thai_roads$bridge %in% 
  #       case_when(
  #         input$bridge_rn == "yes" ~ "T",
  #         input$bridge_rn == "no" ~ "F")
  #   }
  #   
  #   if (input$tunnel_rn != "All"){
  #     tunnel_filter <- thai_roads$tunnel %in% 
  #       case_when(
  #         input$tunnel_rn == "yes" ~ "T",
  #         input$tunnel_rn == "no" ~ "F")
  #   }
  #   
  #   # Filtering road network
  #   thai_roads[province_rn_filter & bridge_filter & tunnel_filter,]
  # })
  # 
  # # Rendering the map based on filtered road network
  # output$road_network_map <- renderTmap({
  #   tmap_mode("view")
  #   
  #   tm_shape(rn_df_reactive()) +
  #       tm_lines()
  # })
}

# Run the application
shinyApp(ui = ui, server = server)