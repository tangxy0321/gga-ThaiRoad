pacman::p_load(tidyverse, sf, tmap,
               shiny, shinydashboard, shinyWidgets, plotly,
               shinyjs, raster, shinycssloaders, shinyBS, spatstat,
               bslib, viridis, classInt, spNetwork, tmaptools)

accidents_thai <- read_rds("data/rds/accidents_thai.rds")
thai_roads <- read_rds("data/rds/thai_roads.rds")
bangkok_districts_sf <- read_rds("data/rds/thai_boundary.rds")

accidents_col <- c(
  "District", "Weather Condition", "Road Description",
  "Slope Description", "Accident Categories", "Vehicle Categories",
  "Fatal Accident", "Year", "Day of Week", "Hour of Accident"
)

convert_variables <- function(x){
  return (case_when(
    x == "District" ~ "district",
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
  skin = "black",
  dashboardHeader(title = "Thailand Road Accidents Dashboard", titleWidth = 400),
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
      menuItem(HTML("Spatial Point Kernel<br/>Density Estimation"), tabName = "sppa", icon = icon("map-marker"),
               startExpanded = FALSE,
               menuSubItem(HTML("1st Order Kernel<br/>Density Estimation"), tabName = "sppa-1st-order"),
               menuSubItem(HTML("2nd Order Kernel<br/>Density Estimation"), tabName = "sppa-2nd-order")),
      menuItem(HTML("Network-constrained<br/>Kernel Density Estimation"), tabName = "nkde_menu", icon = icon("road"),
               startExpanded = FALSE,
               menuSubItem(HTML("Network-constrained<br/>Kernel Density Estimation"), tabName = "nkde"),
               menuSubItem(HTML("Network-constrained<br/>G and K Functions analysis"), tabName = "stat_analysis")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Welcome to the Thailand Road Accident Geospatial Analysis App!",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  HTML("Our app delves into the critical issue of road accidents in Thailand, particularly 
                       focusing on <b>Bangkok, the bustling metropolitan city notorious for its high accident 
                       rates.</b> Despite progress, Thailand still lags behind in road safety compared to 
                       countries like Singapore. To understand this stark contrast, we've embarked on a 
                       comprehensive geospatial analysis.<br>"),
                  
                  img(src = "https://scandasia.com/wp-content/uploads/2021/03/dreamstime_s_13572763-3.jpg", width = "100%"),
                  HTML("<b>Image:</b> Common traffic scenario in Thailand")
                ),
                
                # Overview section
                box(
                  title = "Overview:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("Our app delves into the critical issue of road accidents in Thailand, particularly focusing on <b>Bangkok, the bustling metropolitan city notorious for its high accident rates.</b> Despite progress, Thailand still lags behind in road safety compared to countries like Singapore. To understand this stark contrast, we've embarked on a comprehensive geospatial analysis.")
                ),
                
                # Motivation section
                box(
                  title = "Motivation:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("The glaring gap in road safety metrics between Thailand and Singapore inspired our team to create this tool. While the data is available online, navigating and analyzing it requires technical expertise. Our goal is to <b>democratize data analysis by offering a user-friendly platform accessible to all, regardless of coding proficiency.</b>")
                ),
                
                # Key Features section
                box(
                  title = "Key Features:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("• <b>Exploratory Data Analysis:</b> Gain insights into Thailand's road accident dataset and 
                    road network through various visualizations.<br>"),
                  HTML("• <b>Spatial Point Kernel Density Estimation:</b> Assess spatial distribution, clustering, and 
                  ordering patterns of accidents through visualizing areas with high concentrations of accidents 
                    within Bangkok.<br>"),
                  HTML("• <b>Network-Constrained Analysis:</b> Analyze accident distribution taking into 
                    consideration road networks."),
                ),
                
                # Future Prospects section
                box(
                  title = "Future Prospects:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("As computational limitations confine us to district-level analysis, our vision extends 
                    to <b>encompassing the entirety of Thailand</b>. We also aim to <b>enhance analysis performance 
                    through better computational resources</b>, ultimately advancing data democratization.")
                ),
                
                # Call to Action section
                box(
                  title = "Ending Note:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  HTML("By leveraging geospatial insights, you can identify problem areas, devise targeted 
                    interventions, and contribute to a safer driving environment. Let's work together towards 
                    reducing road accidents and saving lives in Thailand.<br><br>
                    
                    Start exploring now and join us in 
                    <b>Making Thai Roads Safer!!</b><br><br>
                       
                    This app was developed by <b>Xin Yi, Ying Shi, and Jackson</b>")
                ),
              )
            ),
      
      # Basic Distribution Tab
      tabItem(tabName = "basic_distribution",
              fluidRow(
                # Filters for Basic Distribution Tab
                column(4, selectInput(
                            inputId = "district_x",
                            label = "District",
                            choices = c("All", sort(unique(accidents_thai$district))), 
                            selected = "All",
                            multiple = FALSE)),
                column(4, selectInput(
                            inputId = "basic_x",
                            label = "X Variable", 
                            choices = accidents_col, 
                            selected = "District")),
                column(4, selectInput("basic_proportion", "Proportion Variable", 
                                      choices = c("None",accidents_col[2:10]),
                                      selected = "None"))
              ),
              br(),
              plotlyOutput("basic_distribution_plot", height = "600px")
      ),
      
      # Ranking Tab
      tabItem(tabName = "ranking",
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
                column(4, selectInput("district_sp", "District", 
                                      choices = c("All", sort(unique(accidents_thai$district))),
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
                                      choices = c("All" = "All", "Two Wheeled" = "two_wheeled", "Four Wheeled" = "four_wheeled",
                                                  "Heavy Duty" = "heavy_duty", "Others" = "others"), selected = "All")
                ),
                column(4, selectInput("accident_cause_sp", "Accident Categories",
                                      choices = c("All" = "All", "Driver Factors" = "driver_factors", "Speeding" = "speeding",
                                                  "Traffic Violations" = "traffic_violations",
                                                  "External Factors" = "external_factors",
                                                  "Others" = "others"), selected = "All"
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
              fluidRow(
                # Filters for Road Network Tab
                column(4, selectInput("district_rn", "District",
                                      choices = sort(unique(thai_roads$district)),
                                      selected = "Bangkok")
                ),
                column(4, selectInput("bridge_rn", "Bridge",
                                      choices = c("All", "yes", "no"),
                                      selected = "All")
                ),
                column(4, selectInput("tunnel_rn", "Tunnel",
                                      choices = c("All", "yes", "no"),
                                      selected = "All")
                )
              ),
              tmapOutput("road_network_map", height = "800px")
      ),
      
      #1st-order KDE
      tabItem(tabName = "sppa-1st-order",
              fluidPage(
                fluidRow(
                  box(width = 12, solidHeader = TRUE,
                      column(width = 4, solidHeader = TRUE,
                             selectInput('sppa_kde_bk_districts', 'Districts:',
                                         c("All", sort(unique(accidents_thai$district)))),
                             selectInput("sppa_kde_year", "Year:",
                                         c("All", unique(accidents_thai$year)), selected = "All"),
                             selectInput("sppa_kde_accident_type", "Accident Categories:",
                                         c("All" = "All", "Driver Factors" = "driver_factors", "Speeding" = "speeding",
                                           "Traffic Violations" = "traffic_violations",
                                           "External Factors" = "external_factors",
                                           "Others" = "others"), selected = "All"),
                             selectInput("sppa_kde_vehicle_type", "Vehicle Categories:",
                                         c("All" = "All", "Two Wheeled" = "two_wheeled", "Four Wheeled" = "four_wheeled",
                                           "Heavy Duty" = "heavy_duty", "Others" = "others"), selected = "All"),
                      ),
                      column(width = 4, solidHeader = TRUE,
                             selectInput("sppa_kde_kernels", "Smoothing Kernels:",
                                         c("Gaussian" = "gaussian", "Epanechnikov" = "epanechnikov",
                                           "Quartic" = "quartic", "Disc" = "disc"), selected = "gaussian"),
                             selectInput("sppa_kde_bw", "Bandwidth:",
                                         c("Automatic" = "Automatic", "Fixed" = "Fixed",
                                           "Adaptive" = "Adaptive"), selected = "Automatic"),
                             conditionalPanel(
                               condition = "input.sppa_kde_bw == 'Automatic'",
                               radioButtons("auto_sigma", "Automatic Bandwidth Selection:",
                                            c("Cross Validated - bw.diggle()" = "bw.diggle", "Cronie and van Lieshout's Criterion - bw.CvL()" = "bw.CvL", 
                                              "Scott's Rule - bw.scott()" = "bw.scott", "Likelihood Cross Validation - bw.ppl()" = "bw.ppl"))
                             ),
                             conditionalPanel(
                               condition = "input.sppa_kde_bw == 'Fixed'",
                               sliderInput("fixed_sigma", "Fixed Bandwidth (in km):",
                                           min = 1, max = 5, value = 2)
                             )
                      ),
                      column(width = 4,
                             sliderInput("sppa_kde_nearest_neighbor_nsim", "Number of simulations:",
                                         min = 99, max = 999, value = 99),
                             actionButton("SPPA_Run_KDE", "Run Analysis", icon("paper-plane"),
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))                  
                  )
                ),
                fluidRow(
                  column(7,
                         withSpinner(tmapOutput("mapPlot", width="100%", height="350px"), type=5),
                  ),
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "350px",
                    tabPanel("Overview",
                             HTML("<b>Hypotheses:</b><br>
                     Ho = The distribution of traffic accidents in <b>Bangkok</b> are randomly distributed.<br>
                     H1 = The distribution of traffic accidents in <b>Bangkok</b> region are not randomly distributed.<br><br>
                     <b>Confidence level:</b> 95% <br> <b>Critical (alpha) value:</b> 0.05 <br><br>")),
                    tabPanel("Clark-Evans Test",
                             verbatimTextOutput("clark"),
                             HTML("<b>Interpretation</b><br>If p-value < 0.05, reject the null hypothesis that the point patterns are randomly distributed.")),
                    width = 5,
                  )
                )
              )
      ),
      
      #2nd order KDE
      tabItem(tabName = "sppa-2nd-order",
              fluidPage(
                fluidRow(
                  box(width = 12, solidHeader = TRUE,
                      column(width = 6, solidHeader = TRUE,
                             selectInput('sppa_so_bk_districts', 'Districts:',
                                         c(sort(unique(accidents_thai$district))), selected = "Bang Bon"),
                             selectInput("sppa_so_year", "Year:",
                                         c("All", unique(accidents_thai$year)), selected = "All"),
                             selectInput("sppa_so_accident_type", "Accident Categories:",
                                         c("All" = "All", "Driver Factors" = "driver_factors", "Speeding" = "speeding",
                                           "Traffic Violations" = "traffic_violations",
                                           "External Factors" = "external_factors",
                                           "Others" = "others"), selected = "All"),
                             selectInput("sppa_so_vehicle_type", "Vehicle Categories:",
                                         c("All" = "All", "Two Wheeled" = "two_wheeled", "Four Wheeled" = "four_wheeled",
                                           "Heavy Duty" = "heavy_duty", "Others" = "others"), selected = "All"),
                      ),
                      column(width = 6,
                             selectInput("sppa_so_functions", "Type of Function:",
                                         c("G" = "G", "F" = "F", "K" = "K", "L" = "L"), selected = "G"),
                             sliderInput("sppa_so_nsim", "Number of simulations:",
                                         min = 99, max = 999, value = 99),
                             actionButton("SPPA_Run_CSR", "Run Analysis", icon("paper-plane"),
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  )
                ),
                fluidRow(
                  column(7,
                         withSpinner(plotOutput("mapPlot2", width="100%", height="350px"), type=5),
                  ),
                  box(width = 5, height = "350px", solidHeader = TRUE,
                      HTML("<h5><b>Complete Spatial Randomness Test</b></h5>"),
                      uiOutput("function_desc")
                  )
                )
              )
      ),
      
      #NKDE Tab
      tabItem(tabName = "nkde",
              fluidPage(
                fluidRow(
                  column(3, selectInput(inputId = "district", label = "Districts of Bangkok",
                                        choices = c(str_sort(unique(bangkok_districts_sf$ADM2_EN))), selected="Bang Bon")),
                  column(3, selectizeInput(inputId = "accident_cat", label = "Accident Categories",
                                           choices = str_sort(unique(accidents_thai$accident_categories)), multiple = TRUE)),
                  column(3, selectInput(inputId = "fatal", label = "Fatal",
                                        choices = str_sort(unique(accidents_thai$fatal_accident)))),
                  column(3, selectizeInput(inputId = "vehi_type", label = "Vehicle Type",
                                           choices = str_sort(unique(accidents_thai$vehicle_categories)), multiple = TRUE))
                ),
                fluidRow(
                  column(width = 4,
                         sliderInput(inputId = "lixel_length", label = "Lixel length",
                                     min = 0, max = 2000, value = 1000, step = 10),
                         sliderInput(inputId = "min_lixel_dist", label = "Min. Lixel Length",
                                     min = 0, max = 1500, value = 500, step = 10)
                  ),
                  column(width = 4,selectInput(inputId = "kernel_type", label = "Kernel Type",
                                               choices = c("quartic","triangle","tricube","gaussian","cosine","triweight","epanechnikov","uniform"),
                                               selected = "quartic")),
                  column(width = 4,selectInput(inputId = "kernel_method", label = "Method to be used",
                                               choices = c("simple","discontinuous","continuous")))
                ),
                fluidRow(
                  box(width = 8,
                      tmapOutput("ndke_tmap_plot")),
                  box(width = 4, title = "Interpretation",
                      p("The colour intensity of the road segment increases as the number of accident happened on the road segment increases"),
                      p("The error message 'argument is of length zero' indicates that no recorded road accident occurred in the selected district of Bangkok."))
                )
              )
      ),
      
      # G & K functions Tab (getting inner tab for G and K separately)
      tabItem(tabName = "stat_analysis",
              box(width = 4,
                  fluidRow(
                    column(width = 12,
                           selectInput(inputId = "district_k", label = "Districts for Bangkok",
                                       choices = c(str_sort(unique(bangkok_districts_sf$ADM2_EN))), selected = "Bang Bon"),
                           sliderInput(inputId = "nsimulation", label = "Number of Simulations", min = 1, max = 199, step = 1, value = 49),
                           sliderInput(inputId = "func_start", label = "Start", min = 0, max = 750, step = 10, value = 50),
                           sliderInput(inputId = "func_end", label = "End", min = 1, max = 1500, step = 10, value = 100))
                  )),
              tabBox(width = 8,
                     tabPanel(
                       "G Function",
                       plotOutput("g_func_plot"),
                       fluidRow(
                         column(width = 12,
                                h4("Interpretation"),
                                p("The shape of the graph tells us the way the accidents are spatially distributed and measures the distribution of the distances from an arbitrary event to its nearest event."),
                                HTML("<p>When the line:<br/><ul><li>Increases rapidly at short distance: The accident events are clustered.</li><li>Increases slowly up to distance where most events spaced, then increases rapidly: The accident events are distributed evenly.</li></ul></p>"))
                       )
                     ),
                     tabPanel(
                       "K Function",
                       plotOutput("k_func_plot"),
                       fluidRow(
                         column(width = 12,
                                h4("Interpretation"),
                                p("The graph measures the number of events found up to a given distance of any particular event."),
                                HTML("<p>When the line lies:<br/><ul><li>Above the envelop: There is significant cluster pattern.</li><li>Within the envelop: The events are completely random and independent of their spatial locations.</li><li>Below the envelop: There is significant regular pattern observed.</li></ul></p>"))
                       )
                     )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Server logic for Basic Distribution Tab -----------------------------------------------------------------
  
  # District filter
  district_reactive <- reactive({
    if (input$district_x == "All") {
      accidents_district <- accidents_thai
    } else {
      accidents_district <- accidents_thai[accidents_thai$district == input$district_x,]
    }
  })
  
  # Basic Plots
  basic_plot <- reactive({
    
    bx_string <- convert_variables(input$basic_x)
    
    ggplot(district_reactive(), aes_string(x=convert_variables(input$basic_x))) + 
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
    
    abs_plot <- ggplot(district_reactive(), 
                       aes_string(x = convert_variables(input$basic_x), 
                                                  fill = convert_variables(input$basic_proportion))) + 
      geom_bar() +
      labs(title = "Number of Accidents and Proportions",
           x = input$basic_x,
           y = "Count") +
      theme(title=element_text(size=20, face="bold"),
        axis.title=element_text(size=20))
  
    prop_plot <- district_reactive() %>%
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
  
  # Server logic for Ranking Tab -----------------------------------------------------------------

  # Output slider that take in max number of unique values of the selected column as max
  output$n_slider <- renderUI({
    sliderInput("myslider", "Slider text", 1,
                max(count(unique(accidents_thai %>% 
                                   dplyr::select(convert_variables(input$filter_by)) %>% 
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
  
  # Server logic for Spatial Points Tab -----------------------------------------------------------------
    
  # Filter dataset to fit the map
  map_df_reactive <- reactive({
    # Setting up filters default
    district_filter <- TRUE
    weather_filter <- TRUE
    fatal_filter <- TRUE
    
    days_filter <- TRUE
    hour_filter <- TRUE
    vehicle_filter <- TRUE
    accident_cause_filter <- TRUE
    road_desc_filter <- TRUE
    slope_filter <- TRUE
    
    # Setting up filters conditions
    if (input$district_sp != "All"){
      district_filter <- accidents_thai$district %in% input$district_sp
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
    accidents_thai[district_filter & weather_filter & fatal_filter &
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
  
  # Server logic for Road Network Tab -----------------------------------------------------------------

  # Filter Road Networks
  rn_df_reactive <- reactive({
    # Setting up filters default
    bridge_filter <- TRUE
    tunnel_filter <- TRUE

    # Setting up filters conditions
    district_rn_filter <- thai_roads$district %in% input$district_rn

    if (input$bridge_rn != "All"){
      bridge_filter <- thai_roads$bridge %in%
        case_when(
          input$bridge_rn == "yes" ~ "T",
          input$bridge_rn == "no" ~ "F")
    }

    if (input$tunnel_rn != "All"){
      tunnel_filter <- thai_roads$tunnel %in%
        case_when(
          input$tunnel_rn == "yes" ~ "T",
          input$tunnel_rn == "no" ~ "F")
    }

    # Filtering road network
    thai_roads[district_rn_filter & bridge_filter & tunnel_filter,]
  })

  # Rendering the map based on filtered road network
  output$road_network_map <- renderTmap({
    tmap_mode("view")

    tm_shape(rn_df_reactive()) +
        tm_lines()
  })
  
  # Server logic for 1st order spatial point pattern analysis (default view) -----------------------------------------------------------------
  observeEvent(input$SPPA_Run_KDE, {
    districts_input <- input$sppa_kde_bk_districts
    year_input <- input$sppa_kde_year
    accident_type_input <- input$sppa_kde_accident_type
    vehicle_type_input <- input$sppa_kde_vehicle_type
    nsim_input <- input$sppa_kde_nearest_neighbor_nsim
    print(year_input)
    print(accident_type_input)
    print(vehicle_type_input)
    print(nsim_input)
    
    # if districts_input does not equal to "All", filter bangkok_extracted_sf
    if ( districts_input != "All" ) {
      bangkok_districts_sf <- bangkok_districts_sf %>%
        filter(ADM2_EN == districts_input)
    }
    # str(bangkok_extracted_sf)
    
    # If year_input is not "All", filter by year
    if (year_input != "All") {
      accidents_thai <- accidents_thai %>%
        filter(year == year_input)
    }
    
    # If accident_type_input is not "All", filter by accident_categories
    if (accident_type_input != "All") {
      accidents_thai <- accidents_thai %>%
        filter(accident_categories == accident_type_input)
    }
    
    # If vehicle_type_input is not "All", filter by vehicle_categories
    if (vehicle_type_input != "All") {
      accidents_thai <- accidents_thai %>%
        filter(vehicle_categories == vehicle_type_input)
    }
    
    
    district_owin <- as.owin(bangkok_districts_sf)
    district_ppp <- as.ppp(accidents_thai)
    district_ppp_jit <- rjitter(district_ppp,
                                retry=TRUE,
                                nsim=1,
                                drop=TRUE)
    
    district_accidents_ppp <- district_ppp_jit[district_owin]
    district_accidents_ppp_km <- rescale(district_accidents_ppp, 1000, "km")
    
    kernel_input <- input$sppa_kde_kernels
    bandwidth_input <- input$sppa_kde_bw
    print(kernel_input)
    
    if (bandwidth_input == "Automatic") {
      auto_sigma_input <- switch(input$auto_sigma,
                                 "bw.diggle" = bw.diggle,
                                 "bw.CvL" = bw.CvL,
                                 "bw.scott" = bw.scott,
                                 "bw.ppl" = bw.ppl
      )
      
      district_kde <- density(district_accidents_ppp_km,
                              sigma=auto_sigma_input,
                              edge=TRUE,
                              kernel=kernel_input)
      plot(district_kde)
    } else if (bandwidth_input == "Fixed") {
      fixed_sigma_input <- input$fixed_sigma
      district_kde <- density(district_accidents_ppp_km,
                              sigma=fixed_sigma_input,
                              edge=TRUE,
                              kernel=kernel_input)
    } else {
      district_kde <- adaptive.density(default_province_accidents_ppp_km, method="kernel")
    }
    
    raster_kde_auto_diggle <- raster(district_kde)
    projection(raster_kde_auto_diggle) <- CRS("+init=EPSG:32648 +units=km")
    output$mapPlot <- renderTmap({
      tm_basemap(server = "OpenStreetMap.DE") +
        tm_basemap(server = "Esri.WorldImagery") +
        tm_shape(raster_kde_auto_diggle) +
        tm_raster("layer",
                  n = 7,
                  title = "v",
                  style = "pretty",
                  alpha = 0.6,
                  palette = c("#fafac3","#fd953b","#f02a75","#b62385","#021c9e")) +
        tm_shape(bangkok_districts_sf) +
        tm_polygons(alpha=0.1,id="ADM2_EN") +
        tm_view(set.zoom.limits = c(10, 15)) +
        tmap_options(check.and.fix = TRUE)
    })
    
    
    output$clark <- renderPrint(clarkevans.test(
      district_ppp, correction="none",
      clipregion = NULL, alternative=c("two.sided"),
      nsim=nsim_input
    ))
  })
  
  # Server logic for 2nd order spatial point pattern analysis (default view) -----------------------------------------------------------------
  observeEvent(input$SPPA_Run_CSR, {
    so_districts_input <- input$sppa_so_bk_districts
    so_year_input <- input$sppa_so_year
    so_accident_type_input <- input$sppa_so_accident_type
    so_vehicle_type_input <- input$sppa_so_vehicle_type
    so_function_type_input <- input$sppa_so_functions
    so_nsim_input <- input$sppa_so_nsim
    
    # if so_districts_input does not equal to "All", filter bangkok_districts_sf
    so_districts_extracted_sf <- bangkok_districts_sf %>%
      filter(ADM2_EN == so_districts_input)
    
    so_district_accidents_sf <- accidents_thai
    # If so_year_input is not "All", filter by year
    if (so_year_input != "All") {
      so_district_accidents_sf <- accidents_thai %>%
        filter(year == so_year_input)
    }
    
    # If so_accident_type_input is not "All", filter by accident_categories
    if (so_accident_type_input != "All") {
      so_district_accidents_sf <- accidents_thai %>%
        filter(accident_categories == so_accident_type_input)
    }
    
    # If so_vehicle_type_input is not "All", filter by vehicle_categories
    if (so_vehicle_type_input != "All") {
      so_district_accidents_sf <- accidents_thai %>%
        filter(vehicle_categories == so_vehicle_type_input)
    }
    
    so_district_owin <- as.owin(so_districts_extracted_sf)
    so_district_ppp <- as.ppp(so_district_accidents_sf)
    so_district_ppp_jit <- rjitter(so_district_ppp,
                                   retry=TRUE,
                                   nsim=1,
                                   drop=TRUE)
    
    so_district_accidents_ppp <- so_district_ppp_jit[so_district_owin]
    
    # Gest function
    if (so_function_type_input == "G") {
      so_district_csr <- envelope(so_district_accidents_ppp, Gest, nsim = so_nsim_input)
      so_interpretion <- paste0("</h4><b>Hypotheses:</b>
        <br>Ho = The distribution of traffic accidents in <b>",so_districts_input,"</b> are randomly distributed.
        <br>H1 = The distribution of traffic accidents in <b>",so_districts_input,"</b> are not randomly distributed.<br><br>
        <b>The G Function</b><br>Estimates the nearest neighbour distance distribution function G(r) from a point pattern in a window of arbitrary shape<br><br>
        <b>Interpretation</b><br><ul>
        <li>Clustered: G increases rapidly at short distance</li>
        <li>Evenness: G increases slowly up to distance where most events spaced, then increases rapidly</li>
      </ul>")
      
      # Fest function
    } else if (so_function_type_input == "F") {
      so_district_csr <- envelope(so_district_accidents_ppp, Fest, nsim = so_nsim_input)
      so_interpretion <- paste0("</h4><b>Hypotheses:</b>
        <br>Ho = The distribution of traffic accidents in <b>",so_districts_input,"</b> are randomly distributed.
        <br>H1 = The distribution of traffic accidents in <b>",so_districts_input,"</b> are not randomly distributed.<br><br>
        <b>The F Function</b><br>Estimates the empty space function F(r) or its hazard rate h(r) from a point pattern in a window of arbitrary shape<br><br>
        <b>Interpretation</b><br><ul>
        <li>Clustered: F(r) rises slowly at first, but more rapidly at longer distances</li>
        <li>Evenness: F(r) rises rapidly at first, then slowly at longer distances</li>
      </ul>")
      
      # Kest function
    } else if (so_function_type_input == "K") {
      so_district_csr <- envelope(so_district_accidents_ppp, Kest, nsim = so_nsim_input, rank = 1, glocal=TRUE)
      so_interpretion <- paste0("</h4><b>Hypotheses:</b>
        <br>Ho = The distribution of traffic accidents in <b>",so_districts_input,"</b> are randomly distributed.
        <br>H1 = The distribution of traffic accidents in <b>",so_districts_input,"</b> are not randomly distributed.<br><br>
        <b>The K Function</b><br>Estimates Ripley's reduced second moment function K(r) from a point pattern in a window of arbitrary shape<br><br><b>Interpretation</b><br><ul>
        <li>Above the envelop: significant cluster pattern</li>
        <li>Below the envelop: significant regular</li>
        <li>Inside the envelop: Complete Spatial Randomness</li>
      </ul>")
      
      # Lest function
    } else {
      so_district_csr <- envelope(so_district_accidents_ppp, Lest, nsim = so_nsim_input, rank = 1, glocal=TRUE)
      so_interpretion <- paste0("</h4><b>Hypotheses:</b>
        <br>Ho = The distribution of traffic accidents in <b>",so_districts_input,"</b> are randomly distributed.
        <br>H1 = The distribution of traffic accidents in <b>",so_districts_input,"</b> are not randomly distributed.<br><br>
        <b>The L Function</b><br>Calculates an estimate of the L-function (Besag's transformation of Ripley's K-function) for a spatial point pattern<br><b>Interpretation</b><br><ul>
        <li>L(r)>0 indicates that the observed distribution is geographically concentrated.</li>
        <li>L(r)<0 implies dispersion</li>
        <li>L(r)=0 indicates complete spatial randomness (CRS)</li>
      </ul>")
    }
    
    output$mapPlot2 <- renderPlot({
      plot(so_district_csr, main=NULL)
    })
    
    output$function_desc <- renderText({
      HTML(so_interpretion)
    })
  })
  
  # Server logic for NKDE -----------------------------------------------------------------
  # NKDE plot
  nkde_plotting <- reactive({
    distr <- input$district
    acc_cat <- input$accident_cat
    vehi_cat <- input$vehi_cat
    lx_length <- input$lixel_length
    mindist <- input$min_lixel_dist
    kernel_type <- input$kernel_type
    kernel_method <- input$kernel_method
    
    # filter dataset
    thai_distr <- bangkok_districts_sf %>%
      filter(ADM2_EN == distr)
    prov_road <- st_cast(st_intersection(thai_roads, thai_distr),"LINESTRING")
    data_filtered <- accidents_thai %>%
      filter(province == "Bangkok") %>% 
      st_intersection(thai_distr)
    if(!is.null(acc_cat)){
      data_filtered <- data_filtered %>% 
        filter(accident_categories %in% acc_cat)
    }
    if(!is.null(vehi_cat)){
      data_filtered <- data_filtered %>% 
        filter(vehicle_categories %in% vehi_cat)
    }
    
    # run nkde
    lixels <- lixelize_lines(prov_road,lx_length = lx_length,mindist = mindist)
    samples <- lines_center(lixels)
    densities <- nkde(prov_road,
                      events = data_filtered,
                      w = rep(1, nrow(data_filtered)),
                      samples = samples,
                      ## important
                      kernel_name = kernel_type,
                      bw = mindist,
                      ## important
                      div = "bw",
                      method = kernel_method,
                      digits = 1,
                      tol = 1,
                      grid_shape = c(1,1),
                      max_depth = 8,
                      # aggregate events within 10m radius (faster calculation)
                      agg = 10,
                      sparse = TRUE,
                      verbose = FALSE
    )
    samples$density <- densities
    lixels$density <- densities
    samples$density <- samples$density*1000
    lixels$density <- lixels$density*1000
    # plot
    tm_shape(thai_distr)+
      tm_polygons()+
      tm_shape(lixels)+
      tm_lines(col="density",
               lwd = 3)
  })
  output$ndke_tmap_plot <- renderTmap({nkde_plotting()})
  
  # Server logic for Stats -----------------------------------------------------------------
  stat_plotting <- reactive({
    req(input$district_k, input$nsimulation, input$func_start, input$func_end)
    # filter dataset by district
    thai_distr_k <- bangkok_districts_sf %>%
      filter(ADM2_EN == input$district_k)
    data_filter <- accidents_thai %>%
      filter(province == "Bangkok") %>% 
      st_intersection(thai_distr_k)
    thai_road_distr <- thai_roads %>% 
      filter(district == input$district_k)
    thai_road_distr <- st_cast(thai_road_distr, "LINESTRING")
    # stat_test
    kfunc <- kfunctions(thai_road_distr,data_filter,
                        start = input$func_start, end = input$func_end,
                        step = 1, width = 100,
                        agg = 51,
                        nsim = input$nsimulation)
    combo <- kfunc$plotg
    combo
  })
  output$g_func_plot <- renderPlot({stat_plotting()})
  # output$k_func_plot <- renderPlot({stat_plotting()[2]})
}

# Run the application
shinyApp(ui = ui, server = server)