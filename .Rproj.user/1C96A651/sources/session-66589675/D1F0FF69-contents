## load packages

library(cpaltemplates)
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tigris)
library(leaflet)
library(shiny)
library(plotly)
library(tidyr)
library(DT)
library(writexl)

## set working directory
#setwd("C:/Users/theny/Documents/CPAL Summer 2024/Vacant Land Project")

## Load Data

features_final <- st_read("data/Processed Data/Processed Vacant Land Parcels with Additional Data Appended.geojson")

### creating the Shiny App ###

##### Theme #####
title <- tags$a(#href="https://www.google.com",
  div(
    tags$img(src = "images/CPAL_Logo_White.png",
             height = "30", style = "vertical-align: middle; margin-right: 40px;"), # Add margin to the right of the image
    strong("Dallas County Vacant Lots", style = "font-weight: bold; vertical-align: middle;"), # Ensure text is bold
    style = "display: flex; align-items: center;" # Use flexbox to align items centrally
  )
)

# Define UI
ui <- navbarPage(
  title = span(title, style = "display: inline-block;"), # Ensure title is inline to allow other elements next to it
  theme = cpaltemplates::cpal_shiny(),
  
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: #E7ECEE;  color:black}
                  .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
                  .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
                  .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
                  .tabbable > .nav > li[class=active]    > a {background-color: gold; color:white}
                  ")),
  
  tags$head(
    tags$style(HTML(
      "
      .dataTables_wrapper {
        border: 4px solid #042d33; /* Example color for $teal-900 */
        border-radius: 10px 10px 5px 5px;
      }

      .dataTable {
        overflow: hidden;
      }

      .dataTable .last-row {
        background-color: #004D40;
      }

      .dataTable tbody tr:hover {
        background-color: rgba(4, 45, 51, 0.1);
      }

      .dataTable thead th:hover {
        color: #FFF;
        cursor: pointer;
      }

      .dataTable th, 
      .dataTable td {
        font-family: 'Poppins', sans-serif;
        text-align: left;
      }

      .dataTable td {
        font-size: 15px;
        padding-left: 15px;
      }

      .dataTable th {
        background-color: #042d33;
        color: #FFFFFF;
        line-height: 1.2;
        font-style: normal;
        font-variant: small-caps;
      }
      "
    ))
  ),
  # Custom HTML for the action button, positioned using CSS
  tags$head(
    tags$style(HTML("
      .navbar-custom-button {
        position: absolute;
        top: 14px; /* Adjust as needed */
        right: 10px; /* Adjust as needed */
        z-index: 1000; /* Ensure it's above other elements */
      }
       .nav-tabs > li > a {
        font-size: 12px; /* Adjust the font size as needed */
        padding: 6px 14px; /* Adjust the padding as needed */
      }
     .map-container {
        position: relative;
        display: inline-block;
        border: 5px solid #042d33;
        padding: 1px;
        margin: 25px;
        width: 99%;  /* Set the desired width */
        height: 85vh; /* Set the desired height */
      }
      "
    ))
  ),
  
  
  tags$div(
    class = "navbar-custom-button",
    actionButton("showModal", label = "", icon = icon("info-circle"),
                 style = "background-color: transparent; border: 2px solid #FFFFFF; border-radius: 50%; color: 
                 #FFFFFF; font-size: 24px; width: 36px; height: 36px; display: flex; justify-content: center; align-items: center;")
  ),
  tabPanel(
    "Map Tool",
    fluidPage(
      fluidRow(
        column(2,
               checkboxGroupInput("ownership_filters", tags$b("View vacant lots by owner type:"),
                                  choices = list(
                                    "Publicly owned lot" = "PUBLIC_ACCOUNT",
                                    "Faith based ownership" = "FBO_ACCOUNT"
                                  ),
                                  selected = c("PUBLIC_ACCOUNT", "FBO_ACCOUNT")),
               checkboxGroupInput("other_filters", tags$b("View vacant lots with features nearby:"),
                                  choices = list(
                                    "School" = "School_Flag",
                                    "Grocery Store" = "Grocery_Flag",
                                    "Public Transportation" = "Transit_Flag",
                                    "Clinic" = "Clinic_Flag",
                                    "Park" = "Park_Flag",
                                    "Rec Center" = "Rec_Center_Flag",
                                    "Library" = "Library_Flag",
                                    "Environmental Hazard" = "ENV_HAZ_Flag"
                                  )),
               checkboxGroupInput("tier_filters", tags$b("View vacant lots by opportunity level:"),
                                  choices = list(
                                    "High opportunity" = "High",
                                    "Moderate opportunity" = "Moderate",
                                    "Low opportunity" = "Low"
                                  ),
                                  selected = c("High", "Moderate")),
               downloadButton("downloadData", "Download Data"),
               hr(),
               htmlOutput("marker_info")
        ),
        column(7,
               div(class = "map-container",
                   leafletOutput("map", height = "100%", width = "100%")
               )
        ),
        column(3,
               tabsetPanel(
                 tabPanel("Lot Opportunity", htmlOutput("textBox")),
                 tabPanel("Estimate Apartments", 
                          HTML("<div style='font-size: 18px;'>", 
                               "Use the slider to select the number of stories for the apartment building. 
                             Below the slider, input the number of square feet for each apartment.<br>",
                               "</div>"
                          ),
                          uiOutput("stories_slider"), 
                          uiOutput("squarefeet_input"), 
                          HTML("<div style='font-size: 14px;'>", 
                               "<b>Note:</b> This tool provides a basic estimate of the number of apartments that can be built on the 
                               selected vacant land. It does not account for all variables and constraints that may affect the 
                               actual number of units.",
                               "</div>"
                          ),
                          htmlOutput("text2")
                 ),
                 tabPanel("Geography",
                          selectInput("analysis_option", "Select a metric to compare opportunity zones:", 
                                      choices = c("Zip Code" = "PROPERTY_ZIPCODE", "Census Tract" = "tract_name", 
                                                  "City Council District (Dallas)" = "COUNCIL")),
                          htmlOutput("ownership_counts"),
                          dataTableOutput("area_table"),
                          downloadButton("download_table", "Download Table Data")
                 )
               )
        )      
      )
    )
  ),

  )



# Define server logic
server <- function(input, output, session) {
  homesteadDesc <- htmltools::HTML(
  paste0("The displayed vacant lots are either publicly owned or owned by faith-based organizations (FBO). 
          Opportunity levels are based on the proximity of resources such as schools, clinics, grocery stores, parks, 
          public transportation, recreation centers, and libraries, as well as the presence of environmental hazards.<br><br>
                                            
          Use the interactive map to explore the vacant lots. Apply the filters on the left to view lots based on 
          ownership type, nearby features, and opportunity level. Click 'Download Data' to obtain an XLS file containing 
          the selected observations and a data dictionary.<br><br>"))
  
  # Automatically show the modal when the app starts
  showModal(modalDialog(
    title = htmltools::HTML("<strong>About This Tool</strong>"),
    homesteadDesc,
    size = "l",
    footer = modalButton("Dismiss")
  ))
  
  # Observe the info button click to show the modal again
  observeEvent(input$showModal, {
    showModal(modalDialog(
      title = htmltools::HTML("<strong>About This Tool </strong>"),
      homesteadDesc,
      size = "l",
      footer = modalButton("Dismiss")
    ))
  })
  ## reactive logic for filters
  filteredData <- reactive({
    data <- features_final
    
    # Apply ownership filters
    
    if (!is.null(input$ownership_filters) && length(input$ownership_filters) > 0) {
      ownership_conditions <- lapply(input$ownership_filters, function(filter) {
        data[[filter]] == TRUE
      })
      data <- data[Reduce("|", ownership_conditions),]
    }
    
    # Apply feature filters
    
    if (!is.null(input$other_filters) && length(input$other_filters) > 0) {
      for (filter in input$other_filters) {
        data <- data[data[[filter]] == TRUE,]
      }
    }
    
    
    # Apply tier filters
    
    if (!is.null(input$tier_filters) && length(input$tier_filters) > 0) {
      data <- data[data$tier %in% input$tier_filters,]
    }
    
    return(data)
  })
  
  # Reactive expression for ownership counts
  ownership_counts <- reactive({
    data <- filteredData()
    num_fbo <- sum(data$FBO_ACCOUNT, na.rm = TRUE)
    num_public <- sum(data$PUBLIC_ACCOUNT, na.rm = TRUE)
    paste("<div style='font-size: 18px;'>",
          "<b>Number of Publicly Owned Lots:</b> ", num_public,"<br>",
          "<b>Number of Faith Based Owners:</b> ", num_fbo, "<br>")
  })
  
  # Render ownership counts
  output$ownership_counts <- renderUI({
    HTML(ownership_counts())
  })
  
  
  # Reactive value to store information of the clicked marker
  selected_marker <- reactiveVal(NULL)
  
  
  ## map output 
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = cpaltemplates::cpal_mapbox_color,
               attribution = cpaltemplates::cpal_leaflet) %>%
      addCircleMarkers(
        data = filteredData(),
        radius = 12,  
        color = ~case_when(
          FBO_ACCOUNT == TRUE ~ palette_cpal_main[5],
          PUBLIC_ACCOUNT == TRUE ~ palette_cpal_main[1]
        ),
        weight = 1,
        opacity = 1,
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions(maxClusterRadius = 50),
        layerId = ~GIS_PARCEL_ID,  # Use a unique identifier for each marker
        # Add an event listener for click events
        options = markerOptions(clickable = TRUE) 
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c(palette_cpal_main[5], palette_cpal_main[1]),
        labels = c("Publicly Owned Land", "FBO Owned Land"),
        title = "Legend"
      )
  })
  
  
  # Observe click events on map markers
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      # Find the selected marker's data and select only the first row
      marker_data <- filteredData() %>% filter(GIS_PARCEL_ID == click$id) %>% slice(1)
      selected_marker(marker_data)
    }
  })
  
  # Define UI element to show selected marker's information
  
  output$marker_info <- renderUI({
    data <- selected_marker()
    if (!is.null(data) && nrow(data) == 1) {
      # Initialize an empty vector to store the lines
      lines <- c()
      
      # Determine owner type and initialize features list
      owner_type <- ifelse(data$FBO_ACCOUNT, "<b>This vacant lot has a faith based owner,</b>", "<b>This lot is publicly owned,</b>")
      features <- c()
      if (data$School_Flag == TRUE) features <- c(features, "Schools")
      if (data$Grocery_Flag == TRUE) features <- c(features, "Grocery stores")
      if (data$Transit_Flag == TRUE) features <- c(features, "Public transportation")
      if (data$Clinic_Flag == TRUE) features <- c(features, "Clinics")
      if (data$Park_Flag == TRUE) features <- c(features, "Parks")
      if (data$Rec_Center_Flag == TRUE) features <- c(features, "Rec centers")
      if (data$Library_Flag == TRUE) features <- c(features, "Libraries")
      if (data$ENV_HAZ_Flag == TRUE) features <- c(features, "Environmental hazards")
      
      # Combine owner type and features in a single sentence
      if (length(features) > 0) {
        features_sentence <- paste("<b> with these features nearby:</b>", paste(features, collapse = ", "))
        owner_type <- paste(owner_type, features_sentence)
      }
      lines <- c(lines, owner_type)
      
      # Append owner name
      lines <- c(lines, paste0("<b>Owner Name:</b> ", data$OWNER_NAME1))
      
      # Append street address, removing NA values
      address_parts <- na.omit(c(
        data$STREET_NUM,
        paste0(data$FULL_STREET_NAME, ","),
        data$PROPERTY_CITY,
        data$PROPERTY_ZIPCODE
      ))
      address <- paste(address_parts, collapse = " ")
      lines <- c(lines, paste0("<b>Address:</b> ", address))
      
      # Append opportunity level
      lines <- c(lines, paste0("<b>Opportunity Level:</b> ", data$tier))
      
      # Combine lines into a single HTML string with line breaks
      HTML(paste0("<div style='font-size: 18px;'>", paste(lines, collapse = "<br>"), "</div>"))
    }
  })
  
  ## tier visualization      
  output$textBox <- renderUI({     
    HTML(
      paste0(
        "<div style='font-size: 18px;'>", 
        "<b>Opportunity levels of selected vacant lots:</b><br>",
        "<div style='font-size: 18px;'>",
        "<b>High opportunity:</b> ", sum(filteredData()$tier == "High"), " vacant lots<br>",
        "<b>Moderate opportunity:</b> ", sum(filteredData()$tier == "Moderate"), " vacant lots<br>",
        "<b>Low opportunity:</b> ", sum(filteredData()$tier == "Low"), " vacant lots",
        "<div style='font-size: 16px;'>", 
        "<br>",
        "<b>How opportunity levels are calculated:</b><br>",
        "<div style='font-size: 16px;'>",
        "<b>High Opportunity:</b> ", " 5 or more resources and no environmental hazards within 1/4 miles<br>",
        "<b>Moderate Opportunity:</b> ", " 1 to 4 resources and no environmental hazards within 1/4 miles<br>",
        "<b>Low Opportunity:</b> ", " No resources and/or environmental hazard within 1/4 miles",
        "</div>"
      )
    )
  })
  
  
  ## create apartment calcualtor
  
  
  # Slider input for number of stories
  output$stories_slider <- renderUI({
    sliderInput("stories", "Number of stories:", min = 1, max = 10, value = 5, step = 1)
  })
  
  output$squarefeet_input <- renderUI({
    numericInput("desired_sqft", "Square Feet Per Apartment:", value = 900)
  })

  ## calculate estimated apartments
  
  output$text2 <- renderUI({
    req(input$stories, input$desired_sqft)  
    
    # Calculate number of apartments based on selected number of stories and desired square feet
    num_stories <- input$stories
    desired_sqft <- input$desired_sqft
    filtered_data <- filteredData()
    
    # Filter data based on desired square feet
    filtered_data <- filtered_data[filtered_data$area >= desired_sqft, ]
    
    # Calculate total number of apartments
    total_area <- sum(filtered_data$area) * 0.8 # assuming 80% efficiency
    num_apartments <- (total_area / desired_sqft) * num_stories
    
    HTML(
      paste0(
        "<div style='font-size: 18px;'>",
        "Here is what can potentially be built:",
        "<div style='font-size: 18px;'>",
        "<b>Number of Apartments:</b> ", prettyNum(round(num_apartments, 0), big.mark = ","),
        "<br><b>Stories Selected:</b> ", num_stories,
        "<br><b>Square Feet Selected:</b> ", prettyNum(desired_sqft, big.mark = ","),
        "</div>"
      )
    )
  })   
  
  # Download handler for the table data
  output$download_table <- downloadHandler(
    filename = function() {
      paste("CPAL-Vacant-Lot-Data-Geography-Tool-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(area_data(), file, row.names = FALSE)
    }
  )
  
  # Add download handler for the main tool
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CPAL-DC-Vacant-Lot-Data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Remove the spatial data before writing to Excel
      filtered_data_no_geom <- st_drop_geometry(filteredData())
      
      # Load the data dictionary from the working directory
      data_dictionary <- read.csv("data/Data_Dictionary.csv")  
      
      # Create a list of data frames to write to Excel
      sheets <- list(
        "Vacant Lot Data" = filtered_data_no_geom,
        "Data Dictionary" = data_dictionary
      )
      
      # Write the list of data frames to an Excel file
      write_xlsx(sheets, path = file)
    }
  )
  
  # Reactive expression for area data
  area_data <- reactive({
    req(input$analysis_option)
    
    group_var <- input$analysis_option
    
    # Drop geometry column from filteredData()
    data <- st_drop_geometry(filteredData())
    
    # Filter out rows with NA values in the specified grouping variable column
    data <- data %>% filter(!is.na(!!sym(group_var)))
    
    # Group by selected option and calculate total and average area
    summarized_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarize(
        total_area = sum(area, na.rm = TRUE),
        average_area = mean(area, na.rm = TRUE)/ 43560,  # Convert to square miles
        unique_properties = n_distinct(ACCOUNT_NUM)  # Calculate number of unique properties
      ) %>%
      arrange(desc(total_area)) %>%
      ungroup()
    
    # Format total_area and average_area
    summarized_data$total_area <- format(round(summarized_data$total_area), big.mark = ",")
    summarized_data$average_area <- format(round(summarized_data$average_area, 3), big.mark = ",")
    
    return(summarized_data)
  })
  
  # Render the DataTable for area analysis
  output$area_table <- renderDT({
    req(area_data())
    # Rename columns
    renamed_data <- area_data() %>%
      rename(
        Location = !!sym(input$analysis_option),
        "Average Lot Size (Acres)" = average_area,
        "Number of Properties" = unique_properties
      )
    datatable(
      renamed_data %>%
        select(Location, "Average Lot Size (Acres)", "Number of Properties"),
      options = list(pageLength = 10),  
      rownames = FALSE
    )
  })
}



# Run the application
shinyApp(ui = ui, server = server)
