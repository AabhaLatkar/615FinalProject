# Load necessary libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)  # Add DT library for data tables

# Given demographic data
ethnic_groups_data <- data.frame(
  EthnicGroup = c("Palauan", "Carolinian", "Asian", "Other"),
  Percentage = c(70.6, 1.2, 26.5, 1.7)
)

languages_data <- data.frame(
  Language = c("Palauan", "Other Micronesian", "English", "Filipino", "Chinese", "Other"),
  Percentage = c(65.2, 1.9, 19.1, 9.9, 1.2, 2.8)
)

religions_data <- data.frame(
  Religion = c("Roman Catholic", "Protestant", "Modekngei", "Muslim", "Other"),
  Percentage = c(46.9, 30.9, 5.1, 4.9, 12.3)
)

age_structure_data <- data.frame(
  AgeGroup = c("0-14 years", "15-64 years", "65 years and over"),
  Percentage = c(17.49, 71.82, 10.69)
)

# Load Palau data (replace 'your_data_file.csv' with the actual file)
palau_all_states_education <- read_csv("palau all states education.csv", show_col_types = FALSE)
Palau_all_states_GDP <- read_csv("Palau all states GDP.csv", show_col_types = FALSE)

# Palau coordinates
palau_lat <- 7.5150
palau_lng <- 134.5825

# Another Palau marker coordinates
koror_lat <- 7.3419
koror_lng <- 134.4797

# Image URLs for Palau and Koror
palau_image_url <- "https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/thumbnails/image/Palau%20Islands%20in%20the%20Pacific%20Islands%20Region.jpg"
koror_image_url <- "https://i0.wp.com/www.pacificrisa.com/wp-content/uploads/2012/02/P2.-Patch-Reef-at-North-Babeldaob-by-Yimnang-Golbuu.jpg"

# New Palau marker image for the world map
new_palau_marker_image <- "https://www.usgs.gov/media/images/beach-great-lakes"

# SWOT Points
strengths_points <- c("Rich cultural diversity", "Strategic geopolitical location", "Robust economic potential")
weaknesses_points <- c("Limited natural resources", "Dependency on tourism", "Vulnerability to climate change")
opportunities_points <- c("Growing tourism industry", "International partnerships", "Investment in sustainable development")
threats_points <- c("Climate change impacts", "Economic dependence on specific sectors", "Global geopolitical uncertainties")

# Create a Shiny app
ui <- fluidPage(
  # Add inline CSS for a semi-transparent background image
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background: url(", palau_image_url, ") no-repeat center center fixed; 
          -webkit-background-size: cover;
          -moz-background-size: cover;
          -o-background-size: cover;
          background-size: cover;
          opacity: 2.0; /* Adjust the opacity as needed */
        }
        .sidebar-content {
          background: url(", palau_image_url, ") no-repeat center center fixed; 
          -webkit-background-size: cover;
          -moz-background-size: cover;
          -o-background-size: cover;
          background-size: cover;
          opacity: 2.0; /* Adjust the opacity as needed */
        }
        "
      )
    )
  ),
  
  titlePanel("Palau Overview"),
  
  sidebarLayout(
    sidebarPanel(
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General Overview", 
                 leafletOutput("world_map"),
                 leafletOutput("palau_map"),
                 # Add other relevant information
                 h3("Palau Project")
        ),
        
        tabPanel("Key Demographics", 
                 plotOutput("ethnic_groups_chart"),
                 plotOutput("languages_chart"),
                 plotOutput("religions_chart"),
                 plotOutput("age_structure_chart"),
                 textAreaInput(
                   "key_demographics_textbox_2",
                   "Additional Key Demographics Description",
                   "In this section, we delve into the diverse demographics that shape the cultural fabric of Palau. Understanding the population's composition is essential for appreciating the richness and variety of this Pacific island nation."  # Modified text
                 ),
                 tags$style(type="text/css", "#key_demographics_textbox_2 {color: black; background-color: white;}")  # White background
        ),
        
        tabPanel("Regional Comparison", 
                 plotOutput("regional_comparison"),
                 # Add other regional comparison visualizations
        ),
        
        tabPanel("SWOT Analysis", 
                 radioButtons("swot_aspect", "Select SWOT Aspect:",
                              choices = c("Strengths", "Weaknesses", "Opportunities", "Threats"),
                              selected = "Strengths"),
                 
                 # Output for dynamic SWOT analysis
                 dataTableOutput("swot_table"),
                 tags$style(type="text/css", ".dataTables_wrapper {background-color: white;}")  # White background for the table
        )
      )
    )
  )
)

server <- function(input, output) {
  # Output for the world map
  output$world_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = c(0, palau_lng, koror_lng), 
                 lat = c(0, palau_lat, koror_lat),
                 popup = c("World", "Palau", "Koror"),
                 icon = list(iconUrl = new_palau_marker_image, iconSize = c(50, 50))) %>%
      setView(lng = 0, lat = 0, zoom = 2)  # Center the map on the world
  })
  
  # Output for the Palau map
  output$palau_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = palau_lng, lat = palau_lat, popup = "Palau") %>%
      addMarkers(lng = koror_lng, lat = koror_lat, popup = "Koror") %>%
      setView(lng = palau_lng, lat = palau_lat, zoom = 10)  # Center the map on Palau
  })
  
  # Add popup for the Palau and Koror markers
  observe({
    leafletProxy("palau_map") %>%
      clearPopups() %>%
      addPopups(
        lng = palau_lng, lat = palau_lat, 
        popup = paste0("<img src='", palau_image_url, "' height='100' width='100'>")
      ) %>%
      addPopups(
        lng = koror_lng, lat = koror_lat, 
        popup = paste0("<img src='", koror_image_url, "' height='100' width='100'>")
      )
  })
  
  # Output for demographic charts
  output$ethnic_groups_chart <- renderPlot({
    ggplot(ethnic_groups_data, aes(x = EthnicGroup, y = Percentage, fill = EthnicGroup)) +
      geom_bar(stat = "identity") +
      labs(title = "Ethnic Groups in Palau", x = "Ethnic Group", y = "Percentage")
  })
  
  output$languages_chart <- renderPlot({
    ggplot(languages_data, aes(x = Language, y = Percentage, fill = Language)) +
      geom_bar(stat = "identity") +
      labs(title = "Languages in Palau", x = "Language", y = "Percentage")
  })
  
  output$religions_chart <- renderPlot({
    ggplot(religions_data, aes(x = Religion, y = Percentage, fill = Religion)) +
      geom_bar(stat = "identity") +
      labs(title = "Religions in Palau", x = "Religion", y = "Percentage")
  })
  
  output$age_structure_chart <- renderPlot({
    ggplot(age_structure_data, aes(x = AgeGroup, y = Percentage, fill = AgeGroup)) +
      geom_bar(stat = "identity") +
      labs(title = "Age Structure in Palau", x = "Age Group", y = "Percentage")
  })
  
  # Output for regional comparison
  output$regional_comparison <- renderPlot({
    
    # Create a bar chart
    ggplot(Palau_all_states_GDP, aes(x = `Growth rate of per capita GDP`, y = RegionalMember, fill = "yellow")) +
      geom_bar(stat = "identity") +
      labs(title = "Regional Comparison",
           x = "Growth Rate of Per Capita GDP", y = "Regional Member Member Economy") +
      scale_fill_manual(values = "yellow")  # Set bar color to yellow
  })
  
  
  # Output for dynamic SWOT analysis
  output$swot_table <- renderDataTable({
    # Example: Create a dynamic table based on the selected SWOT aspect
    swot_data <- switch(input$swot_aspect,
                        "Strengths" = data.frame(Point = strengths_points),
                        "Weaknesses" = data.frame(Point = weaknesses_points),
                        "Opportunities" = data.frame(Point = opportunities_points),
                        "Threats" = data.frame(Point = threats_points))
    
    # Render the table
    datatable(swot_data, options = list(dom = 't'))
  })
}

shinyApp(ui, server)
