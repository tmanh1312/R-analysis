# Summary values File
# Kaia Truong
# INFO201
# A4 Climate Change

library("dplyr")
library("htmltools")
library("plotly")
library("ggplot2")

co2_df <- read.csv(url("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"))
# install.packages ("map")
world <- map_data("world")
#Which country have the highest value of my Co2 per capita 
#in the current year?
highest_co2_country <- co2_df %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>%
  select(country)    

#Which country have the lowest value of my Co2 per capita 
#in the current year?
lowest_co2_country <- co2_df %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE)) %>%
  select(country) 

#Which year has the highest value of CO2 per capita?
highest_co2_year <- co2_df %>%
  filter(year == year) %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>%
  select(year)

#What is the average Co2 per capita value
#across all countries recorded?
average_co2 <- co2_df %>%
  filter(co2_per_capita == co2_per_capita) %>%
  summarise(
    average_co2_per_capita = mean(co2_per_capita)
  ) 

#What is the average GHG value
#across all countries recorded?
average_ghg <- co2_df %>%
  filter(total_ghg == total_ghg) %>%
  summarise(
    average_ghg = mean(total_ghg)
  ) 

# Add continent column to dataframe
# install.packages("countrycode") in console
library(countrycode)
#df <- data.frame(country = c(co2_df$country))
co2_df$continent <- countrycode(sourcevar = co2_df$country,
                                origin = "country.name",
                                destination = "continent")
# Subset smaller df
small_df <- co2_df %>%
  filter(total_ghg == total_ghg, na.rm = TRUE)  %>%
  filter(ghg_per_capita == ghg_per_capita, na.rm = TRUE)  %>%
  filter(co2 == co2, na.rm = TRUE)  %>%
  filter(co2_per_capita == co2_per_capita, na.rm = TRUE)  %>%
  select(year, iso_code, country, continent, co2, co2_per_capita, total_ghg, ghg_per_capita)

# Define server logic 
server <- function(input, output) {
  # Reactive data frame for plot
  plot_data <- reactive ({
    subset(small_df,
           continent %in% input$cont & year == year)
  })
    
  # Reactive data frame for table
  filtered_data <- reactive ({
    subset(small_df,
             year == input$obs) %>%
    group_by(iso_code)
  })
  # Reactive scatterplot 
  output$plot <- renderPlotly({
    if (input$value == 1){
    ggplotly({
      ggplot(plot_data(),
            aes_string(x = "year", y = "total_ghg", color = "continent")) + 
            geom_point(alpha = 0.5) + 
            theme(legend.position = "none") + 
            labs(y= "Total GHG Emission",
                 x = "Year"
                 )
    })
    } else if (input$value == 2){
      ggplotly({
        ggplot(plot_data(),
               aes_string(x = "year", y = "co2", color = "continent")) + 
          geom_point(alpha = 0.5) + 
          theme(legend.position = "none") + 
          labs(y= "Total CO2 Emission",
               x = "Year"
          )
      })
    } else if (input$value == 3){
      ggplotly({
        ggplot(plot_data(),
               aes_string(x = "year", y = "ghg_per_capita", color = "continent")) + 
          geom_point(alpha = 0.5) + 
          theme(legend.position = "none") + 
          labs(y= "GHG Emission Per Capita",
               x = "Year"
          )
      })
    } else {
      ggplotly({
        ggplot(plot_data(),
               aes_string(x = "year", y = "co2_per_capita", color = "continent")) + 
          geom_point(alpha = 0.5) + 
          theme(legend.position = "none") + 
          labs(y= "CO2 Emission Per Capita",
               x = "Year"
          )
      })
    }
  })
  
  # Reactive world map
  world_map <- function(df) {
    plot_geo(df) %>%
      add_trace(
        hoverinfo = "text", z = ~total_ghg, color = ~total_ghg,colors = "Purples",
        text = paste(df$country, "<br>",
                     "Total GHG:",
                     (df$total_ghg), "<br>",
                     "Total CO2:",
                     (df$co2), "<br>"
                     ), 
        locations = ~iso_code,
        zmin=0,
        zmax=12000,
        marker = list(line = list(color = "white", width = 0.5))
      ) %>%
      colorbar(title = "Total GHG") %>%
      layout(geo = list(
        projection = list(type = "Mercator")))
  }
  output$map <- renderPlotly ({
    ggplotly({
      world_map(filtered_data())
    })
  })
  # Reactive data table
  output$table <- renderTable({
    return(filtered_data())
  })
  
  # Image on intro page
  output$image <- renderUI({ 
    tags$img(src = "https://climate.nasa.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBHUT09IiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--61ad8d69aa8e56a92abda08438663af645be8fcb/cc-vs-gw-vs-wx-768px.jpg?disposition=attachment") 
  }) 
  
}





