#Kaia Truong
#A4 Climate Change

library(shiny)
library(ggplot2)
library(plotly)
co2_df <- read.csv(url("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"))
source("server.r")

# Define UI for application that draws a histogram

introductory_page <- tabPanel(
  "Introduction",
  titlePanel("Remarkable Values of Climate Change"),
  br(),
  h4("On this page, the values of which year and country have the highest and lowest value of CO2 gas and GHG will be reported to reflect their impact on our globe."),
  br(),
  h3("Five calculated values are: "), 
  br(),
  h5("The country with the highest CO2 per capita in the most recent year is", highest_co2_country),
  br(),
  h5("The country with the lowest CO2 per capita in the most recent year is", lowest_co2_country),
  br(),
  h5("The year with the highest CO2 per capita is", highest_co2_year),
  br(),
  h5("The average value of CO2 gas per capita across all countries recorded is ", average_co2),
  br(),
  h5("The average value of GHG value across all countries recorded is ", average_ghg),
  br(),
  h5("On the next tab, scatterplot graph will be generated based on user's choice of data. Overall, the graph will display an increasing trend of the chosen data over the years, which demonstrates the severity of climate change over time."),
  HTML('<center><img src="https://climate.nasa.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBHUT09IiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--61ad8d69aa8e56a92abda08438663af645be8fcb/cc-vs-gw-vs-wx-768px.jpg?disposition=attachment"></center>') 
)


analysis_page <- tabPanel(
  "Climate Change Data Graphing",
      h5("Control Panels"),
      column(
        width = 6,
        selectInput(
        inputId = "cont",
        label = "Choose the continent(s) that you wish to display data:",
        multiple = TRUE,
        choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
        selected = c("Asia", "Europe")
      )),
      column(
      width = 6,
      selectInput(
        inputId = "value", 
        label = "Choose the data you wish to display:",
        choices = c("Total GHG Emission" = 1, "Total CO2 Emission" = 2, "GHG Emission Per Capita" = 3, "CO2 Emission Per Capita" = 4),
        selected = 1
      )),
      br(),
      br(),
      br(),
      br(),
      br(),
      plotlyOutput(outputId = "plot"),
      h4("This scatterplot represents trend of the selected data over the years in selected continents. Here we can clearly see the uptrend relationship between the two despite selected data. The accumulation of total GHG and CO2 have a great impact on our planet as they those gases themselves trap heat and contribute to respiratory disease by smoke and air pollution."),
      br(),
      br(),
      numericInput(
            inputId = "obs",
            label = "Type in the year that you wish to display the distribution of total GHG and total CO2 around the world. (1990-2018)",
            value = 2018,
            min = min(small_df$year),
            max = max(small_df$year),
            step = 1,
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      plotlyOutput(outputId = "map"),
      br(),
      br(),
      br(),
      h4("This world map reports the country with the highest total GHG and CO2 value of the selected year. From 1990 to 2004, the U.S held its first place in term of GHG and CO2 gas emission, this reported value corresponding to the industrial evolution history of the country with its leading manufacturing record. Since 2005, Chinese featured the mass production by enormously expanding domestic market, which has made the accumulative GHG and CO2 gas emission surpassed the U.S. Even though these value reflects an interesting history of industrialization, the impact of those gases contributes to the climate change. Climate change is now a burning issue that needs to be resolved, and hopefully, the reported values on this page can help people acknowlege the negative impact that the GHG and CO2 emission footprint.")
)

ui <- navbarPage(
  "A4 Climate Change",
  introductory_page,
  analysis_page
)
