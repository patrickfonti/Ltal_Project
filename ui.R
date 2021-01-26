#
# This is to plot data from Campbell logger in the Loetchental. 
#

# remotes::install_github("rstudio/bslib")
# remotes::install_github("rstudio/shiny")
# remotes::install_github("rstudio/thematic")
# remotes::install_github("rstudio/dygraphs")

# Load packages
library(shiny)
library(shinythemes)
library(bslib)
library(thematic)
library(ggplot2)
library(tidyr)

# add bslib thematic
material <- bs_theme(
  bg = "#202123", 
  fg = 'white', #"#B8BCC2", 
  primary = "#EA80FC", 
  secondary = "#00DAC6",
  success = "#4F9B29",
  info = "#28B3ED",
  warning = "#FD7424",
  danger = "#F7367E",
  base_font = font_google("Indie Flower"), #"Open Sans"
  heading_font = font_google("Indie Flower"), #"Proza Libre"
  code_font = font_google("Indie Flower") #"Fira Code"
)
# bs_theme_preview(material, with_themer = TRUE)

# Add thematic for ggplot
thematic_on(bg = '#202123', fg = 'white', accent = 'red', font = "Indie Flower")



# Load data
source('1.Import_FTP_data.R')
#DATA <- DATA
#head(DATA)

extract <- function(x) {unlist(strsplit((x),'_'))[[2]]}

# Define UI for application that draws a Dygraph plot
# ui <- fluidPage(theme = shinytheme("lumen"),
ui <- fluidPage(theme = material,
  titlePanel("Campbell data from Lötschental sites"),
  sidebarLayout(
    sidebarPanel(
      
      # Select type of trend to plot
      selectizeInput(inputId = "site", label = strong("Site"),
        #choices = substr(names(DATA),5,9),
        choices = unlist(lapply(names(DATA), FUN=extract)),
        selected = "N08b"),
      
      # Select date range to be plotted
      dateRangeInput("date", strong("Date range"), start = "2020-01-01", end = "2021-07-31",
        min = "2020-01-01", max = "2021-07-31"),
      
      # # Select whether to overlay smooth trend line
      # checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
      # 
      # # Display only if the smoother is checked
      # conditionalPanel(condition = "input.smoother == true",
      #   sliderInput(inputId = "f", label = "Smoother span:",
      #     min = 0.01, max = 1, value = 0.25, step = 0.01,
      #     animate = animationOptions(interval = 100)),
      #   HTML("Higher values give more smoothness.")
      # )
    ),
    
    # Output: Description, lineplot, and reference
    mainPanel(
      plotOutput(outputId = "ggplot.Batt", height = "150px"),
      plotOutput(outputId = "ggplot.Temp", height = "150px"),
      plotOutput(outputId = "ggplot.Dendro", height = "250px"),
      plotOutput(outputId = "ggplot.Sapflow", height = "250px"),
      tags$a(href = "https://www.wsl.ch/en/tree-ring-research/the-loetschental-tree-growth-monitoring-transect.html", "Source: Lötschental transect", target = "_blank")
    )
  )
)


# dygraphOutput(outputId = "dygraph", height = "300px"),
