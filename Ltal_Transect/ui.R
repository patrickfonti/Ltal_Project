#
# This is to plot data from Campbell logger in the Loetchental. 
#

# Load packages
library(shiny)
library(shinythemes)
library(bslib)
library(thematic)
library(ggplot2)
# library(dygraphs)

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
  base_font = font_google("Open Sans"),
  heading_font = font_google("Proza Libre"),
  code_font = font_google("Fira Code")
)
# bs_theme_preview(material, with_themer = TRUE)

# Add thematic for ggplot
thematic_on(bg = '#202123', fg = 'white', accent = 'red', font = NA)



# Load data
#source('1.Import_FTP_data.R')
#DATA <- DATA
head(DATA)

# Define UI for application that draws a Dygraph plot
#ui <- fluidPage(theme = shinytheme("lumen"),
ui <- fluidPage(theme = material,
  titlePanel("Campbell from Lötschental sites"),
  sidebarLayout(
    sidebarPanel(
      
      # Select type of trend to plot
      selectizeInput(inputId = "site", label = strong("Site"),
        choices = substr(names(DATA),5,9),
        selected = "N13Da"),
      
      # Select date range to be plotted
      dateRangeInput("date", strong("Date range"), start = "2020-01-01", end = "2021-07-31",
        min = "2020-01-01", max = "2021-07-31"),
      
      # Select whether to overlay smooth trend line
      checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
      
      # Display only if the smoother is checked
      conditionalPanel(condition = "input.smoother == true",
        sliderInput(inputId = "f", label = "Smoother span:",
          min = 0.01, max = 1, value = 0.25, step = 0.01,
          animate = animationOptions(interval = 100)),
        HTML("Higher values give more smoothness.")
      )
    ),
    
    # Output: Description, lineplot, and reference
    mainPanel(
      plotOutput(outputId = "lineplot", height = "300px"),
      plotOutput(outputId = "ggplot", height = "300px"),
      tags$a(href = "https://www.wsl.ch/en/tree-ring-research/the-loetschental-tree-growth-monitoring-transect.html", "Source: Lötschental transect", target = "_blank")
    )
  )
)
