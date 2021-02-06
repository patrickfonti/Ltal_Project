#
# This is to plot data from the Loetchental project 
#

# add bslib thematic
material <- bs_theme(
  bg = "#202123", 
  fg = "#B8BCC2", #'white',
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


######### SHINY.UI #########
ui <- bootstrapPage(theme = material,
  tabsetPanel(id = 'tspan',
    
  # Home --------------------------------------------------------------------
    tabPanel('Home', icon=icon("home", lib = "font-awesome"), value = '#home',
      titlePanel("Research setting"),
      sidebarLayout( 
        sidebarPanel(),
        mainPanel(
          plotOutput(outputId = "ggplot.setting", height = "300px")
          ) 
        )
  ),
    
  # Campbell --------------------------------------------------------------------
    tabPanel('Campbell', icon=icon("phone-alt", lib = "font-awesome"), value = '#campbell',
      titlePanel("Campbell data from Lötschental sites"),
      sidebarLayout(
        sidebarPanel(
          # Select type of trend to plot
          selectizeInput(inputId = "site", label = strong("Site"),
            #choices = substr(names(DATA),5,9),
            choices = unlist(lapply(names(DATA), FUN=extract2)),
            selected = "N08b"),
          
          # Select date range to be plotted
          dateRangeInput("date", strong("Date range"), start = "2020-01-01", end = Sys.Date(),
            min = "2020-01-01", max = Sys.Date()) #"2021-07-31"
          
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
      )) ,
  # Dendrometer ----------------------------------------------------------------------
    tabPanel('Dendrometers', icon=icon("chart-line", lib = "font-awesome"),
      titlePanel("Dendrometer data 2007-2020"),
        sidebarLayout(
          sidebarPanel(
            # Select type of trend to plot
            selectizeInput(inputId = "siteD", label = strong("Site"),
              #choices = substr(names(DATA),5,9),
              choices = c(Setting$Site), #unlist(lapply(names(DENDRO), FUN=extract1)),
              selected = "N08"),
            
            # Select date range to be plotted
            dateRangeInput("dateD", strong("Date range"), start = "2018-01-01", end = "2020-12-31",
              min = "2006-01-01", max = Sys.Date()), #"2021-07-31"
            
            # Select species and dendrometer plotted
            radioButtons("species", "Select species", choices=c("L","S","both"), selected="both"),
            radioButtons("type", "Select type", choices=c("p","c","both"), selected="both"),
            
            # Select whether to scale
            checkboxInput(inputId = "scale", label = strong("Scale among dendrometers"), value = FALSE) #,

            # Display only if the smoother is checked
            # conditionalPanel(condition = "input.smoother == true",
            #   sliderInput(inputId = "f", label = "Smoother span:",
            #     min = 0.01, max = 1, value = 0.25, step = 0.01,
            #     animate = animationOptions(interval = 100)),
            #   HTML("Higher values give more smoothness.")
            # )
          ),
        mainPanel(plotOutput(outputId = "ggplot.DENDRO", height = "600px")) )
      )
  
    
) # END TABSET
  
)



# dygraphOutput(outputId = "dygraph", height = "300px"),
