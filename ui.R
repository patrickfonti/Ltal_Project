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
thematic_on(bg = '#202123', fg = 'white', accent = 'auto', font = font_spec("Indie Flower", scale = 2))


######### SHINY.UI #########
ui <- fluidPage(
  theme = material,
  tabsetPanel(id = 'tspan',
    
  # Home --------------------------------------------------------------------
    tabPanel('Home', icon=icon("home", lib = "font-awesome"), value = '#home',
      titlePanel("Research setting"),
      sidebarLayout( 
        sidebarPanel(),
        mainPanel(
          plotlyOutput(outputId = "ggplot.setting", height = "300px"),
          verbatimTextOutput("plotly_click"),
          tags$a(href = "https://www.wsl.ch/en/tree-ring-research/the-loetschental-tree-growth-monitoring-transect.html", "Source: Lötschental transect", target = "_blank")
          ) 
        )
  ),
    
  # Campbell --------------------------------------------------------------------
    tabPanel('Campbell', icon=icon("phone-alt", lib = "font-awesome"), value = '#campbell',
      titlePanel("Campbell data from Lötschental sites"),
      sidebarLayout(
        sidebarPanel(
          # Select type of trend to plot
          selectizeInput(inputId = "site", label = strong("Site"), choices = unlist(lapply(names(DATA), FUN=extract2)), selected = "N08b"),
          
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
          plotOutput(outputId = "ggplot.Sapflow", height = "250px")
        )
      )) ,
    
  # Dendrometers ----------------------------------------------------------------------
    tabPanel('Dendrometers', icon=icon("chart-line", lib = "font-awesome"),
      titlePanel("Dendrometer data 2007-2020"),
        sidebarLayout(
          sidebarPanel(
            # Select type of trend to plot
            selectizeInput(inputId = "siteD", label = strong("Site"), choices = c(Setting$Site)[c(10,7,8,3,2,1,11,12,14,9,4,5,6,13)], selected = "N08"),
            
            # # Select date range to be plotted
            # dateRangeInput("dateD", strong("Date range"), start = "2018-01-01", end = "2020-12-31",
            #   min = "2006-01-01", max = Sys.Date()), #"2021-07-31"
            
            # Select species and dendrometer plotted
            radioButtons("species", strong("Select species"), choices=c("L","S","both"), selected="L"),
            radioButtons("type", strong("Select type"), choices=c("p","c","both"), selected="p"),
            
            # Select whether to scale
            checkboxInput(inputId = "showTWD", label = strong("Show TWD"), value = FALSE),
            checkboxInput(inputId = "showdaily", label = strong("Show Daily values"), value = FALSE),

            # Display only if the showdaily is checked
            conditionalPanel(condition = "input.showdaily == true",
              selectizeInput(inputId = "Parameter", label = strong("Select daily parameter"), choices = c("amplitude", "time_min","time_max", "min", "max"), selected = "amplitude"),
              checkboxGroupInput(inputId = "Sensor", label = strong("Select Tree"), choices = ""), # levels(DAILY.DATA$dmID), selected = DAILY.DATA$dmID[1]),
              conditionalPanel(condition = "input.showdaily == true",
                checkboxGroupInput(inputId = "Year", label = strong("Select year(s)"),choices = "")   #levels(DAILY.DATA$year)
              ))
                
            
          ),
          
        mainPanel(
          shinyWidgets::sliderTextInput(inputId = "slider", label = "Time",
            choices    = unique(format(DENDRO$Index, format="%b%Y")),
            selected   = c("Jan2018", max(format(DENDRO$Index, format="%b%Y"))), # min(format(DENDRO$Index, format="%b%Y"))
            grid = FALSE, width = "100%"),
          plotOutput(outputId = "ggplot.DENDRO", height = "500px"),
          # Display only if TWD is checked
          conditionalPanel(condition = "input.showTWD == true", plotOutput(outputId = "ggplot.TWD", height = "300px")),
          plotOutput(outputId = "ggplot.CYCLE", height = "300px"),
          # Display only if TWD is checked
          conditionalPanel(condition = "input.showdaily == true", plotOutput(outputId = "ggplot.AMPLITUDE", height = "300px"))
          # Select slider date range to be plotted
          
          ) )
          
      ) # END TAB PANEL
  
    
) # END TABSET
  
) # END UI

