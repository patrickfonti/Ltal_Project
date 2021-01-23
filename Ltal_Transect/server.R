#
# This is to plot data from Campbell logger in the Loetchental.
#

library(shiny)

# Define server logic required to draw a dygraph plot
# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_dendro <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    SITE <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2]))
  })
   
  selected_site <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
    SITE.long <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
      select(c(TIMESTAMP, starts_with("Dendr"))) %>%
      pivot_longer(starts_with("Dendr"), names_to = 'Sensor') %>%
      mutate(value = as.numeric(as.character(value)), Sensor = as.factor(Sensor)) %>%
      as.data.frame() %>%
      group_by(Sensor) %>% 
      mutate(value = value - mean(value))
  })

  # SITE <- DATA[grep('N08b_',names(DATA))] %>%
  #   as.data.frame(col.names = "") %>%
  #   mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
  #   filter(TIMESTAMP > as.POSIXct('2020-01-01') & TIMESTAMP < as.POSIXct('2021-12-31'))
  # 
  # 
  #   SITE.long <- SITE %>%
  #     select(c(TIMESTAMP, starts_with("Dendr"))) %>%
  #     pivot_longer(starts_with("Dendr"), names_to = 'Sensor') %>%
  #     mutate(value = as.numeric(as.character(value)), Sensor = as.factor(Sensor)) %>%
  #     as.data.frame() %>%
  #     group_by(Sensor) %>% 
  #     mutate(value = value - mean(value))
  #     
      
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = 'white' #"#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_dendro()$TIMESTAMP, y = selected_dendro()$Dendr_Avg.1., type = "l",
      xlab = "Date", ylab = "Delta R", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_dendro()$TIMESTAMP), y = selected_dendro()$Dendr_Avg.1., f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })

  # Create ggplot object the plotOutput function is expecting
  output$ggplot <- renderPlot({
   if(input$smoother){
     ggplot(selected_site(), aes(x=TIMESTAMP,y=value, colour=Sensor)) +
       geom_line() +
       geom_smooth(span = input$f)
   } else {
     ggplot(selected_site(), aes(x=TIMESTAMP,y=value, colour=Sensor)) +
       geom_line()
         }
  })
    
}