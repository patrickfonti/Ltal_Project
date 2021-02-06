#
# This is to plot data from Campbell logger in the Loetchental.
#


# Define server logic required to draw a dygraph plot
# Define server function
server <- function(input, output) {
  selected_batt <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    SITE.long.temp <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.character)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.numeric)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
      select(c(TIMESTAMP, starts_with("Batt_Volt"))) %>%
      pivot_longer(starts_with("Batt_Volt"), names_to = 'Sensor') %>%
      mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
      as.data.frame()
  })
   
  selected_temp <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    SITE.long.temp <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.character)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.numeric)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
      select(c(TIMESTAMP, starts_with("PanelTemp"))) %>%
      pivot_longer(starts_with("PanelTemp"), names_to = 'Sensor') %>%
      mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
      as.data.frame()
  })

    selected_site <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
    SITE.long.dendro <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.character)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.numeric)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
      select(c(TIMESTAMP, starts_with("Dendr"))) %>%
      pivot_longer(starts_with("Dendr"), names_to = 'Sensor') %>%
      mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
      as.data.frame() %>%
      group_by(Sensor) %>% 
      mutate(value = value - mean(value)) %>% 
      ungroup() %>% as.data.frame()
      })
    
    
    selected_siteD <- reactive({
      req(input$dateD)
      validate(need(!is.na(input$dateD[1]) & !is.na(input$dateD[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$dateD[1] < input$dateD[2], "Error: Start date should be earlier than end date."))
      
      SITE.long.DENDRO <- DENDRO %>% 
        select(Index, grep(input$siteD,names(DENDRO))) %>% 
        filter(Index > as.POSIXct(input$dateD[1]) & Index < as.POSIXct(input$dateD[2])) %>% 
        pivot_longer(cols=- Index, names_to = 'Sensor') %>% 
        mutate(value = as.numeric(as.double(value)), Sensor = factor(Sensor)) %>%
        group_by(Sensor) %>% 
        mutate(value = value - mean(value, na.rm=TRUE)) %>% 
        #  mutate(value = scale(value)) %>%
        ungroup() %>% as.data.frame() 
    })
    
    # SITE.long <- SITE %>%
    #   select(c(TIMESTAMP, starts_with("Dendr"))) %>%
    #   pivot_longer(starts_with("Dendr"), names_to = 'Sensor') %>%
    #   mutate(value = as.numeric(as.character(value)), Sensor = as.factor(Sensor)) %>%
    #   as.data.frame() %>%
    #   group_by(Sensor) %>%
    #   mutate(value = value - mean(value))

  selected_sap <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    
    SITE.long.sap <- DATA[grep(input$site,names(DATA))] %>% 
      as.data.frame(col.names = "") %>%
      mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.character)) %>%
      mutate_at(grep("Avg",colnames(.)),funs(as.numeric)) %>%
      filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
      select(c(TIMESTAMP, starts_with("Granier"))) %>%
      pivot_longer(starts_with("Granier"), names_to = 'Sensor') %>%
      mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
      as.data.frame() %>%
      group_by(Sensor) %>% 
      mutate(value = value - mean(value)) %>% 
      ungroup() %>% as.data.frame()
  })

  # Create ggplot object the plotOutput function is expecting
  output$ggplot.Batt <- renderPlot({
    withProgress(message = 'Calculation in progress',
      detail = 'This may take a while...', value = 0, {
        for (i in 1:5) {
          incProgress(1/5)
          Sys.sleep(0.1)
        }
      })
    
      ggplot(selected_batt(), aes(x=TIMESTAMP,y=value, colour=Sensor)) + 
        geom_line() + ylab("Voltage") + xlab("") + theme(legend.position="bottom")
    })

  # Create ggplot object the plotOutput function is expecting
  output$ggplot.Temp <- renderPlot({
       ggplot(selected_temp(), aes(x=TIMESTAMP,y=value, colour=Sensor)) + 
        geom_line() + ylab("Temp") + xlab("") + theme(legend.position="bottom")
   })

    # Create ggplot object the plotOutput function is expecting
  output$ggplot.Dendro <- renderPlot({
     ggplot(selected_site(), aes(x=TIMESTAMP,y=value, colour=Sensor)) + 
       geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
  })
  
  
  # Create ggplot object the plotOutput function is expecting
  output$ggplot.Sapflow <- renderPlot({
      ggplot(selected_sap(), aes(x=TIMESTAMP,y=value, colour=Sensor)) +
        geom_line() + ylab("Sap density") + xlab("") + theme(legend.position="bottom")
  })

  # Create ggplot object the plotOutput function is expecting
   output$ggplot.DENDRO <- renderPlot({
    if((input$species=="L" | input$species=="S") & (input$type=="c" | input$type=="p")) {
    ggplot(selected_siteD() %>% filter(grepl(paste0("_",input$species), Sensor)) %>% filter(grepl(input$type, Sensor)), aes(x= Index, y= value , colour=Sensor)) + 
      geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
    } else {
      if((input$species=="both") & (input$type=="c" | input$type=="p")) {
      ggplot(selected_siteD() %>% filter(grepl(input$type, Sensor)), aes(x= Index, y= value , colour=Sensor)) + 
        geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
      } else {
        if((input$species=="L" | input$species=="S") & input$type=="both") {
          ggplot(selected_siteD() %>% filter(grepl(paste0("_",input$species), Sensor)), aes(x= Index, y= value , colour=Sensor)) + 
            geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
        } else
          if((input$type=="both") & input$type=="both") {
            ggplot(selected_siteD(), aes(x= Index, y= value , colour=Sensor)) + 
              geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
          }
      }
    }
  })
  

  #   ########

  # Create ggplot object showing the setting
  
  output$ggplot.setting <- renderPlot({
    ggplot(Setting, aes(x=X, y=Elevation, col="orange")) +
      geom_line() + ylab("Elevation") + xlab("") +
      geom_label(aes(x=X, y = Elevation, label=Site)) + theme(legend.position = "none")
  })

}


# # Create Dygraph object the plotOutput function is expecting
# output$dygraph <- renderDygraph({
#   # selected_DY() %>% 
#   dygraph(selected_DY()) %>% dyRangeSelector(height=50) %>%
#     dyOptions(fillGraph = TRUE) 
# })

# selected_DY <- reactive({
#   req(input$date)
#   validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
#   validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
#   
#   SITE.DG %>% DATA[grep(input$site,names(DATA))] %>% 
#     as.data.frame(col.names = "") %>%
#     mutate(TIMESTAMP = as.POSIXct(TIMESTAMP)) %>%
#     mutate_at(grep("Avg",colnames(.)),funs(as.character)) %>%
#     mutate_at(grep("Avg",colnames(.)),funs(as.numeric)) %>%
#     filter(TIMESTAMP > as.POSIXct(input$date[1]) & TIMESTAMP < as.POSIXct(input$date[2])) %>% 
#     select(c('Dendr_Avg.1.', 'Dendr_Avg.2.','Dendr_Avg.3.', 'Dendr_Avg.4.')) %>%
#     xts(SITE[,-1:-2], order.by=SITE[,1])
#   
#   print(head(SITE.DG)) 
# })
# 
# SITE %>%
#  select(c('Dendr_Avg.1.', 'Dendr_Avg.2.','Dendr_Avg.3.', 'Dendr_Avg.4.')) %>%
#  xts(SITE[,-1:-2], order.by=SITE[,1]) %>%
#  dygraph() %>% dyRangeSelector(height=50) %>%
#  dyOptions(fillGraph = TRUE) %>%
#  dyShading(from = "1912-1-1", to = "2021-1-1", color = "black")

    
    
    
    
    
    
    
    