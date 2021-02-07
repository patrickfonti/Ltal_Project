# Campbell

# BATTERY
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


# TEMPERATURE
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


# DENDROMETER
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


# SAPFLOW
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



# ggplot BATTERY
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


# ggplot TEMPERATURE
output$ggplot.Temp <- renderPlot({
  ggplot(selected_temp(), aes(x=TIMESTAMP,y=value, colour=Sensor)) + 
    geom_line() + ylab("Temp") + xlab("") + theme(legend.position="bottom")
})


# ggplot DENDROMETERS
output$ggplot.Dendro <- renderPlot({
  ggplot(selected_site(), aes(x=TIMESTAMP,y=value, colour=Sensor)) + 
    geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
})


# ggplot SAPFLOW
output$ggplot.Sapflow <- renderPlot({
  ggplot(selected_sap(), aes(x=TIMESTAMP,y=value, colour=Sensor)) +
    geom_line() + ylab("Sap density") + xlab("") + theme(legend.position="bottom")
})