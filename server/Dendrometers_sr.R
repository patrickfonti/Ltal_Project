# Dendrometers 2006-2020


# DENDROMETER
selected_siteD <- reactive({
  SITE.long.DENDRO <- DENDRO %>%
    select(Index, grep(input$siteD,names(DENDRO))) %>%
    filter(Index > as.POSIXct(paste0("01",input$slider[1]),format="%d%b%Y",tz="UTC") & Index < as.POSIXct(paste0("25",input$slider[2]),format="%d%b%Y",tz="UTC")) %>%
    pivot_longer( -Index, names_to = 'Sensor') %>%
    mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
    group_by(Sensor) %>%
    #mutate(value = value - mean(value)) %>%
    ungroup()
  
})


# CYCLE
selected_cycle <- reactive({
  DENDRO.cycle <- DENDRO %>% 
    select(Index, grep(input$siteD,names(DENDRO))) %>% 
    filter(Index > as.POSIXct(paste0("01",input$slider[1]),format="%d%b%Y",tz="UTC") & Index < as.POSIXct(paste0("25",input$slider[2]),format="%d%b%Y",tz="UTC")) %>% 
    pivot_longer(- Index, names_to = 'Sensor') %>% 
    mutate(value = as.numeric(as.double(value)), Sensor = factor(Sensor)) %>%
    group_by(Sensor) %>% 
    # mutate(value = value - mean(value)) %>% 
    ungroup() %>%
    mutate(
      h = as.numeric(format(as.POSIXct(Index), format="%H",tz="CET")),
      d = as.numeric(format(as.POSIXct(Index), "%d",tz="CET")),
      m = ordered(as.factor(format(as.POSIXct(Index), "%b",tz="CET")),month.abb),
      year = as.numeric(format(as.POSIXct(Index), "%Y",tz="CET")),
      doy = as.numeric(format(as.POSIXct(Index), "%j",tz="CET")),
      species = substr(as.character(Sensor), nchar(as.character(Sensor))-2,nchar(as.character(Sensor))-2),
      type = substr(as.character(Sensor), nchar(as.character(Sensor)),nchar(as.character(Sensor)))) %>%             
    group_by(Sensor,year,doy,species,type) %>%
    mutate(centered_value = value-mean(value)) %>%
    ungroup() %>% 
    group_by(Sensor,h,m,species,type) %>%
    summarise(value = mean(centered_value,na.rm=TRUE)) %>%
    ungroup()
  print(DENDRO.cycle)
})



# ggplot DENDROMETERS
output$ggplot.DENDRO <- renderPlot({
#   ggplot(selected_siteD() %>% filter(grepl(paste0("_",input$species), Sensor)) %>% filter(grepl(input$type, Sensor)), aes(x= Index, y= value , colour=Sensor)) + 
#     geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
  
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


# ggplot CYCLES
output$ggplot.CYCLE <- renderPlot({
  if((input$species=="L" | input$species=="S") & (input$type=="c" | input$type=="p")) {
    ggplot(selected_cycle() %>% filter(grepl(paste0("_",input$species), Sensor)) %>% filter(grepl(input$type, Sensor)), aes(x=h, y=value, colour = Sensor)) +
      geom_line( size = 0.6) +
      facet_grid(type+species ~ m) +
      scale_y_continuous("meanDr") +
      scale_x_discrete("hours", breaks=c(6,12,18)) + theme(legend.position="bottom")
  } else {
    if((input$species=="both") & (input$type=="c" | input$type=="p")) {
      ggplot(selected_cycle() %>% filter(grepl(input$type, Sensor)), aes(x=h, y=value, colour = Sensor)) +
        geom_line( size = 0.6) +
        facet_grid(type+species ~ m) +
        scale_y_continuous("meanDr") +
        scale_x_discrete("hours", breaks=c(6,12,18)) + theme(legend.position="bottom")
    } else {
      if((input$species=="L" | input$species=="S") & input$type=="both") {
        ggplot(selected_cycle() %>% filter(grepl(paste0("_",input$species), Sensor)), aes(x=h, y=value, colour = Sensor)) +
          geom_line( size = 0.6) +
          facet_grid(type+species ~ m) +
          scale_y_continuous("meanDr") +
          scale_x_discrete("hours", breaks=c(6,12,18)) + theme(legend.position="bottom")
      } else
        if((input$type=="both") & input$type=="both") {
          ggplot(selected_cycle(), aes(x=h, y=value, colour = Sensor)) +
            geom_line( size = 0.6) +
            facet_grid(type+species ~ m) +
            scale_y_continuous("meanDr") +
            scale_x_discrete("hours", breaks=c(6,12,18)) + theme(legend.position="bottom")
        }
    }
  }
})
