# Dendrometers 2006-2020

# DENDROMETER
selected_siteD <- reactive({
  SITE.long.DENDRO <- DENDRO %>%
    select(Index, grep(input$siteD,names(DENDRO))) %>%
    filter(Index > as.POSIXct(paste0("01",input$slider[1]),format="%d%b%Y",tz="UTC") & Index < as.POSIXct(paste0("28",input$slider[2]),format="%d%b%Y",tz="UTC")) %>%
    pivot_longer( -Index, names_to = 'Sensor') %>%
    mutate(value = as.numeric(as.character(value)), Sensor = factor(Sensor)) %>%
    group_by(Sensor) %>%
    mutate(value = scale(value, scale=FALSE)) %>%
    ungroup() %>% 
    filter(!is.na(value))
   # print(names(DENDRO))
})

###### TWD and GROWTH 
selected_TWD <- reactive({
  L.LONG <- DENDRO %>% 
    select(Index, grep(input$siteD,names(DENDRO))) %>% 
    filter(Index > as.POSIXct(paste0("01",input$slider[1]),format="%d%b%Y",tz="UTC") & Index < as.POSIXct(paste0("28",input$slider[2]),format="%d%b%Y",tz="UTC")) %>% 
    pivot_longer(- Index, names_to = 'Sensor') %>% 
    mutate(value = as.numeric(as.double(value)), Sensor = factor(Sensor)) %>%
    mutate(
      h = as.numeric(format(as.POSIXct(Index), format="%H",tz="CET")),
      d = as.numeric(format(as.POSIXct(Index), "%d",tz="CET")),
      m = ordered(as.factor(format(as.POSIXct(Index), "%b",tz="CET")),month.abb),
      year = as.numeric(format(as.POSIXct(Index), "%Y",tz="CET")),
      doy = as.numeric(format(as.POSIXct(Index), "%j",tz="CET")),
      species = substr(as.character(Sensor), nchar(as.character(Sensor))-2,nchar(as.character(Sensor))-2),
      type = substr(as.character(Sensor), nchar(as.character(Sensor)),nchar(as.character(Sensor)))) %>%  
    group_by(Sensor,year,doy,species,type) %>%
    mutate(centered_value = value-min(value)) %>%
    ungroup() %>% 
    filter(!is.na(value)) %>%
  group_by(Sensor, year) %>%
  mutate(cuMax = cummax(value), 
    growth.inc = c(diff(lag(cuMax)),NA), 
    growth.cum = cumsum(ifelse(is.na(growth.inc), 0, growth.inc)) + growth.inc*0,
    TWD:=value-cuMax) %>% 
  ungroup() 
})


# CYCLE
selected_cycle <- reactive({
  DENDRO.cycle <- DENDRO %>% 
    select(Index, grep(input$siteD,names(DENDRO))) %>% 
    filter(Index > as.POSIXct(paste0("01",input$slider[1]),format="%d%b%Y",tz="UTC") & Index < as.POSIXct(paste0("28",input$slider[2]),format="%d%b%Y",tz="UTC")) %>% 
    pivot_longer(- Index, names_to = 'Sensor') %>% 
    mutate(value = as.numeric(as.double(value)), Sensor = factor(Sensor)) %>%
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
    ungroup() %>% 
    filter(!is.na(value))
})


# DAILY DENDRO
selected_dailyDendro <- reactive({
  DENDRO.daily <- DAILY.DATA %>% 
    filter(grepl(input$siteD,dmID)) %>% 
    filter(date >= as.Date(paste0("01",input$slider[1]),format="%d%b%Y") & date <= as.Date(paste0("28",input$slider[2]),format="%d%b%Y")) %>% 
    # filter(date > as.POSIXct(paste0("01","Apr2007",format="%d%b%Y",tz="UTC")) & date < as.POSIXct(paste0("25",input$slider[2]),format="%d%b%Y",tz="UTC")) %>% 
    # filter(date >= as.Date(input$slider[1]) & date <= as.Date(input$slider[2])) %>% 
    mutate(time_min = as.numeric(format(as.POSIXct(time_min), format="%H")),
           time_max = as.numeric(format(as.POSIXct(time_max), format="%H"))) %>% 
    pivot_longer(cols = min:time_max, names_to = 'Parameter') %>% 
    mutate(species = substr(as.character(dmID), nchar(as.character(dmID))-2,nchar(as.character(dmID))-2),
      type = substr(as.character(dmID), nchar(as.character(dmID)),nchar(as.character(dmID))),
      year = substr(date,1,4)) %>% 
    filter(!is.na(value))
  
})







################## PLOTTING ###########


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


# ggplot TWD
output$ggplot.TWD <- renderPlot({
#  if(input$showTWD == TRUE) {
  if((input$species=="L" | input$species=="S") & (input$type=="c" | input$type=="p")) {
    ggplot(selected_TWD() %>% filter(grepl(paste0("_",input$species), Sensor)) %>% filter(grepl(input$type, Sensor)), aes(x=Index, y=TWD, colour=Sensor)) +
      geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
  } else {
    if((input$species=="both") & (input$type=="c" | input$type=="p")) {
      ggplot(selected_TWD() %>% filter(grepl(input$type, Sensor)), aes(x=Index, y=TWD, colour=Sensor)) +
        geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
    } else {
      if((input$species=="L" | input$species=="S") & input$type=="both") {
        ggplot(selected_TWD() %>% filter(grepl(paste0("_",input$species), Sensor)), aes(x=Index, y=TWD, colour=Sensor)) +
          geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
      } else
        if((input$type=="both") & input$type=="both") {
          ggplot(selected_TWD(), aes(x=Index, y=TWD, colour=Sensor)) +
            geom_line() + ylab("DeltaR") + xlab("") + theme(legend.position="bottom")
        }
    }
  }
#    }
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


# ggplot AMPLITUDE
output$ggplot.AMPLITUDE <- renderPlot({
  print(input$Sensor)
  selected.trees <- selected_dailyDendro() %>% filter(dmID %in% input$Sensor) %>% filter(year %in% input$Year) %>% filter(!is.na(value))
  print(selected.trees)
  
  if((input$species=="L" | input$species=="S") & (input$type=="c" | input$type=="p")) {
    ggplot(selected.trees %>% select(Parameter, DOY,value,dmID,year) %>% filter(grepl(paste0("_",input$species), dmID),grepl(input$type, dmID), Parameter == input$Parameter)) + 
      geom_col(aes(x = DOY, y = value, group = dmID, fill = year),position = "dodge", alpha=0.9) + facet_grid(.~ dmID) +
      scale_x_continuous(breaks = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))-31, labels = month.abb, limits = c(0,366)) +
      coord_polar() + 
      theme(legend.position="bottom") + 
      ggtitle(input$Parameter)
  } else {
    if((input$species=="both") & (input$type=="c" | input$type=="p")) {
      ggplot(selected.trees %>% select(Parameter, DOY,value,dmID,year) %>% filter(grepl(input$type, dmID),Parameter == input$Parameter)) + 
  geom_col(aes(x = DOY, y = value, group = dmID, fill = year),position = "dodge", alpha=0.9) + facet_grid(.~ dmID) +
  scale_x_continuous(breaks = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))-31, labels = month.abb, limits = c(0,366)) +
  coord_polar() + 
  theme(legend.position="bottom") + 
  ggtitle(input$Parameter)
    } else {
      if((input$species=="L" | input$species=="S") & input$type=="both") {
        ggplot(selected.trees %>% select(Parameter, DOY,value,dmID,year) %>% filter(grepl(paste0("_",input$species),dmID),Parameter == input$Parameter)) + 
          geom_col(aes(x = DOY, y = value, group = dmID, fill = year),position = "dodge", alpha=0.9) + facet_grid(.~ dmID) +
          scale_x_continuous(breaks = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))-31, labels = month.abb, limits = c(0,366)) +
          coord_polar() + 
          theme(legend.position="bottom") + 
          ggtitle(input$Parameter)
      } else
        if((input$type=="both") & input$species=="both") {
          ggplot(selected.trees %>% select(Parameter, DOY,value,dmID,year) %>% filter(Parameter == input$Parameter)) + 
            geom_col(aes(x = DOY, y = value, group = dmID, fill = year),position = "dodge", alpha=0.9) + facet_grid(.~ dmID) +
            scale_x_continuous(breaks = cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31))-31, labels = month.abb, limits = c(0,366)) +
            coord_polar() + 
            theme(legend.position="bottom") + 
            ggtitle(input$Parameter)
        }
    }
  }
})




