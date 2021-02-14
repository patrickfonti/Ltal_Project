#
# This is to plot data from Campbell logger in the Loetchental.
#


# Define server logic required to draw a dygraph plot
# Define server function
server <- function(input, output, session) {
  
  # Sourcing tab ------------------------------------------------------------
  source('server/Home_sr.R', local = T)
  source('server/Campbell_sr.R', local = T)
  source('server/Dendrometers_sr.R', local = T)
  
  # Update Checkbox tree
  TreeVar = reactive({
    if((input$species=="L" | input$species=="S") & (input$type=="c" | input$type=="p")) {
      mydata = selected_dailyDendro() %>% filter(!is.na(value)) %>% select(dmID) %>% filter(grepl(paste0("_",input$species), dmID), grepl(input$type, dmID)) %>% distinct() %>% unlist() %>% as.vector()
    }
    #mydata = unique(selected_dailyDendro()$dmID)}
    else {
      if((input$species=="both") & (input$type=="c" | input$type=="p")) {
      mydata = selected_dailyDendro() %>% filter(!is.na(value)) %>% select(dmID) %>%  filter(grepl(input$type, dmID)) %>% distinct(dmID) %>% unlist() %>% as.vector()
      } else {
        if((input$species=="L" | input$species=="S") & input$type=="both") {
          mydata = selected_dailyDendro() %>% filter(!is.na(value)) %>% select(dmID) %>% filter(grepl(paste0("_",input$species), dmID)) %>% distinct(dmID) %>% unlist() %>% as.vector()
        } else 
          if(input$type=="both" & input$species=="both") {
            mydata = selected_dailyDendro() %>% filter(!is.na(value)) %>% select(dmID) %>% distinct(dmID) %>% unlist() %>% as.vector()
          }
    }}
    })
  observe({ updateCheckboxGroupInput(session, inputId ="Sensor", choices = TreeVar(), selected = TreeVar() )})
  
  # Update Checkbox year
  YearVar = reactive({ mydata = unique(selected_dailyDendro()$year)})
  observe({ updateCheckboxGroupInput(session, inputId ="Year", choices = YearVar(), selected = YearVar() )})
  
  # Session end -------------------------------------------------------------
#  session$onSessionEnded(stopApp)     
}   
    
    
    
    # SITE.long <- SITE %>%
    #   select(c(TIMESTAMP, starts_with("Dendr"))) %>%
    #   pivot_longer(starts_with("Dendr"), names_to = 'Sensor') %>%
    #   mutate(value = as.numeric(as.character(value)), Sensor = as.factor(Sensor)) %>%
    #   as.data.frame() %>%
    #   group_by(Sensor) %>%
    #   mutate(value = value - mean(value))






   
  #   ########



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

    
    
    
    
    
    
    
    