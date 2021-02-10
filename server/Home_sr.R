# Home


# ggplot showing the research setting
output$ggplot.setting <- renderPlotly({
  p<-ggplot(Setting, aes(x=X, y=Elevation, col="orange")) +
    geom_line() + ylab("Elevation") + xlab("") +
    geom_text(aes(x=X, y = Elevation, label=Site)) + theme(legend.position = "none")
  fig <- ggplotly(p)
#  event_register(fig, 'plotly_click')
  return(fig)
  })

# output$plotly_click <- renderPrint({
#   
#   event_data("plotly_click")
# })


# observeEvent(event_data("plotly_click", source = "site_select"), {
#   
#   event_data <- event_data("plotly_click", source = "site_select")
#   
#   updateVarSelectInput(session, "site", selected = event_data[[3]])
#   print(SelectInput$plotly_click)
# })
