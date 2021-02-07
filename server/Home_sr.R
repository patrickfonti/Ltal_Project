# Home

# ggplot showing the research setting

output$ggplot.setting <- renderPlot({
  ggplot(Setting, aes(x=X, y=Elevation, col="orange")) +
    geom_line() + ylab("Elevation") + xlab("") +
    geom_label(aes(x=X, y = Elevation, label=Site)) + theme(legend.position = "none")
})


