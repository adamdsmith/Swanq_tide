ui <- fluidPage(
  fluidRow(
    column(12, 
           helpText("Note: Currently comparisons are available from 3 May 2013 to 7 Feb 2014."))
  ), 
  
  fluidRow(
    column(3, 
           dateInput("date", label = h4("Select a start date:"),
                     value = "2013-05-03")),
    column(3,
           radioButtons("window", label = h4("Select a time window"),
                        choices = list("3 days" = 3, 
                                       "1 week" = 7,
                                       "1 month" = 30), selected = 3))
  ),
  
  fluidRow(
    column(width = 12,
           plotOutput("tides", height = 350))
  ),
  
  fluidRow(
    column(width = 12,
           plotOutput("temps", height = 350))
  )
  
)

server <- function(input, output, session) {

  output$tides <- renderPlot({
    plotDat <- dat[dat$dt >= ymd(input$date) & dat$dt <= (ymd(input$date) + days(input$window)), ]
    scaling <- c(as.numeric(diff(range(plotDat$dt)))*24*60*60, # convert to seconds 
                 diff(range(c(plotDat$bell_wl, plotDat$wl), na.rm = TRUE)))
    
    plotDat <- mutate(plotDat,
                  x.end = dt + wsp_sc * scaling[[1]] * sin(wdir / 180 * pi),
                  y.end = bell_wl + wsp_sc * scaling[[2]] * cos(wdir / 180 * pi))
 
    l_size <- 0.65
    n_days <- length(unique(lubridate::yday(plotDat$dt)))
    arrDat <- plotDat[every_nth(1:nrow(plotDat), 
                                 ifelse(n_days > 8, 60, ifelse(n_days > 4, 20, 10)),
                                 empty = FALSE, inverse = TRUE), ]
    ggplot(data = plotDat, aes(x = dt, y = wl)) +
      geom_line(size = l_size) +
      geom_line(aes(y = bell_wl), size = l_size, color = "blue") +
      geom_segment(data = arrDat,
                   size = l_size,
                   aes(x = x.end,
                       xend = dt,
                       y = y.end,
                       yend = bell_wl,
                       colour = wsp),
                   arrow = arrow(length = unit(0.02, "snpc"), type = "closed")) +
      ylab("Water level, such as it is") + xlab("") +
      scale_colour_gradient("Wind speed (m/s)", low="green", high="red") + theme_bw() +
      theme(legend.justification=c(0,1), legend.position=c(0,1),
            legend.direction = "horizontal", legend.background = element_rect(fill=NA))
    
  })

  output$temps <- renderPlot({
    plotDat <- dat[dat$dt >= ymd(input$date) & dat$dt <= (ymd(input$date) + days(input$window)), ]
    l_size <- 0.65
    ggplot(data = plotDat, aes(x = dt, y = sst)) + geom_line(size = l_size) + 
      geom_line(aes(y = bell_sst), size = l_size, color = "blue") +
      ylab(expression("Water temperature "(degree*C))) + xlab("Date & time") +
      theme_bw()
  })
  
}

shinyApp(ui, server)
