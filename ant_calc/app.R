#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(ggplot2)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[, nums] <- round(df[, nums], digits = digits)
  
  (df)
}

ui <- dashboardPage(
  dashboardHeader(title = "Antenna Calculator"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Basic Calc",
      tabName = "userinputs",
      icon = icon("bar-chart-o")
    ),
    menuItem(
      "SSSputnik",
      tabName = "ssput",
      icon = icon("exclamation-triangle")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "userinputs",
      fluidRow(
        box(
          title = "Inputs",
          numericInput(
            "in_freq",
            "Frequency (MHz):",
            min = 0,
            max = Inf,
            value = 100
          ),
          numericInput(
            "in_vel",
            "Velocity Factor (vf):",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
          ),
          width = 6),
        box(
          selectInput(
            "in_wl",
            "1/2 Wavelength Segments:",
            c(3, 5, 7, 9, 11, 13),
            selected = 3
          ),
          numericInput(
            "in_acc",
            "Decimal places:",
            min = 1,
            max = 10,
            step = 1,
            value = 2
          ), 
          width = 6
        )),
        
        fluidRow(
        infoBoxOutput('pinfoBox1', width = 6),
        infoBoxOutput('pinfoBox2', width = 6),
        width = 12
        ),
      fluidRow(
        infoBoxOutput('pinfoBox3', width = 6),
        infoBoxOutput('pinfoBox4', width = 6),
        width = 12
        )
      ,
      
      fluidRow(box(plotOutput('ant_graphic'),
                   width = 12))
    ),
    
    tabItem(
      tabName = "ssput",
      tags$img(src = "https://i.ibb.co/Vgk92VD/IMG-20.jpg")
      #imageOutput(output$image_sput)
      
      
    )
  ))
)

server <- function(input, output) {
  
  vals <- reactiveValues()
  observe({
    
    vals$hz <- input$in_freq * 1000000
    
    vals$nqwla <- 100 * 0.25 * ((299792458 * 0.951) / vals$hz)
    vals$qwla <- formatC(100 * 0.25 * ((299792458 * 0.951) / vals$hz),
                         format="f", big.mark=",", digits = as.numeric(input$in_acc))
    
    vals$nhwla <- 100 *2 * (0.25 * ((299792458 * 0.951)/ vals$hz))
    vals$hwla <- formatC(100 *2 * (0.25 * ((299792458 * 0.951)/ vals$hz)),
                         format="f", big.mark=",", digits = as.numeric(input$in_acc))
    
    vals$nhwls <- 100 *0.5 * ((299792458 * input$in_vel) / vals$hz)
    vals$hwls <- formatC(100 *0.5 * ((299792458 * input$in_vel) / vals$hz),
                         format="f", big.mark=",", digits = as.numeric(input$in_acc))
    
    vals$nqwls <- 100 *0.5 * (0.5 * ((299792458 * input$in_vel) / vals$hz))
    vals$qwls <- formatC(100 *0.5 * (0.5 * ((299792458 * input$in_vel) / vals$hz)),
                         format="f", big.mark=",", digits = as.numeric(input$in_acc))
                        
  })
  
  calc <- function() {
    a <- vals$nqwla
    b <- vals$nhwls
    c <- vals$nqwls
    
    bnum <- as.numeric(input$in_wl)
    
    dat <<- data.frame(
      section = c("a", "a", rep("b", bnum), "c", "a"),
      long = c(a, a, rep(b, bnum), c, a),
      y = c(0, 0, rep_len(c(-1, 1), bnum +1 ), 0)
    )
    
    dat$clong = cumsum(dat$long)
    dat$x = c(0, dat$clong[-length(dat$clong)])
    dat$xend = dat$clong
    dat$yend = dat$y
    dat$xlab = c(0, dat$clong)[-length(c(0, dat$clong))] + diff(c(0, dat$clong))/2
    dat$long = round(dat$long, 2)
    
    return(dat)
  }
  
  calc_cup <- function() {
    
    dat_cup <- data.frame(
      x = c(0, 0, 0),
      y = c(1.5, -1.5, 1.5),
      xend = c(calc()$clong[1], calc()$clong[1], 0),
      yend = c(1.5, -1.5, -1.5)
    )
    
    return(dat_cup)
  }
  
  output$ant_graphic <- renderPlot(
    
    calc() %>%
      ggplot(aes(y = y, x = x)) +
      geom_segment(aes(xend = xend, yend = yend)) + 
      geom_segment(data = calc_cup(), aes(x = x, y = y, xend = xend, yend = yend)) + 
      geom_text(aes(label = section, y = 3, x = xlab)) + 
      geom_text(aes(label = long, y = -3, x = xlab)) +
      theme_void() + 
      coord_cartesian(ylim = c(-20,20))
  ) 
    
  output$pinfoBox1 <- renderInfoBox({
    infoBox("1/2 Wavelength (air) (mm)",
            vals$hwla, 
            icon = icon("gas-pump"))
  })
  
  output$pinfoBox2 <- renderInfoBox({
    infoBox("1/4 Wavelength (air) (mm)",
            vals$qwla, 
            icon = icon("gas-pump"))
  })
  
  output$pinfoBox3 <- renderInfoBox({
    infoBox("1/2 Wavelength segment (mm)",
            vals$hwls, 
            icon = icon("gas-pump"))
  })
  
  output$pinfoBox4 <- renderInfoBox({
    infoBox("1/4 Wavelength segment (mm)",
            vals$qwls, 
            icon = icon("gas-pump"))
  })
  
}

shinyApp(ui, server)
