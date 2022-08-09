library(shiny)
library(tidyverse)
library(ggplot2)

ui <- navbarPage(title= "Interference Between 2 Wells",
                 tabPanel("Input / Map",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("well_name1", "Enter a well name"),
                              textInput("well_name1", "Enter a second well name"),
                              numericInput("offset_distance", "Enter a distance to offset the second well from the first (ft)", value=670),
                              numericInput("flow1", "Enter the flow rate to assign to the first well (GPM)", value = 550),
                              numericInput("flow2", "Enter the flow rate to assign to the second well (GPM)", value=2000),
                              numericInput("b", "Enter the aquifer thickness (ft)", value=60),
                              numericInput("k", "Enter the hydraulic conductivty (ft/d)", value=89.14),
                              numericInput("tprime", "Enter the number of annual days of well operation", value=95),
                              #sliderInput("t_out", "Day of year to view", min=5, max=365, step=5, value =95),
                              sliderInput("S", "Storativity", min=.02, max=.3, step=.005, value =0.175)
                            ),
                            mainPanel(
                              plotOutput("contour_map")
                              #tableOutput('z')
                            )
                          )
                 ),
)
server <-
  function(input,output,session){
    
    r1 <- reactive(c((c(-8:8,seq(-1.75,1.75,.25))*input$offset_distance),1) %>% sort() %>% na_if(0) %>% na.omit())
    r2 <- reactive({
      (c((c(-8:8,seq(-1.75,1.75,.25))*input$offset_distance),1) %>% sort() %>% na_if(0) %>% na.omit())-input$offset_distance
      })
    u <- reactive({
      (r1()^2*input$S)/(4*input$k*input$b*input$tprime)
    })
    s1 <- reactive({
      (input$flow1*192.5/(4*pi*input$k*input$b)) * (list_well_fxn(u(),200))
    })
    s2 <- reactive({
      (input$flow2*192.5/(4*pi*input$k*input$b)) * (list_well_fxn(u(),200))
    })
    tbl <- reactive({
      tibble(r1(),s1(),r2(),s2()) %>% pivot_longer(
        everything(),names_to = c(".value", "set"),
        names_pattern = "(.)(.)") %>% arrange(desc(set))
    })
    
    
    output$b <- renderText({r()})
    output$z <- renderTable({tbl()})
    output$contour_map <- renderPlot({
      ggplot(tbl(), aes(x=r,y=-s, shape=set)) + geom_point() + geom_line(aes(color=set)) +
        ggtitle(paste0("Cones of depression")) + 
        xlab("Distance from Well 1 (ft)") +
        ylab("Drawdown from static water level (ft)") +
        theme_classic() +
        labs(color="Days since pumping began") +
        guides(color = guide_colorbar(reverse=TRUE))},
      width =800,
      height = 1000,
      res=160)
    
    
  }
shinyApp(ui, server)