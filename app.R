library(shiny)
library(bslib)
library(DAAG)
library(tidyverse)
library(rlang)


pdata <- DAAG::possum
vars <- names(pdata[,6:ncol(pdata)])
colvars <- c

labs <- c("Head Length" = "hdlngth", 
          "Skull Width" = "skullw",
          "Total Length" = "totlngth",
          "Tail Length" = "taill",
          "Foot Length" = "footlgth",
          "Ear Conch Length" = "earconch",
          "Chest Girth" = "chest",
          "Belly Girth" = "belly",
          "Site" = "site",
          "Population" = "Pop",
          "Sex" = "sex", 
          "age" = "age")


ui <- page_sidebar(

  title = "Possum measurements",
  sidebar = sidebar(
    selectInput('xcol', 'X Variable:', c("Head Length" = "hdlngth", 
                                         "Skull Width" = "skullw",
                                         "Total Length" = "totlngth",
                                         "Tail Length" = "taill",
                                         "Foot Length" = "footlgth",
                                         "Ear Conch Length" = "earconch",
                                         "Chest Girth" = "chest",
                                         "Belly Girth" = "belly")),
    selectInput('ycol', 'Y Variable:', c("Head Length" = "hdlngth", 
                                         "Skull Width" = "skullw",
                                         "Total Length" = "totlngth",
                                         "Tail Length" = "taill",
                                         "Foot Length" = "footlgth",
                                         "Ear Conch Length" = "earconch",
                                         "Chest Girth" = "chest",
                                         "Belly Girth" = "belly")),
    selectInput('color', 'Color by:', c("Site" = "site",
                                        "Population" = "Pop",
                                        "Sex" = "sex", 
                                        "age" = "age"))
                 ),

  plotOutput(outputId = "plot1")

)


server <- function(input, output) {

 
  
  output$plot1 <- renderPlot({
    
    ggplot(pdata, aes(x=pdata[,input$xcol], y=pdata[,input$ycol], colour = as.character(pdata[,input$color]))) +
      geom_point(size=3.0, alpha=0.9) +
      theme_bw() +
      labs(x = names(labs[which(labs == input$xcol)]), 
           y = names(labs[which(labs == input$ycol)]),
           color = names(labs[which(labs == input$color)])) +
      theme(axis.title=element_text(size=14, face="bold"),
            legend.title=element_text(size=14, face="bold"))
      
  })
}


shinyApp(ui=ui, server=server)

