#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Written by Pedro J. Aphalo for course IPS-003
#

library(shiny)
library(ggplot2)
library(ggpmisc)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("F-distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("df1",
                        "Numerator df:",
                        min = 1,
                        max = 100,
                        value = 4),
            sliderInput("df2",
                        "Denominator df:",
                        min = 1,
                        max = 100,
                        value = 10)
            ,
            sliderInput("target_F",
                        markdown("_F_-value for which to lookup the _P_-value:"),
                        min = 0,
                        max = 20,
                        value = 4),
            markdown("The area highlighted in black shows the _P_-value."),
            markdown("**Note:** Remember that the area under the curve as a whole represents _P_ = 1")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate 100 points to plot
        F <- seq(0, 20, length.out = 400)
        density <- df(F, input$df1, input$df2)

        scale_labels <- c(sprintf("italic(P)(italic(F)>%.3g)~`=`~%0.4f*\", and\"",
                                  input$target_F,
                                  1 - pf(input$target_F, input$df1, input$df2)),
                          sprintf("italic(P)(italic(F)<=%.3g)~`=`~%0.4f",
                                  input$target_F,
                                  pf(input$target_F, input$df1, input$df2)))

        ggplot(data.frame(F, density), aes(F, density)) +
            geom_area(aes(fill = F > input$target_F)) +
            geom_line() +
            geom_x_margin_arrow(xintercept = input$target_F, size = 1) +
            annotate(geom = "text_npc", npcx = "right", npcy = "top",
                     label = sprintf("italic(P)*\"-value\"~`=`~%0.5f",
                                     signif(1 - pf(input$target_F, input$df1, input$df2), 3)),
                     parse = TRUE, size = 6) +
            scale_fill_manual(name = expression(italic(P(F))~`=`~1*", so that:  "),
                              values = c("black", "grey80"),
                              breaks = c(TRUE, FALSE),
                              labels = parse(text = scale_labels)) +
            labs(x = expression(italic(F)-value), y = "Density") +
            theme(legend.position = "bottom")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
