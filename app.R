#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(RColorBrewer)
require(png)
require(dplyr)
library(shinyWidgets)

data <- read.delim2('data.txt')
data_sub <- data[1:2488,]
data_final <- data_sub %>% filter(!(Distance.X == 0 & Distance.Y == 0))
unique_batters_cnt <- length(unique(data_final$Batter))
pal <- colorRampPalette(brewer.pal(8, "Set1"))(unique_batters_cnt)
dates <- as.character(sort(unique(data_final$Date)))

data_input <- data_final

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarPanel(
        sidebarPanel(
            pickerInput("datesel",
                        "Date Selection",
                        choices = dates,
                        options = list(`actions-box` = TRUE),
                        multiple = T,
                        selected = dates),
            width = 15
        )
    ),
    mainPanel(
        plotlyOutput("distPlot")
    )
)


# Define server logic required to draw a baseball field
server <- function(input, output) {
    observe({
        data_input <- data_final %>% filter(Date %in% input$datesel)
        output$distPlot <- renderPlotly({
            fig <- ggplot(data_input, aes(x=Distance.X, y=Distance.Y, color = Batter), color=Batter) + 
                    geom_point() +
                    scale_colour_manual(values=pal) +
                    guides(color = FALSE, size = FALSE)

            ggplotly(fig) %>%
                layout(autosize = F, width = 600, height = 400, 
                       images = list(source = base64enc::dataURI(file = "Picture1.png"),
                                     x = 0, y = 0.75, 
                                     sizex = 1, sizey = 1, 
                                     opacity = 0.5,
                                     xref = "paper", yref = "paper"
                       ),
                       xaxis = list(
                           range=c(min(data_final$Distance.X),max(data_final$Distance.X)),
                           zeroline = TRUE
                       ),
                       yaxis = list(
                           range=c(min(data_final$Distance.Y),max(data_final$Distance.Y)),
                           zeroline = TRUE
                       )
                ) %>% config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
