
# laden packages
library(shiny)
library(OBIC)
library(data.table)

# Gebruikersinterface (hier definieer je hoe alles er uit komt te zien)
ui <- fluidPage(

    # Application title
    titlePanel("Demo OBIC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("clay", "Kleigehalte (%)", min = 0, max = 100, value = 3, step = 2),
            sliderInput("os", "Organische stof (%)", min = 0, max = 30, value = 3, step = 0.5),
            sliderInput("ph", "Zuurtegraad (pH)", min = 4, max = 9, value = 6, step = 0.1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # bereken OBI score voor crumbleability
    observe({
        crumbleability <-  calc_crumbleability(input$clay, input$os, input$ph)
        sealing <- calc_sealing_risk(input$clay, input$os)
        
        output$plot <- renderPlot({
            barplot(height = c(crumbleability, sealing),
                    col = c(1,2),
                    legend.text = c("Verkuimelbaarheid", "Verslemping"),
                    args.legend = list(x = "topright"))
        })
        
    })

  
    

}

# Run the application 
shinyApp(ui = ui, server = server)
