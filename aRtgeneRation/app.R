library(shiny)
library(shinyWidgets)
library(colourpicker)
library(aRtsy)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("aRt GeneRator"),

    # Dashboard color
    setBackgroundColor(
        color = c("#F7FBFF", "#2171B5"),
        gradient = "radial",
        direction = c("top", "left")
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "generator",
                        label = "Generator",
                        choices = c("Squares",
                                    "Ribbons")
            ),

            numericInput(inputId = "seed",
                         label = "Random Seed",
                         value = 8675309
            ),

            selectInput(inputId = "colors",
                        label = "Color Palette",
                        choices = c("Dark Autumn" = "dark1",
                                    "Gustav Klimt" = "klimt"),
            ),

            numericInput(inputId = "plotHeight",
                         label = "Plot Height",
                         min = 100,
                         max = 1000,
                         value = 500),

            conditionalPanel(
                condition = "input.generator == 'Squares'",

                colourInput(inputId = "borderColor",
                            label = "Border color",
                            value = "Black"),

                sliderInput(inputId = "cuts",
                            label = "Cuts",
                            min = 1,
                            max = 10000,
                            value = 50),

                sliderInput(inputId = "ratio",
                            label = "Ratio",
                            min = 1,
                            max = 3,
                            value = 1.618),

                sliderInput(inputId = "resolution",
                            label = "resolution",
                            min = 50,
                            max = 1000,
                            value = 200)
                # selectInput(inputId = "colors",
                #             label = "Color Palette",
                #             choices = c("Dark Autumn" = "dark1",
                #                         "Gustav Klimt" = "klimt"),
                ),
        ),

            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("aRt")
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$aRt <- renderPlot({
        set.seed(input$seed)

        height = function() input$plotHeight

        if (input$generator == "Squares") {
            canvas_squares(colors = colorPalette(input$colors),
                           background = input$borderColor,
                           cuts = input$cuts,
                           ratio = input$ratio,
                           resolution = input$resolution)
        }

        else if (input$generator == "Ribbons") {
            canvas_ribbons(colors = colorPalette(input$colors))
        }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
