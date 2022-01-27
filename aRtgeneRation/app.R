library(shiny)
library(shinyWidgets)
library(colourpicker)
library(shinyjqui)
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
                                    "Ribbons"),
                        helpText("What algorithm would you like to use?")
            ),

            numericInput(inputId = "seed",
                         label = "Random Seed",
                         value = 8675309
            ),

            checkboxInput(inputId = "chooseOwnColors",
                          label = "Choose colors manually?",
                          value = FALSE),


            conditionalPanel(
                condition = "input.chooseOwnColors == 1",
                colourInput(inputId = "color1",
                            label = "Color 1",
                            value = "#317da3"),

                colourInput(inputId = "color2",
                            label = "Color 2",
                            value = "#cdd690"),

                colourInput(inputId = "color3",
                            label = "Color 3",
                            value = "#a63f6a"),
            ),

            conditionalPanel(
                condition = "input.chooseOwnColors == 0",
                selectInput(inputId = "colorPalette",
                            label = "Color Palette",
                            choices = c("Dark Autumn" = "dark1",
                                        "Gustav Klimt" = "klimt"),
                ),
            ),


            # sliderInput(inputId = "plotHeight",
            #              label = "Plot Height (Pixels)",
            #              min = 100,
            #              max = 3000,
            #              value = 500),
            #
            # sliderInput(inputId = "plotWidth",
            #              label = "Plot Width (Pixels)",
            #              min = 100,
            #              max = 3000,
            #              value = 500),

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
                            value = 1.62,
                            step = .01),

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
                jqui_resizable(plotOutput('aRt', width = '750px', height = '750px')),
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$aRt <- renderPlot(
        # height = function() input$plotHeight,
        # width = function() input$plotWidth,
        {
            set.seed(input$seed)

            if (input$generator == "Squares") {

                if (input$chooseOwnColors == TRUE) {
                    plotColors = c(input$color1, input$color2, input$color3)
                }

                else if (input$chooseOwnColors == FALSE) {
                    plotColors = colorPalette(input$colorPalette)
                }
                canvas_squares(colors = plotColors,
                               background = input$borderColor,
                               cuts = input$cuts,
                               ratio = input$ratio,
                               resolution = input$resolution)
            }

            else if (input$generator == "Ribbons") {
                canvas_ribbons(colors = colorPalette(input$colors))
            }

        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
