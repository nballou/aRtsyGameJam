library(shiny)
library(shinyWidgets)
library(colourpicker)
library(shinyjqui)
library(aRtsy)
library(randomcoloR)

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

    tags$style(
        HTML('
         #buttons {
         background-color:yellow; position:fixed; margin-bottom:50px; opacity:1; height:50px; z-index:5;
         display: flex;
         align-items: center;
         justify-content: center;
         }

         #randomSeedRow {
        height:100px;
         }

         ')
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

            fluidRow(
                id = "randomSeedRow",
                align = "center",
                column(width = 3,
                       numericInput(inputId = "seed",
                                    label = "Seed",
                                    value = as.integer(runif(1, min = 0, max = 100000)))
                ),

                column(
                    width = 3,
                    actionButton(inputId = "regenSeed",
                                 "Get new seed")
                )
            ),

            radioGroupButtons(inputId = "colorChoice",
                              label = "How would you like to choose colors?",
                              choices = c("Color Palette" = "colorPalette",
                                          "Manual colors" = "manualColors",
                                          "Manual gradient" = "manualGradient")
            ),

            conditionalPanel(
                condition = "input.colorChoice == 'colorPalette'",

                selectInput(inputId = "colorPalette",
                            label = "Color Palette",
                            choices = c("Dark Autumn" = "dark1",
                                        "Gustav Klimt" = "klimt"),
                ),
            ),

            conditionalPanel(
                condition = "input.colorChoice == 'manualColors'",

                sliderInput(inputId = "numColors",
                            label = "Number of Colors",
                            min = 1,
                            max = 6,
                            value = 2),

                uiOutput("manualColors")

                # colourInput(inputId = "color1",
                #             label = "Color 1",
                #             value = "#317da3"),
                #
                # colourInput(inputId = "color2",
                #             label = "Color 2",
                #             value = "#cdd690"),
                #
                # colourInput(inputId = "color3",
                #             label = "Color 3",
                #             value = "#a63f6a"),
            ),


            conditionalPanel(
                condition = "input.colorChoice == 'manualGradient'",

                colourInput(inputId = "color1",
                            label = "Color 1",
                            value = "#317da3"),

                colourInput(inputId = "color2",
                            label = "Color 2",
                            value = "#cdd690"),
            ),

            # Options for "Squares" generator
            conditionalPanel(
                condition = "input.generator == 'Squares'",

                # colourInput(inputId = "borderColor",
                #             label = "Border color",
                #             value = "Black"),

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
            ),
        ),

        # Create resizable image as output
        mainPanel(
            jqui_resizable(plotOutput('aRt', width = '750px', height = '750px')),
        )
    )
)

# Define server logic
server <- function(input, output) {

    # output$seed <- renderUI(
    #     numericInput("seed", "Random seed", value = as.integer(runif(1, min = 0, max = 1000000)))
    # )
    #
    observeEvent(input$regenSeed, {
        updateNumericInput(inputId = "seed", value = as.integer(runif(1, min = 0, max = 1000000)))
    })

    # Dynamically create color selectors based on the number of colors chosen
    output$manualColors <- renderUI({
        lapply(1:input$numColors, function(i) {
            # removeUI(
            #     selector = "div:has(> #txt)"
            # )
            colourInput(inputId = paste0('manualColor', i),
                        label = paste0('Color ', i),
                        value = randomColor())
        })
    })

    output$aRt <- renderPlot({

        set.seed(input$seed)

        if (input$generator == "Squares") {

            if (input$colorChoice == "manualColors") {

                labels <- paste0("manualColor", i:input$numColors)

                plotColors <- c()
                for (i in 1:length(labels)) {
                    plotColors <- c(plotColors, input[[labels[i]]])
                }
            }

            else if (input$colorChoice == "colorPalette") {
                plotColors <- colorPalette(input$colorPalette)
            }

            art <- canvas_squares(colors = plotColors,
                                  background = input$borderColor,
                                  cuts = input$cuts,
                                  ratio = input$ratio,
                                  resolution = input$resolution)

            # else if (input$chooseOwnColors == FALSE) {
            #     plotColors = colorPalette(input$colorPalette)
            # }

            #
            # if (input$chooseOwnColors == TRUE) {
            #     art <- art +
            #         ggplot2::scale_fill_gradient(low = input$color1, high = input$color2)
            # }
        }

        else if (input$generator == "Ribbons") {
            art <- canvas_ribbons(colors = colorPalette(input$colors))
        }
        art
    })
}

# Run the application
shinyApp(ui = ui, server = server)
