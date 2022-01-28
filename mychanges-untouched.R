library(shiny)
library(shinyWidgets)
library(colourpicker)
library(shinyjqui)
library(aRtsy)
library(viridisLite)
library(shinyBS)

# Load helper functions
source("shinyHelperFunctions.R")

# Create list of colors to sample from
allColors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("aRt GeneRator"),

  # Dashboard color
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),

  # Sidebar with user inputs
  sidebarLayout(
    sidebarPanel(

      h5("Play around with the options to create a piece of art you like. If you generate one you want to keep
         you can save it using the 'save' button below."),

      tipify(radioGroupButtons(inputId = "generator",
                               label = "Generator",
                               choices = c("Squares",
                                           "Ribbons",
                                           "Watercolor"),
                               selected = sample(c("Squares","Ribbons","Watercolor"), 1),
                               individual = TRUE), "What algorithm would you like to use?"),

      fluidRow(
        column(width = 6,
               numericInput(inputId = "seed",
                            label = "Seed",
                            value = as.integer(runif(1, min = 0, max = 100000)))
        ),

        column(
          width = 3,
          div(style = "margin-top: 25px;",
              tipify(actionButton(inputId = "regenSeed",
                                  label = "Get new seed"), "Click this to generate a new initial random piece to make changes to"))
        ),
      ),

      radioGroupButtons(inputId = "colorChoice",
                        label = "How would you like to choose colors?",
                        choices = c("Color Palette" = "colorPalette",
                                    "Manual colors" = "manualColors",
                                    "Manual gradient" = "manualGradient"),
                        individual = TRUE
      ),

      conditionalPanel(
        condition = "input.colorChoice == 'colorPalette'",

        selectInput(inputId = "colorPalette",
                    label = "Color Palette",
                    choices = c("Dark Autumn" = "dark1",
                                "Gustav Klimt" = "klimt",
                                "Cividis",
                                "Magma",
                                "Rocket"),
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
      ),

      conditionalPanel(
        condition = "input.colorChoice == 'manualGradient'",

        sliderInput(inputId = "numGradientColors",
                    label = "Number of Colors in Gradient",
                    min = 1,
                    max = 8,
                    value = 3),

        colourInput(inputId = "color1",
                    label = "Starting Color",
                    value = "#317da3"),

        colourInput(inputId = "color2",
                    label = "Ending Color",
                    value = "#cdd690"),
      ),

      # Options for "Squares" generator
      conditionalPanel(
        condition = "input.generator == 'Squares'",

        # colourInput(inputId = "borderColor",
        #             label = "Border color",
        #             value = "Black"),

        tipify(sliderInput(inputId = "cuts",
                           label = "Cuts",
                           min = 1,
                           max = 100,
                           value = 20), "This algorithm makes repeated cuts into the canvas at random locations and colouring areas these cuts create."),

        tipify(sliderInput(inputId = "ratio",
                           label = "Ratio",
                           min = 1,
                           max = 3,
                           value = 1.62,
                           step = .01), "??"),

        tipify(sliderInput(inputId = "Resolution",
                           label = "Resolution",
                           min = 20,
                           max = 400,
                           value = 100), "Increasing the resolution increases the quality of the artwork but also increases the computation time exponentially."),
      ),

      # Options for "Watercolor" generator
      conditionalPanel(
        condition = "input.generator == 'Watercolor'",

        colourInput(inputId = "background",
                    label = "Background Color",
                    value = "#FFFFFF"),

        # colourInput(inputId = "borderColor",
        #             label = "Border color",
        #             value = "Black"),

        tipify(sliderInput(inputId = "layers",
                           label = "Layers",
                           min = 5,
                           max = 200,
                           value = 50), "This art works by layering several geometric shapes and deforming each shape by repeatedly splitting its edges."),

        tipify(sliderInput(inputId = "depth",
                           label = "Algorithm Depth",
                           min = 1,
                           max = 3,
                           value = 2), "Number of repetitions of the algorithm."),
      ),

      h6("PS: the image is resizable."),

      actionButton("save", "Save image"),

      h6("This app was created by Nick Ballou and Elena Petrovskaya, based on the aRtsy package by Koen Derks."),

    ),

    # Create resizable image as output

    mainPanel(
      jqui_resizable(plotOutput('aRt', width = '750px', height = '750px')),
    )
  )
)

# Define server logic
server <- function(input, output) {

  observeEvent(input$regenSeed, {
    updateNumericInput(inputId = "seed", value = as.integer(runif(1, min = 0, max = 99999)))
  })

  # Dynamically create color selectors based on the number of colors chosen
  output$manualColors <- renderUI({
    set.seed(input$seed)
    lapply(1:input$numColors, function(i) {

      colourInput(inputId = paste0('manualColor', i),
                  label = paste0('Color ', i),
                  value = sample(allColors, 1))
    })
  })

  output$aRt <- renderPlot({

    set.seed(input$seed)

    # Create color palette from the various user options
    if (input$colorChoice == "manualColors") {

      labels <- paste0("manualColor", 1:input$numColors)

      plotColors <- c()
      for (i in 1:length(labels)) {
        plotColors <- c(plotColors, input[[labels[i]]])
      }
    }

    else if (input$colorChoice == "colorPalette") {
      set.seed(input$seed)
      if (input$colorPalette %in% c("dark1", "klimt")) {
        plotColors <- colorPalette(input$colorPalette)
      }

      else if (input$colorPalette == "Cividis") {
        plotColors <- viridisLite::cividis(sample(1:8, 1))
      }

      else if (input$colorPalette == "Magma") {
        plotColors <- viridisLite::magma(sample(1:8, 1))
      }

      else if (input$colorPalette == "Rocket") {
        plotColors <- viridisLite::rocket(sample(1:8, 1))
      }
    }

    else if (input$colorChoice == "manualGradient") {
      colorFunc <- colorRampPalette(c(input$color1, input$color2))
      plotColors <- colorFunc(input$numGradientColors)
    }

    # Generator for square-style
    if (input$generator == "Squares") {

      art <- canvas_squares(colors = plotColors,
                            background = input$borderColor,
                            cuts = input$cuts,
                            ratio = input$ratio,
                            resolution = input$resolution)
    }

    # Generator for ribbon style images
    else if (input$generator == "Ribbons") {
      art <- canvas_ribbons(colors = plotColors)
    }

    # Generator for watercolor-style images
    else if (input$generator == "Watercolor") {
      art <- my_canvas_watercolors(colors = plotColors,
                                   background = input$background,
                                   layers = input$layers,
                                   depth = input$depth)
    }
    observeEvent(input$save, {saveCanvas(art, filename = paste0("myArtwork", sample(1:100, 1), ".png"))})
    art
  })
}

# Run the application
shinyApp(ui = ui, server = server)
