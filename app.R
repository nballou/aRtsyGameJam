library(shiny)
library(shinyWidgets)
library(colourpicker)
library(shinyjqui)
library(aRtsy)
library(viridisLite)
library(shinythemes)
library(shinyBS)

# Load helper functions ####
source("shinyHelperFunctions.R")

# Create list of colors to sample from
allColors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

# Define UI
ui <- navbarPage(title = "aRt GeneRator",
                 theme = shinytheme("spacelab"),
                 inverse = TRUE,
                 tabPanel(title = "Generator",
                          # Dashboard color
                          setBackgroundColor(
                            color = c("#F7FBFF", "#8bbac9"),
                            gradient = "radial",
                            direction = c("top", "left")
                          ),

                          # Sidebar with user inputs
                          sidebarLayout(
                            sidebarPanel( # Sidebar panel ####
                                          h5("Play around with the options to create a piece of art you like.
                                             If you generate one you want to keep you can save it using the 'save' button below."),
                                          id = "sidebar",
                                          tipify(radioGroupButtons(inputId = "generator",
                                                                   label = "Generator",
                                                                   choices = c("Squares",
                                                                               "Ribbons",
                                                                               "Watercolor",
                                                                               "Collatz"),
                                                                   selected = sample(c("Squares","Ribbons","Watercolor","Collatz"), 1),
                                                                   individual = TRUE),
                                                 "What algorithm would you like to use?"
                                          ),

                                          fluidRow(
                                            column(width = 6,
                                                   numericInput(inputId = "seed",
                                                                label = "Seed",
                                                                value = as.integer(runif(1, min = 0, max = 100000)))
                                            ),

                                            column(
                                              width = 3,
                                              div(style = "margin-top: 25px; margin-bottom:25px;",
                                                  tipify(actionButton(inputId = "regenSeed",
                                                                      label = "Get New Seed"),
                                                         "Click this to generate a new initial random piece to make changes to")
                                              ),
                                            ),

                                            radioGroupButtons(inputId = "colorChoice",
                                                              label = "How would you like to choose colors?",
                                                              choices = c("Color Palette" = "colorPalette",
                                                                          "Manual colors" = "manualColors",
                                                                          "Manual gradient" = "manualGradient"),
                                                              individual = TRUE
                                            ),

                                            # Color options ####
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

                                              sliderInput(inputId = "numPaletteColors",
                                                          label = "Number of Colors in Palette",
                                                          min = 1,
                                                          max = 6,
                                                          value = 2),

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

                                            conditionalPanel(
                                              condition = "input.generator != 'Squares'",
                                              colourInput(inputId = "background",
                                                          label = "Background Color",
                                                          value = "#FFFFFF"),
                                            ),

                                            # "Squares" generator options ####
                                            conditionalPanel(
                                              condition = "input.generator == 'Squares'",

                                              tipify(sliderInput(inputId = "cuts",
                                                                 label = "Cuts",
                                                                 min = 1,
                                                                 max = 100,
                                                                 value = 20),
                                                     "This algorithm makes repeated cuts into the canvas at random locations and colouring areas these cuts create."
                                              ),

                                              sliderInput(inputId = "ratio",
                                                          label = "Ratio",
                                                          min = 1,
                                                          max = 3,
                                                          value = 1.62,
                                                          step = .01),

                                              sliderInput(inputId = "resolution",
                                                          label = "Border Width",
                                                          min = 1,
                                                          max = 40,
                                                          value = 10)
                                            ),

                                            # "Ribbons" generator options ####
                                            conditionalPanel(
                                              condition = "input.generator == 'Ribbons'",

                                              prettyCheckbox(inputId = "triangle",
                                                             label = "Triangle?",
                                                             value = TRUE,
                                                             bigger = TRUE,
                                                             outline = TRUE),

                                              sliderInput(inputId = "ribbonWidth",
                                                          label = "Ribbon Width",
                                                          value = 2.5,
                                                          min = 1,
                                                          max = 10),

                                              sliderInput(inputId = "maxHeight",
                                                          label = "Max Ribbon Height",
                                                          min = 50,
                                                          max = 85,
                                                          value = 75),

                                              sliderInput(inputId = "minHeight",
                                                          label = "Min Ribbon Height",
                                                          min = 15,
                                                          max = 50,
                                                          value = 25),
                                            ),

                                            # "Watercolor" generator options ####
                                            conditionalPanel(
                                              condition = "input.generator == 'Watercolor'",

                                              tipify(sliderInput(inputId = "layers",
                                                                 label = "Layers",
                                                                 min = 1,
                                                                 max = 60,
                                                                 value = 20),
                                                     "This art works by layering several geometric shapes and deforming each shape by repeatedly splitting its edges. Warning: adding layers is computionally intensive, so high values may take longer to render."
                                              ),

                                              tipify(sliderInput(inputId = "userMean",
                                                                 label = "Blurriness",
                                                                 min = 1,
                                                                 max = 40,
                                                                 value = 6),
                                                     "Changes the blurriness of the shapes."
                                              ),

                                              tipify(sliderInput(inputId = "depth",
                                                                 label = "Algorithm Depth",
                                                                 min = 1,
                                                                 max = 3,
                                                                 value = 2),
                                                     "Number of repetitions of the algorithm."
                                              ),
                                            ),

                                            # "Collatz" generator options ####
                                            conditionalPanel(
                                              condition = "input.generator == 'Collatz'",

                                              sliderInput(inputId = "numStrands",
                                                          label = "Number of Strands",
                                                          min = 1,
                                                          max = 500,
                                                          step = 1,
                                                          value = 200),

                                              sliderInput(inputId = "strandSize",
                                                          label = "Strand size",
                                                          min = .1,
                                                          max = 10,
                                                          step = .1,
                                                          value = 1
                                              ),

                                              tipify(sliderInput(inputId = "angle1",
                                                                 label = "First Angle",
                                                                 min = 0,
                                                                 max = 0.050,
                                                                 step = .001,
                                                                 value = .008),
                                                     "The angle the strand moves on odd-numbered steps."
                                              ),

                                              tipify(sliderInput(inputId = "angle2",
                                                                 label = "Second Angle",
                                                                 min = 0,
                                                                 max = 0.050,
                                                                 step = .001,
                                                                 value = .0145),
                                                     "The angle the strand moves on even-numbered steps"
                                              ),
                                              checkboxInput(inputId = "side",
                                                            label = "Turn 90 Degrees?",
                                                            value = FALSE)
                                            ),

                                            h6("P.S. The image is resizable; use the drag icon on the bottom left."),
                                            actionButton(inputId = "Save",
                                                         label = "Save image"),
                                          ),

                            ),

                            # Create resizable image as output
                            mainPanel( # Main Panel ####
                                       jqui_resizable(plotOutput('aRt', width = '750px', height = '750px')),
                            ),
                          ),
                 ),

                 # Credit tab ####
                 tabPanel(title = "Credit",
                          fluidPage(
                            mainPanel(
                              h4("This app was created by Nick Ballou and Elena Petrovskaya. The underlying code is based heavily on the aRtsy package by Koen Derks - we simply added a few extra parameters and made the generators accessible without needing knowledge of R."),
                              tags$a(href="https://github.com/nballou/aRtsyGameJam", "Link to source code for shiny app"),
                              h6(""),
                              tags$a(href="https://github.com/koenderks/aRtsy#watercolors", "Link to aRtsy R package"),
                            )
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
    # set.seed(input$seed)
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
        plotColors <- colorPalette(input$colorPalette, n = input$numPaletteColors)
      }

      else if (input$colorPalette == "Cividis") {
        plotColors <- viridisLite::cividis(input$numPaletteColors)
      }

      else if (input$colorPalette == "Magma") {
        plotColors <- viridisLite::magma(input$numPaletteColors)
      }

      else if (input$colorPalette == "Rocket") {
        plotColors <- viridisLite::rocket(input$numPaletteColors)
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
                            resolution = input$resolution*10)
    }

    # Generator for ribbon style images
    else if (input$generator == "Ribbons") {
      art <- my_canvas_ribbons(colors = plotColors,
                               triangle = input$triangle,
                               background = input$background,
                               maxHeight = input$maxHeight,
                               minHeight = input$minHeight,
                               ribbonWidth = input$ribbonWidth)
    }

    # Generator for watercolor-style images
    else if (input$generator == "Watercolor") {
      art <- my_canvas_watercolors(colors = plotColors,
                                   background = input$background,
                                   layers = input$layers,
                                   depth = input$depth,
                                   userMean = input$userMean)
    }

    # Generator for watercolor-style images
    else if (input$generator == "Collatz") {
      art <- my_canvas_collatz(colors = plotColors,
                               background = input$background,
                               n = input$numStrands,
                               angle.even = input$angle1,
                               angle.odd = input$angle2,
                               side = input$side,
                               strandSize = input$strandSize)
    }
    observeEvent(input$save, {saveCanvas(art, filename = paste0("myArtwork", sample(1:100, 1), ".png"))})
    art
  })
}

# Run the application
shinyApp(ui = ui, server = server)
