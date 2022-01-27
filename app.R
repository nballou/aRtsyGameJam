library(shiny)
library(shinyWidgets)
library(colourpicker)
library(shinyjqui)
library(aRtsy)
library(viridisLite)

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

      radioGroupButtons(inputId = "generator",
                        label = "Generator",
                        choices = c("Squares",
                                    "Ribbons",
                                    "Watercolor"),
                        selected = sample(c("Squares","Ribbons","Watercolor"), 1),
                        individual = TRUE,
                        helpText("What algorithm would you like to use?")
      ),

      fluidRow(
        column(width = 6,
               numericInput(inputId = "seed",
                            label = "Seed",
                            value = as.integer(runif(1, min = 0, max = 100000)))
        ),

        column(
          width = 3,
          div(style = "margin-top: 25px;",
              actionButton(inputId = "regenSeed",
                           label = "Get new seed"))
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

        numericInput(inputId = "numGradientColors",
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

        sliderInput(inputId = "cuts",
                    label = "Cuts",
                    min = 1,
                    max = 100,
                    value = 20),

        sliderInput(inputId = "ratio",
                    label = "Ratio",
                    min = 1,
                    max = 3,
                    value = 1.62,
                    step = .01),

        sliderInput(inputId = "resolution",
                    label = "resolution",
                    min = 20,
                    max = 400,
                    value = 100)
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

        sliderInput(inputId = "layers",
                    label = "layers",
                    min = 5,
                    max = 200,
                    value = 50),

        sliderInput(inputId = "depth",
                    label = "Algorithm Depth",
                    min = 1,
                    max = 3,
                    value = 2),
      ),

      actionButton("save", "Save image"),

    ),

    # Create resizable image as output

    mainPanel(
      jqui_resizable(plotOutput('aRt', width = '750px', height = '750px')),
    )
  )
)

# Define server logic
server <- function(input, output) {

  req(input$seed)
  req(input$numColors)

  observeEvent(input$regenSeed, {
    updateNumericInput(inputId = "seed", value = as.integer(runif(1, min = 0, max = 1000000)))
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
    observeEvent(input$save, {saveCanvas(art, filename = "myArtwork.png")})
    art
  })
}

# Run the application
shinyApp(ui = ui, server = server)
