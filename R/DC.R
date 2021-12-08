#' Generates text output displaying your primary source of food, given your 15N and 13C inputs
#' @export
#' @title Diet Calculator

library(shiny)
library(tidyverse)

ui <- fluidPage(numericInput(inputId = "Cnumber",
                             label = "d13C (per mil)",
                             value = "2",),
                numericInput(inputId = "Nnumber",
                             label = "d15N (per mil)",
                             value = "1"),
                textOutput(outputId = "DietCalculator")
)


server <- function(input, output, session) {
  d13Cpdb <- reactive(input$Cnumber)
  d15Nair <- reactive(input$Nnumber)

  output$DietCalculator <- renderText({
    req(input$Cnumber, input$Nnumber)
    if(d13Cpdb() <= -23 & d13Cpdb() >= -29 & d15Nair() <= 7 & d15Nair() >= -1)
    {"Your diet consists of Fruits, Vegetables, and Grains"
    } else if(d13Cpdb() <= -22 & d13Cpdb() >= -29 & d15Nair() <= 11 & d15Nair() >= -7)
    {"Your diet derives from Non-corn-Fed Meat Consumers"
    } else if(d13Cpdb() <= -17 & d13Cpdb() >= -19 & d15Nair() <= 6 & d15Nair() >= 4)
    {"Your diet consists priarily of Dairy"
    } else if(d13Cpdb() <= -21 & d13Cpdb() >= -18 & d15Nair() <= 17 & d15Nair() >= 12)
    {"Your diet primarily consists of Marine Fish"
    } else if(d13Cpdb() <= -12 & d13Cpdb() >= -17 & d15Nair() <= 11 & d15Nair() >= 3)
    {"Your diet primarily derives from Corn-Fed Meat Consumers"
    } else if(d13Cpdb() <= -10 & d13Cpdb() >= -13 & d15Nair() <= 5 & d15Nair() >= 3)
    {"Your diet consists of Corn & Sugar Cane Product Consumers, eat healthier"
    } else {"Diverse diet, your isotope values reflect a mixing of multiple sources"
    }
  })
}

shinyApp(ui = ui, server = server)
