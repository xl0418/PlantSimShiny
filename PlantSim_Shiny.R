# Load packages ----
library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(icon)
library(PlantSim)
source('~/CodeAtOxford/PlantSimShiny/PlantSIm_plot.R', echo=TRUE)
source('~/CodeAtOxford/PlantSimShiny/PlantSim_sim.R', echo=TRUE)
# Compute the current date

# Design the header ----
header <-
  dashboardHeader(
    title = span("Detecting Density Dependence",
                 style = "color: white; font-size: 28px; font-weight: bold"),
    titleWidth = 500
  )

# Design the sidebar ----
sidebar <- dashboardSidebar(disable = TRUE)


# Design the body ----
body <- dashboardBody(### changing theme
  ### changing theme
  theme = bslib::bs_theme(),

  fluidRow(
    tabBox(
      title = NULL,
      width = 12,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      height = "2056px",
      tabPanel(
        "Simulation",
        box(
          title = "Parameter settings",
          status = "danger",
          # solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          fluidRow(column(4,
                          selectInput("num_spe",
                                      h5("Species:"),
                                      c("1",
                                        "2"))
                          ),

                   column(
                     4,
                     numericInput("num_plot",
                                  h5("Plots"),
                                  value = 100,
                                  step = 50)
                   )),

          fluidRow(column(4,
                          numericInput("con_com",
                                      h5("Conspecific competition:"),
                                      value = 0.01,
                                      min = 0,
                                      max = 1,
                                      step = 0.001)
          ),

          column(
            4,
            uiOutput("hetero")

          )),

          fluidRow(column(4,
                          numericInput(
                            "obs_err",
                            h5("Observation error"),
                            value = 0,
                            step = 1,
                            min = 0
                          )),

                   column(
                     4,
                     numericInput("growth_rate",
                                  h5("Growth rate"),
                                  value = 1.1,
                                  step = 0.1)
                   )),


          fluidRow(column(
            4,
            numericInput("st_portion",
                         h5("Stay rate"),
                         value = 0.4,
                         step = 0.1,
                         min = 0,
                         max = 1)
          ),

          column(
            4,

            numericInput("surv_rate",
                         h5("Survival rate"),
                         value = 0.5,
                         step = 0.1,
                         min = 0,
                         max = 1)

          )),

          fluidRow(column(
            8,
            sliderInput(
              "sim_time",
              label = h5("Selec simulating time"),
              min = 5,
              max = 20,
              value = 10,
              width = 300
            )
          )),

          fluidRow(column(
            8,
            selectInput("models",
                        "Simulation model:",
                        c("Ricker" = "ricker")),
            br(),
            br(),
            actionButton("go","Simulate")
            ))
        ),

        box(
          title = "Local growth curves ",
          status = "info",
          collapsible = TRUE,
          width = 8,
          # Output: Histogram ----
          plotlyOutput(outputId = "localPlot")
        ),

        box(
          title = "Global growth curves",
          status = "info",
          collapsible = TRUE,
          width = 8,
          # Output: Histogram ----
          plotlyOutput(outputId = "globalPlot")
        )

      )
    )


  ))


ui <- dashboardPage(header, sidebar, body)


# Server logic
server <- function(input, output) {
  output$hetero = renderUI({
    req(input$num_spe) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
    if (input$num_spe == "2") {
      numericInput(
        inputId = "hetero_com",
        label = h5("Heterospecific competition:"),
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.01# condition on the state
      )
    }

  })


  # Simulating results and plots on Tab 2
  parasInput <- reactiveValues(argu = NULL)

  model_result <- eventReactive(input$go, {
    parasInput$argu <- c(input$num_plot,
                         as.numeric(input$num_spe),
                         input$sim_time,
                         input$growth_rate,
                         input$st_portion,
                         input$surv_rate,
                         input$obs_err,
                         input$con_com,
                         input$hetero_com
                         )
    if (is.null(parasInput$argu)) {
      sim_result <- PlantSim_sim(paras = c(input$num_plot,
                                           as.numeric(input$num_spe),
                             input$sim_time,
                             input$growth_rate,
                             input$st_portion,
                             input$surv_rate,
                             input$obs_err,
                             input$con_com,
                             input$hetero_com))
    } else {
      sim_result <- PlantSim_sim(parasInput$argu)
    }
    list(result = sim_result)
    })

    output$localPlot <- renderPlotly({

        PlantSim_plot(model_result()$result)[[1]]

    })

    output$globalPlot <- renderPlotly({

      PlantSim_plot(model_result()$result)[[2]]

    })

}

# Run the app
shinyApp(ui, server)
