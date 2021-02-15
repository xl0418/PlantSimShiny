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
source('~/CodeAtOxford/PlantSimShiny/PlantSim_infplot.R', echo=TRUE)
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
        fluidRow(
          column(3,
                 offset = 0, style='padding:0px;',
            box(
              title = "Parameter settings",
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              fluidRow(column(6,
                              selectInput("num_spe",
                                          h5("Species:"),
                                          c("1",
                                            "2"))
              ),

              column(
                6,
                numericInput("num_plot",
                             h5("Plots"),
                             value = 100,
                             step = 50)
              )),

              fluidRow(column(6,
                              numericInput("con_com",
                                           h5("Conspecific competition (a):"),
                                           value = 0.01,
                                           min = 0,
                                           max = 1,
                                           step = 0.001)
              ),

              column(
                6,
                uiOutput("hetero")

              )),

              fluidRow(column(6,
                              numericInput(
                                "obs_err",
                                h5("Observation error"),
                                value = 0,
                                step = 1,
                                min = 0
                              )),

                       column(
                         6,
                         numericInput("growth_rate",
                                      h5("Growth rate"),
                                      value = 1.1,
                                      step = 0.1)
                       )),


              fluidRow(column(
                6,
                numericInput("st_portion",
                             h5("Stay rate"),
                             value = 0.4,
                             step = 0.1,
                             min = 0,
                             max = 1)
              ),

              column(
                6,

                numericInput("surv_rate",
                             h5("Survival rate"),
                             value = 0.5,
                             step = 0.1,
                             min = 0,
                             max = 1)

              )),

              fluidRow(column(
                12,
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
                12,
                selectInput("models",
                            "Simulation model:",
                            c("Ricker" = "ricker")),
                br(),
                actionButton("go","Simulate")
              ))
            )
          ),

        column(7,
               offset = 0, style='padding:0px;',
               box(
                 title = "Model ",
                 status = "info",
                 collapsible = TRUE,
                 solidHeader = TRUE,
                 width = 12,
                 withMathJax(),
                 tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                 )
                 ),
                 helpText(h4('The Ricker model is used for simulation
                             $$N_{i,t+1} = r_i N_{i, t} e^{1 - \\sum_{j}^{n}\\alpha_{ij} N_{j, t}}$$'))
               ),


               box(
                 title = "Local growth curves ",
                 status = "info",
                 collapsible = TRUE,
                 solidHeader = TRUE,
                 width = 12,
                 # Output: Histogram ----
                 plotlyOutput(outputId = "localPlot")
               ),

               box(
                 title = "Global growth curves",
                 status = "info",
                 collapsible = TRUE,
                 solidHeader = TRUE,
                 width = 12,
                 # Output: Histogram ----
                 plotlyOutput(outputId = "globalPlot")
               )
               ),

        column(2,
               offset = 0, style='padding:0px;',
          tags$head(tags$style(HTML(".small-box {width: 200px}"))),
          fluidRow(valueBoxOutput("nspe")),
          fluidRow(valueBoxOutput("stayrate")),
          fluidRow(valueBoxOutput("growthrate")),
          fluidRow(valueBoxOutput("con_com")),
          fluidRow(uiOutput("hetero_text"))
        )

      )
      ),

      # Second tab
      tabPanel(
        "Inference",
        fluidRow(
          tags$head(tags$style(HTML(".small-box {width: 200px}"))),
          column(2,
                 valueBoxOutput("nspe_tab2")
                 ),
          column(2,
                 valueBoxOutput("stayrate_tab2")
          ),
          column(2,
                 valueBoxOutput("growthrate_tab2")
                 ),
          column(2,
                 valueBoxOutput("con_com_tab2")
                 ),
          column(2,
                 uiOutput("hetero_text_tab2")
          )
          ),
        fluidRow(
          column(12,
                 offset = 0, style='padding:0px;',
                 box(
                   title = "Parameter Inference ",
                   status = "info",
                   collapsible = TRUE,
                   solidHeader = TRUE,
                   width = 12,
                   helpText(h4('The linear regression is applied on the log-transformed
                            abundances on every two successive time-snaps across all plots.')),

                   # Output: inference 1 ----
                   plotlyOutput(outputId = "inference1"),

                   # inference plot 2
                   helpText(h4('When fixing the growth rate (related to the intercept), does the estimates perform better?')),

                   # Output: inference 2 ----
                   plotlyOutput(outputId = "inference2")
                 ),


          )

        )
      )
    )
  )
)


ui <- dashboardPage(header, sidebar, body)


# Server logic
server <- function(input, output, session) {
  output$nspe <- renderValueBox({

    valueBox(
      value = formatC(input$num_spe, digits = 1, format = "f"),
      subtitle = "Species",
      icon = icon("bug"),
      color = if (as.numeric(input$num_spe) > 1) "red" else "green",
      width = 12
    )
  })

  output$stayrate <- renderValueBox({

    valueBox(
      value = formatC(input$st_portion, digits = 1, format = "f"),
      subtitle = "Stay rate",
      icon = icon("eject"),
      color = "yellow",
      width = 12
    )
  })

  output$con_com <- renderValueBox({

    valueBox(
      value = formatC(input$con_com, digits = 3, format = "f"),
      subtitle = "Conspecific coefficient",
      icon = icon("compress"),
      color = "orange",
      width = 12
    )
  })


  output$growthrate <- renderValueBox({

    valueBox(
      value = formatC(input$growth_rate, digits = 2, format = "f"),
      subtitle = "Growth rate",
      icon = icon("air-freshener"),
      color = "purple",
      width = 12
    )
  })


  output$hetero_text = renderUI({
    req(input$num_spe) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
    if (input$num_spe == "2") {
        valueBox(
          value = formatC(input$hetero_com, digits = 3, format = "f"),
          subtitle = "Heterospecific coefficient",
          icon = icon("compress-arrows-alt"),
          color = "light-blue",
          width = 12
        )
    }

  })


  output$hetero = renderUI({
    req(input$num_spe) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
    if (input$num_spe == "2") {
      numericInput(
        inputId = "hetero_com",
        label = h5("Heterospecific competition (b):"),
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.01# condition on the state
      )
    }

  })


  # Simulating results and plots on Tab 1
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
    true.paras <- c(input$growth_rate,
                    input$con_com,
                    input$hetero_com)
    list(result = sim_result, trueparas = true.paras)
    })

    output$localPlot <- renderPlotly({

        PlantSim_plot(model_result()$result)[[1]]

    })

    output$globalPlot <- renderPlotly({

      PlantSim_plot(model_result()$result)[[2]]

    })



    # tags on Tab 2
    output$nspe_tab2 <- renderValueBox({

      valueBox(
        value = formatC(input$num_spe, digits = 1, format = "f"),
        subtitle = "Species",
        icon = icon("bug"),
        color = if (as.numeric(input$num_spe) > 1) "red" else "green"
      )
    })

    output$stayrate_tab2 <- renderValueBox({

      valueBox(
        value = formatC(input$st_portion, digits = 1, format = "f"),
        subtitle = "Stay rate",
        icon = icon("eject"),
        color = "yellow",
      )
    })

    output$con_com_tab2 <- renderValueBox({

      valueBox(
        value = formatC(input$con_com, digits = 3, format = "f"),
        subtitle = "Conspecific coefficient",
        icon = icon("compress"),
        color = "orange"
      )
    })


    output$growthrate_tab2 <- renderValueBox({

      valueBox(
        value = formatC(input$growth_rate, digits = 2, format = "f"),
        subtitle = "Growth rate",
        icon = icon("air-freshener"),
        color = "purple"
      )
    })


    output$hetero_text_tab2 = renderUI({
      req(input$num_spe) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
      if (input$num_spe == "2") {
        valueBox(
          value = formatC(input$hetero_com, digits = 3, format = "f"),
          subtitle = "Heterospecific coefficient",
          icon = icon("compress-arrows-alt"),
          color = "light-blue"
        )
      }

    })

    # plots on Tab 2
    output$inference1 <- renderPlotly({

      PlantSim_infplot(model_result()$result, model_result()$trueparas)[[1]]

    })
    # plot 2 on Tab 2
    output$inference2 <- renderPlotly({

      PlantSim_infplot(model_result()$result, model_result()$trueparas)[[2]]

    })

}

# Run the app
shinyApp(ui, server)
