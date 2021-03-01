# Load packages ----
library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(icon)
library(PlantSim)
library(TruncatedNormal)

source('PlantSIm_plot.R', echo = TRUE)
source('PlantSim_sim.R', echo = TRUE)
source('PlantSim_infplot.R', echo = TRUE)
source('PlantSim_biasstayplot.R', echo = TRUE)
source('PlantSim_biasobsplot.R', echo=TRUE)
source('PlantSim_biasmisheteroplot.R', echo=TRUE)
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
      tabPanel("Simulation",
               fluidRow(
                 column(
                   3,
                   offset = 0,
                   style = 'padding:0px;',
                   box(
                     title = "Parameter settings",
                     status = "danger",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     fluidRow(column(6,
                                     selectInput(
                                       "num_spe",
                                       h5("Species:"),
                                       c("1",
                                         "2")
                                     )),

                              column(
                                6,
                                numericInput("num_plot",
                                             h5("Plots"),
                                             value = 100,
                                             step = 50)
                              )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "con_com",
                         h5("Conspecific competition (a):"),
                         value = 0.01,
                         min = 0,
                         max = 1,
                         step = 0.001
                       )
                     ),

                     column(6,
                            uiOutput("hetero"))),

                     fluidRow(column(
                       6,
                       numericInput(
                         "obs_err",
                         h5("Observation error rate"),
                         value = 0,
                         step = 0.02,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,
                       numericInput(
                         "growth_rate",
                         h5("Growth rate"),
                         value = 1.1,
                         step = 0.1
                       )
                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "st_portion",
                         h5("Stay rate"),
                         value = 0.4,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,

                       numericInput(
                         "surv_rate",
                         h5("Survival rate"),
                         value = 0.5,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )

                     )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "killrate",
                         h5("Elimination rate"),
                         value = 0.05,
                         step = 0.01,
                         min = 0,
                         max = 1
                       )
                     )
                     ),

                     fluidRow(column(
                       12,
                       sliderInput(
                         "sim_time",
                         label = h5("Selec simulating time"),
                         min = 5,
                         max = 30,
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
                       actionButton("go", "Simulate")
                     ))
                   )
                 ),

                 column(
                   7,
                   offset = 0,
                   style = 'padding:0px;',
                   box(
                     title = "Model ",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     withMathJax(),
                     tags$head(
                       tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                     ),
                     helpText(
                       h4(
                         'The Ricker model is used for simulation
                             $$N_{i,t+1} = r_i N_{i, t} e^{1 - \\sum_{j}^{n}\\alpha_{ij} N_{j, t}}$$'
                       )
                     )
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

                 column(
                   2,
                   offset = 0,
                   style = 'padding:0px;',
                   tags$head(tags$style(HTML(
                     ".small-box {width: 200px}"
                   ))),
                   fluidRow(valueBoxOutput("nspe")),
                   fluidRow(valueBoxOutput("stayrate")),
                   fluidRow(valueBoxOutput("growthrate")),
                   fluidRow(valueBoxOutput("con_com")),
                   fluidRow(uiOutput("hetero_text"))
                 )

               )),

      # Second tab
      tabPanel(
        "Inference",
        fluidRow(
          tags$head(tags$style(HTML(
            ".small-box {width: 200px}"
          ))),
          column(2,
                 valueBoxOutput("nspe_tab2")),
          column(2,
                 valueBoxOutput("stayrate_tab2")),
          column(2,
                 valueBoxOutput("growthrate_tab2")),
          column(2,
                 valueBoxOutput("con_com_tab2")),
          column(2,
                 uiOutput("hetero_text_tab2"))
        ),
        fluidRow(column(
          12,
          offset = 0,
          style = 'padding:0px;',
          box(
            title = "Parameter Inference ",
            status = "info",
            collapsible = TRUE,
            solidHeader = TRUE,
            width = 12,
            helpText(
              h4(
                'The linear regression is applied on the log-transformed
                            abundances on every two successive time-snaps across all plots.'
              )
            ),

            # Output: inference 1 ----
            plotlyOutput(outputId = "inference1"),

            # inference plot 2
            helpText(
              h4(
                'When fixing the growth rate (related to the intercept), does the estimates perform better?'
              )
            ),

            # Output: inference 2 ----
            plotlyOutput(outputId = "inference2")
          ),


        ))
      ),

      # dispersal tab: Tab 3
      tabPanel(title = "Dispersal",
               fluidRow(
                 box(
                   title = 'How does dispersal rate affect the bias?',
                   status = "info",
                   collapsible = TRUE,
                   solidHeader = TRUE,
                   width = 12,
                   withMathJax(),

                   helpText(
                     h4(
                       'Global dispersal intuitively destroys the signal of local dyanmics.
                       Thus, fitting local models to the data set involving dispersal is likely to produce bias
                       in parameter inference. As below, we show the impact of different dispersal rates on the bias.'
                     )
                   ),
                   helpText(
                     h4(
                       'In the simulation model, the dispersal rate is correltaed to the stay rate. So, we show the parameter
                       estimates when varying the stay rates.'
                     )
                   ),

                   helpText(
                     h4(
                       'The linear regression of the Ricker model is given by
                       $$r_c = log(\\frac{N_{c, t+1}}{N_{c, t}}) = \\beta_0 - a N_{c, t} - b N_{h, t}. $$
                       Using the data of \\(r_c\\), \\(N_{c, t}\\), \\(N_{h, t}\\), one can estimate \\(\\beta_0\\), \\(a\\), \\(b\\).'
                     )
                   ),

                   helpText(
                     h4(
                       'However, incorporating dispersal changes the underlying mechanisms, i.e. the local density dependence.
                       The true model used in the simulation actually yields
                       $$N_{c, J, t+1} = \\delta N_{c, J, t}  e^{\\beta_0 - a N_{c,J, t} - b N_{h, J, t} }
                       + \\frac{1}{L} \\sum_{m}^{L} (1 - \\delta) N_{c, m, t} e^{\\beta_0 - a N_{c,m, t} - b N_{h, m, t} }.$$
                       The second term on the RHS represents the spatial effect, which reduces the explanationary power
                       of the origional linear model.'
                     )
                   ),

                   helpText(
                     h4(
                       'In the other extreme case that all plants join the global dispersal, the model collaps to a simple model, i.e. \\(\\delta = 0 \\)
                       $$N_{c, J, t+1} = \\frac{1}{L} \\sum_{m}^{L} N_{c, m, t} e^{\\beta_0 - a N_{c,m, t} - b N_{h, m, t} } = E\\big( N_{c, m, t+1 }\\big).$$
                       One can use the data of \\(r_c\\) and \\( log(N_{c, m, t}) \\) to predict the slope to be -1,
                       $$r_c = log( \\frac{N_{c, m, t+1}}{N_{c, m, t}}) = log(E\\big( N_{c, m, t+1 }\\big)) - log(N_{c, m, t}).$$
                       This model may provide a way to predict how much dispersal rate is expected.'
                     )
                   ),
                 )
               ),

               fluidRow(
                 column(
                   width = 3,
                   box(
                     title = "Parameter settings",
                     status = "danger",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     fluidRow(column(6,
                                     selectInput(
                                       "num_spe_tab3",
                                       h5("Species:"),
                                       c("1",
                                         "2")
                                     )),

                              column(
                                6,
                                numericInput(
                                  "num_plot_tab3",
                                  h5("Plots"),
                                  value = 100,
                                  step = 50
                                )
                              )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "con_com_tab3",
                         h5("Conspecific competition (a):"),
                         value = 0.01,
                         min = 0,
                         max = 1,
                         step = 0.001
                       )
                     ),

                     column(6,
                            uiOutput("hetero_tab3"))),

                     fluidRow(column(
                       6,
                       numericInput(
                         "obs_err_tab3",
                         h5("Observation error"),
                         value = 0,
                         step = 0.01,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,
                       numericInput(
                         "growth_rate_tab3",
                         h5("Growth rate"),
                         value = 1.1,
                         step = 0.1
                       )
                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "st_portion_tab3",
                         h5("Stay rate step"),
                         value = 0.1,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,

                       numericInput(
                         "surv_rate_tab3",
                         h5("Survival rate"),
                         value = 0.5,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )

                     )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "killrate_tab3",
                         h5("Elimination rate"),
                         value = 0.05,
                         step = 0.01,
                         min = 0,
                         max = 1
                       )
                     )
                     ),

                     fluidRow(column(
                       12,
                       sliderInput(
                         "sim_time_tab3",
                         label = h5("Selec simulating time"),
                         min = 5,
                         max = 30,
                         value = 10,
                         width = 300
                       )
                     )),

                     fluidRow(column(
                       12,
                       selectInput("models_tab3",
                                   "Simulation model:",
                                   c("Ricker" = "ricker")),
                       br(),
                       actionButton("go_tab3", "Simulate")
                     ))

                   )
                 ),

                 column(
                   width = 9,
                   box(
                     title = "Bias vs. Stay rate",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasstayplot ----
                     plotlyOutput(outputId = "biasstayplot")
                   ),
                   box(
                     title = "Bias vs. Stay rate fixing growth rate",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasstayplot ----
                     plotlyOutput(outputId = "biasstayplotfixing")
                   ),

                   box(
                     title = "Global disperal check",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: glodisplot ----
                     plotlyOutput(outputId = "glodisplot")
                   )
                 )

               )),

      # observation tab: tab 4
      tabPanel(title = "Observation error",
               fluidRow(
                 box(
                   title = 'How does observation error affect the bias?',
                   status = "info",
                   collapsible = TRUE,
                   solidHeader = TRUE,
                   width = 12,

                   helpText(
                     h4(
                       'Observation error is inevitable in empirical ecology studies.
                       According to the linear regression model,
                       $$b_{\\text{OLS}} = \\frac{Cov(x, y)}{\\sigma_{x}^2 + \\sigma_{\\text{obs}^2}}$$
                       obervation error often leads to underestimation of the slope, indicating density dependence even when it is absent.'
                     )
                   ),
                   helpText(
                     h4(
                       'In reality, it could be much more complicated in terms of the nature of spatial explicity.
                       Dispersal could leads to unpredictable bias in parameter estimations. Here, we present how bias is related to
                       the observation error using the explicitly spatial model.'
                     )
                   )

                 )
               ),

               fluidRow(
                 column(
                   width = 3,
                   box(
                     title = "Parameter settings",
                     status = "danger",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     fluidRow(column(6,
                                     selectInput(
                                       "num_spe_tab4",
                                       h5("Species:"),
                                       c("1",
                                         "2")
                                     )),

                              column(
                                6,
                                numericInput(
                                  "num_plot_tab4",
                                  h5("Plots"),
                                  value = 100,
                                  step = 50
                                )
                              )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "con_com_tab4",
                         h5("Conspecific competition (a):"),
                         value = 0.01,
                         min = 0,
                         max = 1,
                         step = 0.001
                       )
                     ),

                     column(6,
                            uiOutput("hetero_tab4"))),

                     fluidRow(column(
                       6,
                       numericInput(
                         "obs_err_tab4",
                         h5("Observation error step"),
                         value = 0,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,
                       numericInput(
                         "growth_rate_tab4",
                         h5("Growth rate"),
                         value = 1.1,
                         step = 0.1
                       )
                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "st_portion_tab4",
                         h5("Stay rate"),
                         value = 0.1,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,

                       numericInput(
                         "surv_rate_tab4",
                         h5("Survival rate"),
                         value = 0.5,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )

                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "killrate_tab4",
                         h5("Elimination rate"),
                         value = 0.05,
                         step = 0.01,
                         min = 0,
                         max = 1
                       )
                     )
                     ),

                     fluidRow(column(
                       12,
                       sliderInput(
                         "sim_time_tab4",
                         label = h5("Selec simulating time"),
                         min = 5,
                         max = 30,
                         value = 10,
                         width = 300
                       )
                     )),

                     fluidRow(column(
                       12,
                       selectInput("models_tab4",
                                   "Simulation model:",
                                   c("Ricker" = "ricker")),
                       br(),
                       actionButton("go_tab4", "Simulate")
                     ))

                   )
                 ),

                 column(
                   width = 9,
                   box(
                     title = "Bias vs. Observation error",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasobsplot ----
                     plotlyOutput(outputId = "biasobsplot")
                   ),
                   box(
                     title = "Bias vs. Observation error fixing growth rate",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasobsplotfixing ----
                     plotlyOutput(outputId = "biasobsplotfixing")
                   )
                 )

               )),

      # misidentification tab: tab 5
      tabPanel(title = "Misidentification",
               fluidRow(
                 box(
                   title = 'If we misidentify some competitors, what do we expect for the bias?',
                   status = "info",
                   collapsible = TRUE,
                   solidHeader = TRUE,
                   width = 12,

                   helpText(
                     h4(
                       'In this tab, we explore when the system is consisting of multiple competitors
                       what bias will occur if only fitting on conpecifics data. For example, we simulate
                       the plant community with two species. When making inference, we only use the conspecific species
                       as if we haven\'t identified the heterspecifics.'
                     )
                   )

                 )
               ),
               fluidRow(
                 column(
                   width = 3,
                   box(
                     title = "Parameter settings",
                     status = "danger",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     fluidRow(column(6,
                                     selectInput(
                                       "num_spe_tab5",
                                       h5("Species:"),
                                       c(
                                         "2")
                                     )),

                              column(
                                6,
                                numericInput(
                                  "num_plot_tab5",
                                  h5("Plots"),
                                  value = 100,
                                  step = 50
                                )
                              )),

                     fluidRow(column(
                       6,
                       numericInput(
                         "con_com_tab5",
                         h5("Conspecific competition (a):"),
                         value = 0.01,
                         min = 0,
                         max = 1,
                         step = 0.001
                       )
                     ),

                     column(
                       6,
                       numericInput(
                         "hetero_com_tab5",
                         h5("Heterospecific competition (b):"),
                         value = 0.01,
                         min = 0,
                         max = 1,
                         step = 0.001
                       )
                     )
                     ),

                     fluidRow(column(
                       6,
                       numericInput(
                         "obs_err_tab5",
                         h5("Observation error step"),
                         value = 0,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,
                       numericInput(
                         "growth_rate_tab5",
                         h5("Growth rate"),
                         value = 1.1,
                         step = 0.1
                       )
                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "st_portion_tab5",
                         h5("Stay rate"),
                         value = 0.1,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )
                     ),

                     column(
                       6,

                       numericInput(
                         "surv_rate_tab5",
                         h5("Survival rate"),
                         value = 0.5,
                         step = 0.1,
                         min = 0,
                         max = 1
                       )

                     )),


                     fluidRow(column(
                       6,
                       numericInput(
                         "killrate_tab5",
                         h5("Elimination rate"),
                         value = 0.05,
                         step = 0.01,
                         min = 0,
                         max = 1
                       )
                     )
                     ),


                     fluidRow(column(
                       12,
                       sliderInput(
                         "sim_time_tab5",
                         label = h5("Selec simulating time"),
                         min = 5,
                         max = 30,
                         value = 10,
                         width = 300
                       )
                     )),

                     fluidRow(column(
                       12,
                       selectInput("models_tab5",
                                   "Simulation model:",
                                   c("Ricker" = "ricker")),
                       br(),
                       actionButton("go_tab5", "Simulate")
                     ))

                   )
                 ),

                 column(
                   width = 9,
                   box(
                     title = "Bias when misidentifying heterospecifics",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasmisheteroplot ----
                     plotlyOutput(outputId = "biasmisheteroplot")
                   ),
                   box(
                     title = "Bias when misidentifying heterospecifics and fixing growth rate",
                     status = "info",
                     collapsible = TRUE,
                     solidHeader = TRUE,
                     width = 12,
                     # Output: biasmisheteroplotfixing ----
                     plotlyOutput(outputId = "biasmisheteroplotfixing")
                   )
                 )

               )
               ),

      # About tab
      tabPanel(title = "About",
               fluidRow(
                 column(
                   10,
                   offset = 1,
                   helpText(h2('About')),
                   helpText(
                     h4(
                       'This app is created by Liang Xu at Oxford, initiated by exploring a number of factors that
                      influence on detecting density dependence in plant communities.'
                     )
                   ),
                   helpText(h3('Basic functions')),
                   helpText(
                     h4(
                       '-- Simulating plant communities under a variety of growth models.\n'
                     )
                   ),
                   helpText(h4(
                     '-- Inferring model parameters using linear regression.\n'
                   )),
                   helpText(
                     h4(
                       '-- Analyzing bias by tuning interaction coefficients, observation error, and dipersal rate, etc.\n'
                     )
                   ),
                   helpText(h3('Bug report')),
                   helpText(
                     h4(
                       '-- Feel free to use the app. If you encounter bugs, please report by sending emails to xl0418@gmail.com
                        or opening an issue at https://github.com/xl0418/PlantSimShiny.'
                     )
                   )
                 )

               ))
    )
  ))


ui <- dashboardPage(header, sidebar, body)


# Server logic
server <- function(input, output, session) {
  output$nspe <- renderValueBox({
    valueBox(
      value = formatC(input$num_spe, digits = 1, format = "f"),
      subtitle = "Species",
      icon = icon("bug"),
      color = if (as.numeric(input$num_spe) > 1)
        "red"
      else
        "green",
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
    parasInput$argu <- c(
      input$num_plot,
      as.numeric(input$num_spe),
      input$sim_time,
      input$growth_rate,
      input$st_portion,
      input$surv_rate,
      input$obs_err,
      input$killrate,
      input$con_com,
      input$hetero_com
    )
    if (is.null(parasInput$argu)) {
      sim_result <- PlantSim_sim(
        paras = c(
          input$num_plot,
          as.numeric(input$num_spe),
          input$sim_time,
          input$growth_rate,
          input$st_portion,
          input$surv_rate,
          input$obs_err,
          input$killrate,
          input$con_com,
          input$hetero_com
        )
      )
    } else {
      sim_result <- PlantSim_sim(parasInput$argu)
    }
    true.paras <- c(input$surv_rate,
                    input$growth_rate,
                    input$st_portion,
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
      color = if (as.numeric(input$num_spe) > 1)
        "red"
      else
        "green"
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

  # plot 1 on Tab dispersal tab 3

  output$hetero_tab3 = renderUI({
    req(input$num_spe_tab3) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
    if (input$num_spe_tab3 == "2") {
      numericInput(
        inputId = "hetero_com_tab3",
        label = h5("Heterospecific competition (b):"),
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.01# condition on the state
      )
    }

  })

  parasInput_tab3 <- reactiveValues(argu = NULL)

  model_paras_tab3 <- eventReactive(input$go_tab3, {
    parasInput_tab3$argu <- c(
      input$num_plot_tab3,
      as.numeric(input$num_spe_tab3),
      input$sim_time_tab3,
      input$growth_rate_tab3,
      input$st_portion_tab3,
      input$surv_rate_tab3,
      input$obs_err_tab3,
      input$killrate_tab3,
      input$con_com_tab3,
      input$hetero_com_tab3
    )
    if (is.null(parasInput_tab3$argu)) {
      paras = c(
        input$num_plot_tab3,
        as.numeric(input$num_spe_tab3),
        input$sim_time_tab3,
        input$growth_rate_tab3,
        input$st_portion_tab3,
        input$surv_rate_tab3,
        input$obs_err_tab3,
        input$killrate_tab3,
        input$con_com_tab3,
        input$hetero_com_tab3
      )

    } else {
      paras <- parasInput_tab3$argu
    }
    list(paras = paras)
  })

  output$biasstayplot <- renderPlotly({
    biasstay_plot(model_paras_tab3()$paras)[[1]]

  })

  output$biasstayplotfixing <- renderPlotly({
    biasstay_plot(model_paras_tab3()$paras)[[2]]

  })

  output$glodisplot <- renderPlotly({
    biasstay_plot(model_paras_tab3()$paras)[[3]]

  })

  # plot 1 on Tab observation error tab 4

  output$hetero_tab4 = renderUI({
    req(input$num_spe_tab4) # this makes sure Shiny waits until input$num_spe has been supplied. Avoids nasty error messages
    if (input$num_spe_tab4 == "2") {
      numericInput(
        inputId = "hetero_com_tab4",
        label = h5("Heterospecific competition (b):"),
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.01# condition on the state
      )
    }

  })

  parasInput_tab4 <- reactiveValues(argu = NULL)

  model_paras_tab4 <- eventReactive(input$go_tab4, {
    parasInput_tab4$argu <- c(
      input$num_plot_tab4,
      as.numeric(input$num_spe_tab4),
      input$sim_time_tab4,
      input$growth_rate_tab4,
      input$st_portion_tab4,
      input$surv_rate_tab4,
      input$obs_err_tab4,
      input$killrate_tab4,
      input$con_com_tab4,
      input$hetero_com_tab4
    )
    if (is.null(parasInput_tab4$argu)) {
      paras = c(
        input$num_plot_tab4,
        as.numeric(input$num_spe_tab4),
        input$sim_time_tab4,
        input$growth_rate_tab4,
        input$st_portion_tab4,
        input$surv_rate_tab4,
        input$obs_err_tab4,
        input$killrate_tab4,
        input$con_com_tab4,
        input$hetero_com_tab4
      )

    } else {
      paras <- parasInput_tab4$argu
    }
    list(paras = paras)
  })

  output$biasobsplot <- renderPlotly({
    biasobs_plot(model_paras_tab4()$paras)[[1]]

  })

  output$biasobsplotfixing <- renderPlotly({
    biasobs_plot(model_paras_tab4()$paras)[[2]]

  })


  # Misidentification tab: tab 5

  parasInput_tab5 <- reactiveValues(argu = NULL)

  model_paras_tab5 <- eventReactive(input$go_tab5, {
    parasInput_tab5$argu <- c(
      input$num_plot_tab5,
      as.numeric(input$num_spe_tab5),
      input$sim_time_tab5,
      input$growth_rate_tab5,
      input$st_portion_tab5,
      input$surv_rate_tab5,
      input$obs_err_tab5,
      input$killrate_tab5,
      input$con_com_tab5,
      input$hetero_com_tab5
    )
    if (is.null(parasInput_tab5$argu)) {
      paras = c(
        input$num_plot_tab5,
        as.numeric(input$num_spe_tab5),
        input$sim_time_tab5,
        input$growth_rate_tab5,
        input$st_portion_tab5,
        input$surv_rate_tab5,
        input$obs_err_tab5,
        input$killrate_tab5,
        input$con_com_tab5,
        input$hetero_com_tab5
      )

    } else {
      paras <- parasInput_tab5$argu
    }
    list(paras = paras)
  })

  output$biasmisheteroplot <- renderPlotly({
    biasmisheteroplot(model_paras_tab5()$paras)[[1]]

  })

  output$biasmisheteroplotfixing <- renderPlotly({
    biasmisheteroplot(model_paras_tab5()$paras)[[2]]

  })

}

# Run the app
shinyApp(ui, server)
