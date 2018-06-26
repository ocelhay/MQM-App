# Medicine Quality Modelling App
# MORU, MAEMOD, Olivier Celhay
# ui.R  definition of the user interface
# -----------------------------------------------------------------------------

fluidPage(
  includeCSS("./www/styles.css"),
  
  theme = shinytheme("readable"),
  
  mainPanel(width = 12,
            navbarPage(NULL, position = "static-top", id = "tabs", collapsible = TRUE,  windowTitle = "Medicine Quality App",
                       tabPanel("Malaria", value = "malaria",
                                
                                # Common to all scenarios
                                fluidRow(htmlOutput("parameters_values")),
                                
                                # Separate columns for each scenario
                                fluidRow(
                                  column(width = 6,
                                         div(class = "borderscenario12",
                                             div(class = "cent", h3("Scenario 1")),
                                             fluidRow(
                                               column(width = 6,
                                                      numericInput("r0_s1", "Transmission Setting, R0", min = 1.2, max = 4, value = 2.5, width = "200px")
                                               ),
                                               column(width = 6,
                                                      sliderInput("wait_treat_s1", "Average wait time before treatment", min = 0, max = 10, value = 2)
                                               )
                                             )
                                         )
                                  ),
                                  column(width = 6,
                                         div(class = "borderscenario22",
                                             div(class = "cent", h3("Scenario 2")),
                                             fluidRow(
                                               column(width = 6,
                                                      numericInput("r0_s2", "Transmission Setting, R0", min = 1.2, max = 4, value = 2.5, width = "200px")
                                               ),
                                               column(width = 6,
                                                      sliderInput("wait_treat_s2", "Average wait time before treatment", min = 0, max = 10, value = 2)
                                               )
                                             )
                                         )
                                  )
                                ),
                                
                                # Common to all scenarios
                                fluidRow(
                                  column(width = 4, br()),
                                  column(width = 4,
                                         div(style="display: inline-block;vertical-align:center; width: 150px;", numericInput("total_q", "Total Steps:", value = 5, min = 1, max = 10)),
                                         div(style="display: inline-block;vertical-align:top; width: 5px;", HTML("<br>")),
                                         div(style="display: inline-block;vertical-align:center;", actionButton("run", "Run both Scenarios")),
                                         br(),
                                         div(style="display: inline-block;vertical-align:center;", htmlOutput("progress_update"))
                                  ),
                                  column(width = 4, br())
                                ),
                                
                                
                                # Separate columns for each scenario
                                fluidRow(
                                  column(width = 6,
                                         div(class = "borderscenario12",

                                             plotOutput("plot_incidence_s1", height = "300px"),
                                             plotOutput("plot_global_s1", height = "300px")
                                         )
                                  ),
                                  column(width = 6,
                                         div(class = "borderscenario22",
                                             plotOutput("plot_incidence_s2", height = "300px"),
                                             plotOutput("plot_global_s2", height = "300px")
                                         )
                                  )
                                ),
                                
                                # Common to all scenarios
                                fluidRow(
                                  column(width = 3, br()),
                                  column(width = 6,
                                         uiOutput("plot_navig_buttons")
                                  ),
                                  column(width = 3, br())
                                )
                       )
                       # ,
                       # tabPanel("Welcome", value = "welcome",
                       #          includeMarkdown("./www/welcome.md")
            )
  )
)