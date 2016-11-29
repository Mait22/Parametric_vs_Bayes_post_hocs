library(shiny)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("flatly"),
                  titlePanel("Comparison of Bayes and parametric analsysis in ANOVA post-hoc settings"),
                  sidebarLayout(
                    sidebarPanel("Please specify analysis settings",width = 3,
                                 uiOutput("data_selector"),
                                 p(""),
                                 uiOutput("post_hoc_selector"),
                                 p(""),
                                 sliderInput("pvalue", "P-value:",
                                             min = 0.01, max = 1, value = 0.05
                                 ),
                                 p(""),
                                 sliderInput("BF", "BF in absolute terms:",
                                             min = 0, max = 100, value = 10
                                 ),
                                 p(""),
                                 actionButton("upload2","After data upload - update variable selectors"),
                                 p(""),
                                 uiOutput("VAL_selector"),
                                 p(""),
                                 uiOutput("FAC_selector"),
                                 p(""),
                                 textInput("user.dataset.name", "Give name for dataset",value = "UserDataset"),
                                 p(""),
                                 fileInput("OwnData", "Upload your own data...",
                                           accept=c("text/csv", "text/comma-separated-values,text/plain")),
                                 p(""),
                                 actionButton("upload","Add uploaded dataset to selection"),
                                 p(""),
                                 actionButton("go","Compute the results"),
                                 p("")
                    ),
                    
                    
                    mainPanel(
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),
                      
                      
                    tabsetPanel(
                        
                        
                        
                    tabPanel(title = "Rakenduse kirjeldus",textOutput("test"),
                                 fluidRow(includeMarkdown("documentation.md"))),
                        
                    tabPanel("Main effects"
                                 
                        ),
                    
                    tabPanel("Post hocs in table"
                             
                             
                    ),
                    
                    tabPanel("Post hocs plotted on a graph",
                             plotOutput("F1plotout"),
                             plotOutput("F2plotout"),
                             plotOutput("Interplotout")
                             
                    ),
                    
                    tabPanel("DEBUG PRINT PANEL",
                             textOutput("debug1"),
                             textOutput("debug2"),
                             textOutput("debug3"),
                             textOutput("debug4"),
                             textOutput("debug5"),
                             tableOutput("debug6")
                    )
                    
                    )
                    
                    ))))
