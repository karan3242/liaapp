
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Silver App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    
    actionButton(inputId = "update_file",label = "Update File"),
    conditionalPanel(condition = "(input.update_file % 2) == 1",
                     passwordInput(inputId = "password_file",label = "Password",placeholder = "e.g. 1234")
                     ),
    conditionalPanel(condition = "input.password_file == '4567' & (input.update_file % 2) == 1",
                     fileInput(inputId = "base_file",label = "New Base File:",accept = c(".xlsx")),
                     uiOutput("download_file"),
                     helpText(p("After updating the database,",strong("close"),"the application and",strong("restart"),"it after loading the",strong("new"),"updated base file.")),
                     br()
                     ),
    
    radioButtons("Datatype",label = "Choose type",
                 choices = list(
                   "single observation" = 0,
                   "data file" = 1
                   )),
    
    conditionalPanel(
      condition = "input.Datatype == 0",
      
      fluidRow(numericInput("x1",label="208/204", value=38.6106),
               numericInput("x2",label="207/204", value=15.6315709),
               numericInput("x3",label="206/204", value=18.5495))
    ),
    
    conditionalPanel(
      condition = "input.Datatype == 1",
      helpText(p("When uploading a .csv file please be attentive to guidelines below:"),
               p("Make sure",strong("only 3 columns"),"are in the file, and that their values correspond to the values of the database."),
               p("Example given: Make sure that the ratio 206Pb/204Pb is not in fact in its inverse form (204Pb/206Pb)"),
               p("Choose the correct column in which each ratio resides inside the file.")),
      fileInput('file1', "Choose CSV File",
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      fluidRow(selectInput("H1", label="208/204 in:", 
                           choices=list("Column 1"=2, "Column 2"=3,"Column 3"=4),
                           selected = 2),
               selectInput("H2", label="207/204 in:",
                           choices=list("Column 1"=2, "Column 2"=3,"Column 3"=4),
                           selected = 3),
               selectInput("H3", label="206/204 in:",
                           choices=list("Column 1"=2, "Column 2"=3,"Column 3"=4),
                           selected = 4))
    ),
    
    numericInput(inputId = "numobs",label = "Number of Observation to show",value = 5,min = 1,max = 30,step = 1,width = '100%'),
    
    
    
    checkboxGroupInput(inputId = "errorFilter",label = "Precision Filter:",
                       choices = outlier, selected = names(outlier),inline = TRUE),
    
    h5("*Precision calculated using", a(target="_blank","Mahalanobis Distance", href="http://classifion.sicyon.com/References/M_distance.pdf")),
    
    checkboxGroupInput(inputId = "typeFilter",label = "Type Filter:",
                       choices = type,inline = TRUE, selected = unname(unlist(type))),
    
    checkboxGroupInput(inputId = "ConstFilter",label = "Main Constituent Filter:",
                       choices = main,inline = TRUE, selected = unname(unlist(main))),
    
    "*Following filters pertain to graphs only:",
    
    selectizeInput(inputId = "selectCountry",label = "Choose Country:",choices = CountriesVar,
                   selected = "",multiple = TRUE),
    fluidRow(
      column(6,actionButton("checkCountryAll",label = "Select all countries")),
      column(6,actionButton("checkCountryNone",label = "De-Select all countries"))),
    br(),
      
    selectizeInput(inputId = "Reg",label = "Choose Region:",choices = RegionsVar,
                   selected = "",multiple = TRUE),
    
    actionButton("checkRegionNone",label = "De-Select all Regions"),
    br(),
    actionButton(inputId = "update",label = "Update filters by distance table"),
    
    br(),
    downloadButton(outputId = "download",label = "Download table to .csv file")


    ),
  
  # Show a plot of the generated distribution
  mainPanel(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Distance Table",
                         
                         dataTableOutput("dis")
                        
                         ),
                
                tabPanel("2d Graphs - Countries",
                         
                         column(width = 12,plotlyOutput("obsGraph2"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("obsGraph1"),align = "center")
                         
                         ),
                
                tabPanel("3d Graphs - Countries",
                         
                         plotlyOutput("obsGraph3d")
                         
                ),
                
                tabPanel("2d Graphs - Region",
                         
                         column(width = 12,plotlyOutput("regGraph2"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("regGraph1"),align = "center")
                         
                         ),
                
                tabPanel("3d Graphs - Region",
                         
                         plotlyOutput("regGraph3d")
                         
                )
          )
  ),
  
  
  # fluidPage(style="padding-top: 80px;",
  #           
  #           absolutePanel(
  #             top=667, left = 628, width = 400,
  #             draggable = TRUE,
  #             wellPanel(
  #               HTML(markdownToHTML(fragment.only=TRUE, text=c("Summary Table"))
  #                    ),
  #               fluidRow(column(3,tableOutput("countrySummary")),
  #                        column(2),
  #                        column(3,tableOutput("regionSummary")))
  #               ),
  #               
  #             
  #             style = "opacity: 0.92"
  #           ))
  
  
  
  )) 




