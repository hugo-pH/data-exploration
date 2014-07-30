shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("d.source", label = h3("Select the source of the data"),
                   choices = list("Public dataset" = "public", 
                                  ".csv file" = "csv"), selected = "public"),
#       fileInput('file1', 'Choose file to upload',
#                 accept = c(
#                   'text/csv',
#                   'text/comma-separated-values',
#                   'text/tab-separated-values',
#                   'text/plain',
#                   '.csv',
#                   '.tsv'
#                 )
#       ),
#       tags$hr(),
#       checkboxInput('header', 'Header', TRUE),
#       radioButtons('sep', 'Separator',
#                    c(Comma=',',
#                      Semicolon=';',
#                      Tab='\t'),
#                    ','),
#       radioButtons('quote', 'Quote',
#                    c(None='',
#                      'Double Quote'='"',
#                      'Single Quote'="'"),
#                    '"'),
#       selectInput("dataset",
#                   label="Choose the dataset",
#                   choices = c("PlantGrowth", "InsectSprays", "germination", "wheat",
#                               "potato", "ryegrass", "eden.potato", "darwin.maize", 
#                               "adugna.sorghum", "apple.uniformity", "beall.webworms",
#                               "blackman.wheat","bliss.borers")
#       ), 
      uiOutput("select.source"),
      uiOutput("select.header"),
      uiOutput("select.sep"),
      uiOutput("select.csv"),
      uiOutput("select.res_var"),
      uiOutput("select.gr_var"),
      checkboxInput("check.gr2",
                    label = "Select a second agrupation variable",
                    value = FALSE),
      uiOutput("select.gr2"),
      uiOutput("check.gr3"),
      uiOutput("select.gr3"),
      radioButtons("stat", label = "Use parametric or nonparametric tests",
                   choices = list("parametric" = "par", "nonparametric" = "nonpar"), 
                   selected = NULL)
    ),
    
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("Boxplot", plotOutput("boxplot")),
                  tabPanel("Boxplot-facets", plotOutput("box.facet")),
                  tabPanel("Normality",  plotOutput("histo"), verbatimTextOutput("norm"), plotOutput("qqplot")),
                  tabPanel("Homocedasticity", plotOutput("homo.plot"), verbatimTextOutput("homo")),
                  tabPanel("Differences", plotOutput("barplot_stat"), verbatimTextOutput("aov"), verbatimTextOutput("groups"))
                  
      )
    )
  )
))