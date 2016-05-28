#suppressPackageStartupMessages()
#?actionbutton
#?eventreactive####
shinyUI(
  pageWithSidebar(
    headerPanel("Data Analyser"),
    sidebarPanel(
      fileInput('file','Upload csv file',accept = c('text/csv','text/comma-separated-values','.csv')),
      h5('OR'),
      selectInput('dataname', 'Select a dataset:', c(Choose='', paste(datalist$results[,3],datalist$results[,4],sep = ' -- ')), selectize=T),
      uiOutput('choiceg'),
      uiOutput("t_g"),
      uiOutput('data_fac'),
      uiOutput('partbt')
    ,width = 3),
    mainPanel(
      tabsetPanel(
        tabPanel('Plot',
                 h4('PLOT'),
                 fluidRow(
                   column(3,
                          uiOutput("dselectp")
                   ),
#                    column(3,offset = 0,
#                           uiOutput("dselectpy")
#                    ),
                   #column(3,offset = 0,
                   #      actionButton("goButton", "Go!")
                   #),
                   column(3,offset = 0,
                          downloadButton('downloadData', 'Save As')
                   )                 
                 ),
                 h4('vs'),
                  plotOutput("bplot"),
                  uiOutput('dselectp3d'),
                  scatterplotThreeOutput('bplot3d')
        ),
          tabPanel('Predictive Modelling',
         fluidRow(
           column(3,
                  h4('Neural Nets')
           ),
           uiOutput('nnbuttonpm'),
           plotOutput('nnplotpm'),
           verbatimTextOutput('nnt'),
           verbatimTextOutput('net_tab'),
           verbatimTextOutput('net_tab2'),
           uiOutput('svmbuttonpm'),
           plotOutput('svmplotpm'),
           verbatimTextOutput('svmt'),
           verbatimTextOutput('svm_tab'),
           verbatimTextOutput('svm_tab2'),
           uiOutput('gambuttonpm'),
           plotOutput('gamplotpm'),
           verbatimTextOutput('gamt'),
           verbatimTextOutput('gam_tab'),
           verbatimTextOutput('gam_tab2'),
           uiOutput('ldabuttonpm'),
           plotOutput('ldaplotpm'),
           verbatimTextOutput('ldat'),
           verbatimTextOutput('lda_tab'),
           verbatimTextOutput('lda_tab2'),
           uiOutput('rfbuttonpm'),
           plotOutput('rfplotpm'),
           verbatimTextOutput('rft'),
           verbatimTextOutput('rf_tab'),
           verbatimTextOutput('rf_tab2'),
           uiOutput('boostbuttonpm'),
           plotOutput('boostplotpm'),
           verbatimTextOutput('boostt'),
           verbatimTextOutput('boost_tab'),
           verbatimTextOutput('boost_tab2'),
           uiOutput('regbuttonpm'),
           plotOutput('regplotpm'),
           verbatimTextOutput('regt'),
           verbatimTextOutput('reg_tab'),
           verbatimTextOutput('reg_tab2'),
           uiOutput('nbbuttonpm'),
           plotOutput('nbplotpm'),
           verbatimTextOutput('nbt'),
           verbatimTextOutput('nb_tab'),
           verbatimTextOutput('nb_tab2'),
           uiOutput('knnbuttonpm'),
           plotOutput('knnplotpm'),
           verbatimTextOutput('knnt'),
           verbatimTextOutput('knn_tab'),
           verbatimTextOutput('knn_tab2'),
           uiOutput('Resbuttonpm'),
           verbatimTextOutput('Rest'),
           uiOutput('Timbuttonpm'),
           verbatimTextOutput('Timt'),
           plotOutput('Resplotpm')
         )#,
         #plotOutput("bplot"),
         #uiOutput('dselectp3d'),
          ),
        tabPanel('Cluster',
                 fluidRow(
                   column(3,
                          uiOutput("epbuttonc")
                   )
                 ),
                 plotOutput('eplotck'),
                 fluidRow(
                   column(3,
                          uiOutput("dselectc")
                   )
                   #                    column(3,offset = 0,
                   #                           uiOutput("dynamicselectk2")
                   #                    ),
                   #                    column(4,offset = 0,
                   #                           numericInput('k','Number of clusters:',3)
                   #                    )
                 ),
                 plotOutput('bplotck'),
                 uiOutput('dselectc3d2'),
                 scatterplotThreeOutput('bplotck2'),
                 h4('PAM'),
                 uiOutput('ppbuttonc'),
                 plotOutput('pplotck',height = '800px'),
                 h4('Calinski criterion'),
                 uiOutput('calpbuttonc'),
                 plotOutput('cplotck'),
                 h4('Bayesian Information Criterion for expectation-maximization, initialized by hierarchical clustering for parameterized Gaussian mixture models'),
                uiOutput('bicpbuttonc'),
                plotOutput('bicplotck1'),
                plotOutput('bicplotck2'),
                h4('NbClust-renam'),
                uiOutput('nbcpbuttonc'),
                plotOutput('nbcplotck'),
                textOutput('nbctextck'),
                h4('Hierarchical Clustering'),
                uiOutput('hpbuttonc'),
                plotOutput('hplotck'),
                 br(),
                 br(),
                 br(),
                 br(),
                 #imageOutput("imgg"),
                 br(),
                 br(),
                 br(),
                 br()
                 #imageOutput("imgb")
        ),
        tabPanel('Documentation',
                 h3('Help'),
                 textOutput('help'),
                 h3("Source Code (server.R and ui.R):",br(), a(" Github", href="https://github.com/usergaurav/da")))
      )
    ) 
  )
)

# Use observeEvent whenever you want to perform an action in 
# response to an event. (Note that "recalculate a value" does not 
#                        generally count as performing an action–see
#                        eventReactive for that.) 
# 
# Use eventReactive to create a calculated value that only updates 
# in response to an event. This is just like a normal reactive 
# expression except it ignores all the usual invalidations that come
# from its reactive dependencies; it only invalidates in response to
# the given event.
# 
# Both observeEvent and eventReactive take an ignoreNULL(T) parameter 
# that affects behavior when the eventExpr evaluates to NULL (or in 
# the special case of an actionButton, 0). In these cases, if 
# ignoreNULL is TRUE, then an observeEvent will not execute and an 
# eventReactive will raise a silent validation error. 
# This is useful behavior if you don't want to do the action or 
# calculation when your app first starts, but wait for the user to 
# initiate the action first (like a "Submit" button); whereas 
# ignoreNULL=FALSE is desirable if you want to initially perform 
# the action/calculation and just let the user re-initiate it (like
# a "Recalculate" button).

# # in ui.R
# fluidPage(
#   checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
#   selectizeInput('in2', 'Select a state', choices = state.name),
#   plotOutput('plot')
# )
# 
# # in server.R
# function(input, output) {
#   output$plot <- renderPlot({
#     validate(
#       need(input$in1, 'Check at least one letter!'),
#       need(input$in2 != '', 'Please choose a state.')
#     )
#     plot(1:10, main = paste(c(input$in1, input$in2), collapse = ', '))
#   })
# }

# select vs selectize
# selectInput('in6', 'Options', state.name, multiple=TRUE, selectize=F)
# selectInput('in6', 'Options', state.name, multiple=TRUE, selectize=TRUE)