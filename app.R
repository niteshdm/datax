## Only run examples in interactive R sessions
if (interactive()) {
  ui <- fluidPage(
    includeCSS("style.css"),
    h1("Data exploration and Modeling"),
    
    navlistPanel(
      tabPanel("Data",  sidebarLayout(
        mainPanel(
          fileInput(
            "file1",
            "Upload training dataset(CSV File)",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          tags$hr(),
          checkboxInput("header", "Header", TRUE)
        ),
        
        mainPanel(tableOutput("contents"))
      )),
      tabPanel(
        "Exploration",
        radioButtons(
          "ptype",
          "Choose type:",
          list("Summary", "Structure", "Correlation", "Describe")
        ),
        mainPanel(verbatimTextOutput("value"))
      ),
      tabPanel(
        "Plot",
        selectInput(
          "select",
          label = "Select Plot Type",
          choices = c("Histogram", "Scatter", "Boxplot")
        ),
        conditionalPanel(
          condition = "input.select == 'Histogram'",
          headerPanel("Histogram"),
          sidebarPanel(selectInput('x1col', 'X Variable', "")),
          mainPanel(plotOutput('MyPlot1'))
        ),
        conditionalPanel(
          condition = "input.select == 'Boxplot'",
          headerPanel("Boxplot"),
          sidebarPanel(selectInput('x3col', 'X Variable', "")),
          mainPanel(plotOutput('MyPlot2'))
        ),
        conditionalPanel(
          condition = "input.select == 'Scatter'",
          headerPanel("Scatter"),
          sidebarPanel(
            selectInput('x2col', 'X Variable', ""),
            selectInput('y2col', 'Y Variable', "", selected = "")
            
          ),
          mainPanel(plotOutput('MyPlot3'))
        )
      ),
      tabPanel(
        "Model",
        sidebarPanel(
          selectInput(
            "algo",
            label = "Select Algorithm",
            choices = c("Multiple Regression")
          )
        ),
        conditionalPanel(
          condition = "input.algo == 'Multiple Regression'",
          headerPanel("Multiple Regression")
          ,
          sidebarPanel(
          p("Select the inputs for the Independent Variable"),
          uiOutput("choose_columns"),
          p("Select the inputs for the Dependent Variable"),
          selectInput(
            inputId = "y4col",
            label = "Dependent Variables",
            multiple = FALSE,
            ""
          )),
          
          mainPanel(
            verbatimTextOutput(outputId = "RegSum"),
            verbatimTextOutput(outputId = "rmse")
          )
        ),
        conditionalPanel(
          condition = "input.algo == 'M5 Prime'",
          headerPanel("M5 Prime"),
          mainPanel(
            verbatimTextOutput(outputId = "m5p")
          )
        ),
        conditionalPanel(
          condition = "input.algo == 'Random Forest'",
          headerPanel("Random Forest"),
          mainPanel(
            verbatimTextOutput(outputId = "randf")
          )
        )
        
      ),tabPanel(
        "Test",sidebarLayout(
          mainPanel(
            fileInput(
              "file2",
              "Upload testing dataset(CSV File)",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
          ),
          
          mainPanel(tableOutput("regrt"),downloadButton('download',"Download Prediction Result"))
        ))
    )
  )
  
  
  
  server <- function(input, output, session) {
    library(psych)
    library(corrplot)
    library(RWeka)
    library(randomForest)
    
    filedata <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df <- read.csv(inFile$datapath, header = input$header)
      updateSelectInput(
        session,
        inputId = 'x1col',
        label = 'X Variable',
        choices = names(df),
        selected = names(df)
      )
      updateSelectInput(
        session,
        inputId = 'x2col',
        label = 'X Variable',
        choices = names(df),
        selected = names(df)
      )
      updateSelectInput(
        session,
        inputId = 'y2col',
        label = 'Y Variable',
        choices = names(df),
        selected = names(df)[2]
      )
      updateSelectInput(
        session,
        inputId = 'x3col',
        label = 'X Variable',
        choices = names(df),
        selected = names(df)
      )
      updateSelectInput(
        session,
        inputId = 'x4col',
        label = 'X Variable',
        choices = names(df),
        selected = names(df)
      )
      updateSelectInput(
        session,
        inputId = 'y4col',
        label = 'Y Variable',
        choices = names(df),
        selected = names(df)[2]
      )
      
      
      return(df)
      
    })
    
    output$contents <- renderTable({
      filedata()
    })
    
    
    
    output$value <- renderPrint(ptype <-
                                  ifelse(
                                    input$ptype == "Summary",
                                    print(summary(filedata())),
                                    ifelse(
                                      input$ptype == "Structure",
                                      print(filedata()) ,
                                      ifelse(
                                        input$ptype == "Correlation",
                                        print(cor(data.matrix(filedata(
                                        )))),
                                        ifelse(input$ptype == "Describe", print(describe(filedata(
                                          
                                        ))))
                                      )
                                    )
                                  ))
    
    
    output$MyPlot1 <- renderPlot({
      if (is.null(input$file1))
        return("No data uploaded")
      x1    <- filedata()[, input$x1col]
      bins1 <- nrow(filedata())
      hist(x1,
           breaks = bins1,
           col = 'darkgray',
           border = 'red')
      
      
    })
    
    output$MyPlot2 <- renderPlot({
      if (is.null(input$file1))
        return("No data uploaded")
      x2    <- filedata()[, input$x3col]
      boxplot(x2, col = 'darkgray', border = 'red')
      
      
      
      
    })
    
    output$MyPlot3 <- renderPlot({
      if (is.null(input$file1))
        return("No data uploaded")
      x3 <- filedata()[, c(input$x2col, input$y2col)]
      plot(x3, col = "red")
      
    })
    
    lm1 <-
      reactive({
        lm(reformulate(input$columns, input$y4col), data = filedata())
      })
    
    output$RegSum <- renderPrint({
      if (is.null(input$file1))
        return("No data uploaded")
      
      
      summary(lm1())
    })
    
    output$rmse <- renderPrint({
      if (is.null(input$file1))
        return("No results to display")
      RSS <- c(crossprod(lm1()$residuals))
      MSE <- RSS / length(lm1()$residuals)
      RMSE <- sqrt(MSE)
      
      paste0("RMSE:  ",RMSE)
      
      
    })
    
    # output$m5p <- renderPrint({
    #   if (is.null(input$file1))
    #     return("No data uploaded")
    #   
    #   mp <-
    #     reactive({
    #       M5P(reformulate(input$columns, input$y4col), data = filedata())
    #     })
    #   summary(mp())
    # })
    # 
    # 
    # output$randf <- renderPrint({
    #   if (is.null(input$file1))
    #     return("No data uploaded")
    #   
    #   rf <-
    #     reactive({
    #       randomForest(reformulate(input$columns, input$y4col), data = filedata())
    #     })
    #   summary(rf())
    # })
    
    
    
    
    
    
    
    output$choose_columns <- renderUI({
      if (is.null(input$file1))
        return()
      # Get the data set with the appropriate name
      dat <- filedata()
      colnames <- names(dat)
      
      # Create the checkboxes and select them all by default
      checkboxGroupInput("columns",
                         "Choose columns",
                         choices  = colnames,
                         selected = colnames)
    })
    
    
    
    output$data_table <- renderTable({
      if (is.null(input$file1))
        return()
      
      dat <- filedata()
      
      # Make sure columns are correct for data set (when data set changes, the
      # columns will initially be for the previous data set)
      if (is.null(input$columns) ||
          !(input$columns %in% names(dat)))
        return()
      
      # Keep the selected columns
      dat <- dat[, input$columns, drop = FALSE]
      
      
    })
    
    
    testdata <- reactive({
      inFile2 <- input$file2
      if (is.null(inFile2))
        return(NULL)
      return(read.csv(inFile2$datapath, header = input$header))
    })
    
    output$testd <- renderTable({
      head(testdata())
    })
    
    
    pred <- reactive({
       if (is.null(testdata()))
         return(NULL)
      test <- predict(lm1(),testdata())
      test
    })
    
    
    # output$regrt <- renderTable({
    #   pred()
    #   
    # })
    # 
    
    output$download <- downloadHandler(
      filename = function(){"prediction_result.csv"}, 
      content = function(filename){
        write.csv(pred(), filename)
      }
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  
  shinyApp(ui, server)
}
