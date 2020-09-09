library(shiny)


pacman::p_load(pacman, rio, tidyverse, dplyr, ggplot2, rlang, caret, randomForest)

# Features of the dataframe
features <- c('setting1',	'setting2',	's2',	's3',	's4',
              's6',	's7',	's8',	's9', 's11',	's12',	's13',	's14',	's15',	's17',	
              's20',	's21')

# Read in the random forest model
final_model <- readRDS("../../results/final_model.rds")


ui <- fluidPage(
   
   titlePanel("Input Sensor Data"),
   tags$style("#pred {font-size:30px;
              display:block; }"),
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = 'eid', label = 'Which engine', choices = seq(1,100)),
         h4(helpText('Upload the Sensor Data file as a .csv')),
         fileInput('file','Upload the sensor data'),
         #tags$hr()
         checkboxInput(inputId = 'header', label='Does it have a header?', value=FALSE)
      ),
      
      mainPanel(
         uiOutput('tb')
      )
   )
)

server <- function(input, output) {
   
  # Input file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    isolate({
      data1 <-read.csv(file = file1$datapath, header = input$header)
      colnames(data1) <- features
    })
    data1
  })
  
  # Converted dataframe for plotting purposes
  cdf <- reactive({
    #isolate({
      cdf1 <- gather(data(), 'Sensor', 'Value')
      cdf1$Sensor <- factor(cdf1$Sensor, levels=features)
    #})
    cdf1
  })
  
  # Create the model input by making the lagged features
  modelInput <- reactive({
    isolate({
      lagged_df <- data.frame(data())
      
      for (i in features){
        for (j in 1:7){
          tempv <- paste(i,j,sep='_lag_')
          lagged_df <-
            lagged_df %>%
            mutate(!!sym(tempv) := dplyr::lag(!!sym(i), n=j, default=NA))
        }
      }
      lagged_df <- na.omit(lagged_df)
      #lagged_df <- cbind(engine_id=input$eid,lagged_df)
      lagged_df <- cbind(engine_id=strtoi(input$eid),lagged_df)
    })
    lagged_df
  })
  
  # Save the predictions
  predictions <- reactive({
    isolate({
      preds <- predict(final_model, modelInput())
    })
    preds[[1]]
  })
  #gather(data(), 'Sensor', 'Value')
  #cdf$Sensor <- factor(cdf$Sensor, levels=features)
  
  
  # Output the model input with lagged features for inspection
  output$sum <- renderTable({
    if(is.null(modelInput())){return ()}
    modelInput()
    
  })
  
  # Output what the user had as input
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  
  # Make a panel plot of all of the sensor readings
  output$plot <- renderPlot({
    ggplot(cdf(), aes_string(x=as.numeric(row.names(cdf())), y='Value')) + 
      geom_line() + 
      facet_grid(Sensor ~ ., scales = 'free') +
      theme(
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        axis.text = element_blank()
        #axis.ticks = element_blank()
      )
  })
  
  # Output the model predictions as text
  output$pred <- renderText({
    ifelse(predict(final_model, modelInput())[[1]]==0,'No maintenance required', 'At risk of failure. Please perform scheduled maintenance.')
  })
  
  
  # Tabular output with default as the cluster diagram
  output$tb <- renderUI({
    if(is.null(data()))
      tags$img(src='cluster_viz.png', heigth=600, width=800)
    else
      tabsetPanel(tabPanel("Plot", plotOutput("plot", height=800, width=1200)),tabPanel("Data", tableOutput("table")),tabPanel("Model Prediction", textOutput("pred")))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

