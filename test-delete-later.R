library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("input1", "Input 1:", value = 5),
      textInput("input2", "Input 2:", value = "Hello")
    ),
    mainPanel(
      verbatimTextOutput("output")
    )
  )
)

server <- function(input, output, session) {
  # Define a reactive expression based on input1
  reactiveValue1 <- reactive({
    input$input1
  })
  
  # Define an observer to update input2 based on input1
  observeEvent(reactiveValue1(), {
    if (reactiveValue1() > 10) {
      updateTextInput(session, "input2", value = "Large value!")
    } else {
      updateTextInput(session, "input2", value = "Small value!")
    }
  })
  
  # Output the updated input values
  output$output <- renderPrint({
    paste("Input 1:", input$input1)
    paste("Input 2:", input$input2)
  })
}

shinyApp(ui, server)
