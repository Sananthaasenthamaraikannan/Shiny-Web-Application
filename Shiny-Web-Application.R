library(shiny)
ui <- fluidPage(
  h2("Hi"),
  textInput("name", "Enter your name:"),
  textOutput("greet")
)
server <- function(input, output) {
  output$greet <- renderText({
    paste("Hello", input$name)
  })
}
shinyApp(ui = ui, server = server)
