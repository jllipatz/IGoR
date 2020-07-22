
### 22/07/2020 1.09.0

.IGoR$page$union$ui <- function()
  .IGoR$ui(page="union",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("union.data")
      ) ),
      column(width=6, 
        .IGoR$load.ui("union"),
        uiOutput("union.type")
  ) ) )


.IGoR$page$union$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"union")
  
  output$union.data<- renderUI({
    .IGoR$test$list
    selectizeInput("union.data", .IGoR$s1(.IGoR$Z$all$join.data), choices=c(.IGoR$TABLE,.tables()))
  })
  
  output$union.type <- renderUI(
    if (.isNotEmpty(input$union.data))
      box(width='100%',
        radioButtons("union.type", .IGoR$s2(.IGoR$Z$union$type),
                     .IGoR$Znames("union","type",c("union","intersect","setdiff"))
  )   ) )
  
  output$union.command2 <- renderUI(
    .IGoR$textarea("union", "...(table,table2)", 4,
      if (.isNotEmpty(input$union.data)
          &&(length(input$union.type)>0))
        .IGoR$command2(
          glue("{input$union.type}({input$union.data})")
  ) )   )
                       
}
  