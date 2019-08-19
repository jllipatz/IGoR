
### 12/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$slice$ui <- function()
  .IGoR$ui(page="slice",
    fluidRow(
      column(width=6,
        box(width='100%',
          fluidRow(
            column(width=6, numericInput("slice.top",.IGoR$s2(.IGoR$Z$slice$top),1)),
            column(width=6, uiOutput("slice.end"))
          ),
          checkboxInput("slice.drop",.IGoR$s4(.IGoR$Z$any$drop),FALSE)
      )),
      column(width=6, .IGoR$load.ui("slice"))
  ) )


.IGoR$page$slice$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"slice")
  
  output$slice.end <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      numericInput("slice.end",.IGoR$s2(.IGoR$Z$slice$end),nrow(get(input$main.data,envir=.GlobalEnv)))
  )
  
  output$slice.command2 <- renderUI(
    .IGoR$textarea("slice", "slice(range)", 3, 
      if (length(input$slice.end)>0) {
        n <- nrow(get(input$main.data,envir=.GlobalEnv))
        .IGoR$command2(
          if (is.na(input$slice.end)||is.na(input$slice.top)) ""
          else glue(
               if (input$slice.top>1)
                 if (input$slice.end<n)
                   if (input$slice.top!=input$slice.end) 
                     if (input$slice.drop)
                          "slice(-({input$slice.top}:{input$slice.end}))"
                     else "slice({input$slice.top}:{input$slice.end})"
                   else if (input$slice.drop)
                          "slice(-{input$slice.top})"
                     else "slice({input$slice.top})"
                 else
                   if (input$slice.drop)
                        "head({input$slice.top-1})"
                   else "tail({n-input$slice.top+1})"
               else     
                 if (input$slice.drop)
                      "tail({n-input$slice.end})"
                 else "head({input$slice.end})"
          )    )
      } )
  )

}
        