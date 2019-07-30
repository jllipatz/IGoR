
.IGoR$page$spread$ui <- function()
  div(id = "bloc_spread", h3("'spread' : Passage du format long vers le format large"),
    fluidRow(
      column(width=6,
        imageOutput("spread.long",height='200px'),
        box(width='100%', uiOutput("spread.columns"))
      ),
      column(width=6,
        imageOutput("spread.wide",height='200px'),
        .IGoR$loadBox("spread","spread.out")
      )
    ),
    .IGoR$commandBox("spread")
  )

.IGoR$page$spread$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"spread")
  
  output$spread.wide <- renderImage(list(src="images/wide.png"),deleteFile = FALSE)
  
  output$spread.long <- renderImage(list(src="images/long.png"),deleteFile = FALSE)
  
  output$spread.columns <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        selectizeInput("spread.K", label = "Variable donnant le nom des futures colonnes (K)",
                       choices = c(.IGoR$COLV,.columns(input$main.data,"character"))),
        selectizeInput("spread.V", label = "Variable donnant la valeur des futures colonnes (V)",
                       choices = c(.IGoR$COLV,.columns(input$main.data)))
        
  ))

  output$spread.command2 <- renderUI(
    if ((length(input$main.data)>0)
      &&.isnotEmpty(input$spread.K)&&.isNotEmpty(input$spread.V))
      .IGoR$textarea("spread", "spread(k,v)", 2,
        if (input$spread.K==input$spread.V) {
          output$spread.comment <-  renderText("*** ERREUR : Les colonnes 'Noms' et 'Valeurs' sont identiques.")
          ""
        } 
        else
          .IGoR$command2(glue("spread({input$spread.K},{input$spread.V})"))
  )   )                    

}