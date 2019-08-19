
### 10/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$skim$ui <- function()
  .IGoR$ui(page="skim",
    fluidRow(
      column(width=6, uiOutput("skim.control")),
      column(width=6, .IGoR$load.ui("skim"))
  ) )


.IGoR$page$skim$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"skim")
  
  output$skim.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$select.ui("skim", buttons.title=.IGoR$s2(.IGoR$Z$skim$skim),
                      buttons.all=FALSE, buttons.class=FALSE,
                      drop=FALSE)
  )
  
  .IGoR$select.what(input,output,"skim", columns.all=TRUE)
  .IGoR$select.drop(input,output,"skim")
 
   output$skim.command2 <- renderUI(
     .IGoR$textarea("skim", "skim(columns)", 3,
       if ((length(input$skim.type)>0)
         &&((input$skim.type<4)||.isNotEmpty(input$skim.pattern)))
         .IGoR$command2(
           "skim(",
           .IGoR$select(input,"skim"),
            ")"
   ) )   )
  
}