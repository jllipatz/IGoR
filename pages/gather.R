### 28/06/2019 1.01.2: Corretion: widget gather.inverse qui n'apparaissait pas etbloquait la seletion de type 1


.IGoR$page$gather$ui <- function()
  div(id = "bloc_gather", h3("'gather' : Passage du format large vers le format long"),
    fluidRow(
      column(width=6,
        imageOutput("gather.wide",height='200px'),
        uiOutput("gather.control")
      ),
      column(width=6,
        imageOutput("gather.long",height='200px'),
        .IGoR$loadBox("gather","gather.out"),
        box(width='100%',
          textInput("gather.out.K","Colonne recevant les noms (K)","k"),
          textInput("gather.out.V","Colonne recevant les valeurs (V)","v")
    ))),
    .IGoR$commandBox("gather")
  )


.IGoR$page$gather$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"gather")
  
  output$gather.wide <- renderImage(list(src="images/wide.png"),deleteFile = FALSE)
  
  output$gather.long <- renderImage(list(src="images/long.png"),deleteFile = FALSE)

  output$gather.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$select.ui("gather", NULL,
                      buttons.title="Transposer les variables...", buttons.all=FALSE, buttons.class=FALSE,
                      drop=FALSE)
  )
  
  .IGoR$select.what(input,output,"gather", buttons.class=FALSE)
  .IGoR$select.drop(input,output,"gather")

  output$gather.command2 <- renderUI(
    .IGoR$textarea("gather", "gather(k,v,columns)", 2,
      if ((length(input$gather.type)>0)
        &&((input$gather.type<4)||.isNotEmpty(input$gather.pattern)))
        .IGoR$command2(
          glue("gather({input$gather.out.K},{input$gather.out.V}"),
          {
            v <- .IGoR$select(input,"gather")
            if (.isNotEmpty(v)) paste0(", ",v)
          },
          ")"
  ) )   )
  
  observeEvent(input$gather.command2,
    .IGoR$try(input,output,"gather", .fn=function(x) sprintf(.IGoR$OUTNCOL, ncol(x))
  ))

}

