
### 28/06/2019 1.01.2: Correction: widget gather.inverse qui n'apparaissait pas et bloquait la selection de type 1
### 10/08/2019 1.04.2: Externalisation des libellés en français
### 10/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
### 02/09/2020 1.10.2: Ajout d'une option d'utilisation de pivot_longer


.IGoR$page$gather$ui <- function()
  .IGoR$ui(page="gather",
    fluidRow(
      column(width=6,
        imageOutput("gather.wide",height='120px'),
        if (paste0(version$major,version$minor)>="3.6.0") 
          box(width='100%', checkboxInput("gather.pivot",.IGoR$s2(.IGoR$Z$gather$pivot),FALSE)),
        uiOutput("gather.control")
      ),
      column(width=6,
        imageOutput("gather.long",height='200px'),
        .IGoR$load.ui("gather"),
        box(width='100%',
          fluidRow(
            column(width=6, textInput("gather.out.K",.IGoR$s2(.IGoR$Z$gather$out.k),"k")),
            column(width=6, textInput("gather.out.V",.IGoR$s2(.IGoR$Z$gather$out.v),"v"))
          )
  ) ) ) )


.IGoR$page$gather$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"gather")
  
  output$gather.wide <- renderImage(list(src="images/wide.png"),deleteFile = FALSE)
  
  output$gather.long <- renderImage(list(src="images/long.png"),deleteFile = FALSE)

  output$gather.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$select.ui("gather", NULL,
                      buttons.title=.IGoR$s2(.IGoR$Z$gather$gather), buttons.all=FALSE,
                      buttons.class=.isTRUE(input$gather.pivot),
                      drop=FALSE)
  )
  
  .IGoR$select.what(input,output,"gather",columns.all=TRUE)
  .IGoR$select.drop(input,output,"gather")

  output$gather.command2 <- renderUI(
    .IGoR$textarea("gather", "gather(k,v,columns)", 2,
      if ((length(input$gather.type)>0)
        &&((input$gather.type<4)||.isNotEmpty(input$gather.pattern)))
        if (.isTRUE(input$gather.pivot))
             .IGoR$command2(
              "pivot_longer(",
              if (input$gather.type==2)
                   paste0("is.",input$gather.class) %>% {if (.isTRUE(input$gather.drop)) paste0("Negate(",.,")") else .}
              else .IGoR$select(input,"gather") %>% {if (.isNotEmpty(.)) paste0("c(",.collapse0(.),")") else "everything()"},
              ", names_to=", shQuote(input$gather.out.K),
              ", values_to=", shQuote(input$gather.out.V),
              ")"
            )
        else .IGoR$command2(
              "gather(",
              .name(input$gather.out.K),
              ",", .name(input$gather.out.V),
              .IGoR$select(input,"gather") %>% {if (.isNotEmpty(.)) paste0(", ",.)},
              ")"
            )
  ) )   
  
  observeEvent(input$gather.command2,
    .IGoR$try(input,output,"gather", .fn=function(x) sprintf(.IGoR$Z$gather$msg.result, ncol(x))
  ))

}

