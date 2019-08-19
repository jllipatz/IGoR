
### 06/08/2019 1.03.3: Ajout de la possibilité de préfixer les noms des nouvelles variables
### 10/08/2019 1.04.2: Externalisation des libellés en français

### BUG: CHRCOLV ne peut pas comporter d'accent : problème d'encodage

.IGoR$page$spread$ui <- function()
  .IGoR$ui(page="spread",
    fluidRow(
      column(width=6,
        imageOutput("spread.long",height='200px'),
        uiOutput("spread.columns")
      ),
      column(width=6,
        imageOutput("spread.wide",height='200px'),
        .IGoR$load.ui("spread")
  ) ) )

.IGoR$page$spread$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"spread")
  
  output$spread.wide <- renderImage(list(src="images/wide.png"),deleteFile = FALSE)
  
  output$spread.long <- renderImage(list(src="images/long.png"),deleteFile = FALSE)
  
  output$spread.columns <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      box(width='100%',
        fluidRow(
          column(width=6, selectizeInput("spread.K",.IGoR$s1(.IGoR$Z$spread$var.k),
                                         choices = c(.IGoR$CHRCOLV,.columns(input$main.data,"character")))),
          column(width=6, uiOutput("spread.sep"))
        ),
        fluidRow(
          column(width=6, selectizeInput("spread.V", .IGoR$s1(.IGoR$Z$spread$var.v),
                                         choices = c(.IGoR$COLV,.columns(input$main.data))))
  )   ) )
  
  output$spread.sep <- renderUI(
    if (.isNotEmpty(input$spread.K))
      checkboxInput("spread.sep",.IGoR$s4(paste0(.IGoR$Z$spread$prefix,input$spread.K,"'")), FALSE)
  )

  output$spread.command2 <- renderUI(
    if (length(input$main.data)>0)
      .IGoR$textarea("spread", "spread(k,v)", 2,
        if (.isNotEmpty(input$spread.K)&&.isNotEmpty(input$spread.V))
          if (input$spread.K==input$spread.V) {
            output$spread.comment <-  renderText("*** ERREUR : Les colonnes 'Noms' et 'Valeurs' sont identiques.")
            ""
          } 
          else
            .IGoR$command2(
              "spread(",
              glue("{input$spread.K},{input$spread.V}"),
              if (.isTRUE(input$spread.sep)) ", sep=\"\"",
              ')'
  )   )     )                 

}