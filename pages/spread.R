
### 06/08/2019 1.03.3: Ajout de la possibilité de préfixer les noms des nouvelles variables
### 10/08/2019 1.04.2: Externalisation des libellés en français
### 10/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
### 01/09/2020 1.10.2: Ajout d'une option d'utilisation de pivot_wider


.IGoR$page$spread$ui <- function()
  .IGoR$ui(page="spread",
    fluidRow(
      column(width=6,
        imageOutput("spread.long",height='200px'),
        uiOutput("spread.columns")
      ),
      column(width=6,
        imageOutput("spread.wide",height='200px'),
        if (paste0(version$major,version$minor)>="3.6.0") 
          box(width='100%', checkboxInput("spread.pivot",.IGoR$s2(.IGoR$Z$spread$pivot),FALSE)),
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
    if (.isNotEmpty(input$spread.K)&&.isFALSE(input$spread.pivot))
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
            if (.isTRUE(input$spread.pivot))
                 .IGoR$command2(
                   "pivot_wider(",
                   "names_from=", shQuote(input$spread.K),
                   ", values_from=", shQuote(input$spread.V),
                   ")"
                 )
            else .IGoR$command2(
                   "spread(",
                   .name(input$spread.K),
                   ",", .name(input$spread.V),
                   if (.isTRUE(input$spread.sep)) ", sep=\"\"",
                   ")"
  )   )          )                 

}