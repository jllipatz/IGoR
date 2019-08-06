
### 06/08/2019 1.03.3: Ajout de la possibilité de préfixer les noms des nouvelles variables

.IGoR$page$spread$ui <- function()
  div(id = "bloc_spread",
    fluidRow(
      column(width=4, 
        img(src="images/spread.png", height = "48px"),
        h3(span("Passage du format large vers le format long", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("spread"), "du package", strong("tidyr"), 
          span("ventile de l'information stockée sur plusieurs lignes dans une seule colonne en de l'information stockée sur une seule ligne dans différentes colonnes", style='color:blue'), ". ",
          "Une seconde colonne indique les noms des différentes colonnes à créer dans la table résultat."
    ) ) ),
    fluidRow(
      column(width=6,
        imageOutput("spread.long",height='200px'),
        uiOutput("spread.columns")
      ),
      column(width=6,
        imageOutput("spread.wide",height='200px'),
        .IGoR$load.ui("spread")
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
      box(width='100%',
        fluidRow(
          column(width=6, selectizeInput("spread.K",.IGoR$s1("Noms des futures colonnes (K)"),
                                         choices = c(.IGoR$COLV,.columns(input$main.data,"character")))),
          column(width=6, uiOutput("spread.sep"))
        ),
        fluidRow(
          column(width=6, selectizeInput("spread.V", .IGoR$s1("Valeurs des futures colonnes (V)"),
                                       choices = c(.IGoR$COLV,.columns(input$main.data))))
  )   ) )
  
  output$spread.sep <- renderUI(
    if (.isNotEmpty(input$spread.K))
      checkboxInput("spread.sep",.IGoR$s4(paste0("Préfixer par '",input$spread.K,"'")), FALSE)
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