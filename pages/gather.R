### 28/06/2019 1.01.2: Corretion: widget gather.inverse qui n'apparaissait pas etbloquait la seletion de type 1


.IGoR$page$gather$ui <- function()
  div(id = "bloc_gather",
    fluidRow(
      column(width=4, 
        img(src="images/gather.png", height = "48px"),
        h3(span("Passage du format large vers le format long", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("gather"), "du package", strong("tidyr"), 
          span("collecte de l'information stockée en ligne dans différentes colonnes en de l'information stockée en une unique colonne sur plusieurs lignes", style='color:blue'), ". ",
          "Les noms des colonnes d'origine est conservée dans une seconde colonne de la table résultat.", br(),
          "Il est possible de transposer un ensemble de variables de types différents, la colonne du résultat prendra un type compatible, généralement 'caractère'."
    ) ) ),
    fluidRow(
      column(width=6,
        imageOutput("gather.wide",height='200px'),
        uiOutput("gather.control")
      ),
      column(width=6,
        imageOutput("gather.long",height='200px'),
        .IGoR$load.ui("gather"),
        box(width='100%',
          column(width=6, textInput("gather.out.K",.IGoR$s2("Colonne recevant les noms (K)"),"k")),
          column(width=6, textInput("gather.out.V",.IGoR$s2("Colonne recevant les valeurs (V)"),"v"))
    ) ) ),
    .IGoR$commandBox("gather")
  )


.IGoR$page$gather$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"gather")
  
  output$gather.wide <- renderImage(list(src="images/wide.png"),deleteFile = FALSE)
  
  output$gather.long <- renderImage(list(src="images/long.png"),deleteFile = FALSE)

  output$gather.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$select.ui("gather", NULL,
                      buttons.title=.IGoR$s2("Transposer les variables..."), buttons.all=FALSE, buttons.class=FALSE,
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

