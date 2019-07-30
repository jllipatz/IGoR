
.IGoR$page$skim$ui <- function()
  div(id = "bloc_skim",
    fluidRow(
      column(width=4, 
        img(src="images/skim.png", height = "46px"),
        h3(span("Analyse rapide de la table courante", style="color: blue"))
    ),
    column(width=8, 
      p("La fonction ", code("skim"), " du package ", strong("skimr"), " produit une analyse rapide du contenu statistique d'une table.",
        "Pour chaque variable, en fonction de son type, elle calcule une batterie adaptée d'indicateurs statistiques de dispersion.", br(),
        "Le résultat est une table de données appartenant à un type de table (", em("skim_df"), ") dont l'édition dans cette page conduit à une présentation améliorée de son contenu. ",
        "Mais cette table peut être interrogée ou re-travaillée de façon standard."
    ) ) ),
    fluidRow(
      column(width=6, uiOutput("skim.control")),
      column(width=6, .IGoR$loadBox("skim","skim.out"))
    ),
    .IGoR$commandBox("skim")
  )

.IGoR$page$skim$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"skim")
  
  output$skim.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$select.ui("skim", NULL,
                      buttons.title="Analyser les variables...", buttons.all=FALSE, buttons.class=FALSE,
                      drop=FALSE)
  )
  
  .IGoR$select.what(input,output,"skim", columns.all=TRUE, buttons.class=FALSE)
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