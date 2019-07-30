### 03/06/2019         Correction de l'affichage des modalités pour les chaînes de caractères
### 12/07/2019 1.02.0: Ajout d'une option de passage par summarise

.IGoR$page$distinct$ui <- function()
  div(id = "bloc_distinct",
    fluidRow(
      column(width=4,
        img(src="images/distinct.png", height = "46px"),
        h3(span("Modalités des variables et dénombrements", style="color: blue"))
        ),
        column(width=8, 
          p("En l'absence de demande de comptage, la fonction ", code("distinct"), " extrait les modalités distinctes d'une variable ou d'un croisement de variables. ",
            "Si aucune variable n'est sélectionnée, la fonction extrait les observations distinctes.", br(),
            "On peut également compter le nombre d'occurences pour chaque croisement de variables, en s'appuyant sur deux fonctions du package ", strong("dplyr"),
            ": soit sur ", code("count")," qui ne permet pas de modifier le nom de la variable recevant les comptages, soit sur ", code("summarise"),".",
            "Si aucune variable n'est sélectionnée, ces fonctions retournent le nombre d'observations de la table."
    ) ) ),
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("distinct.columns")
      ) ),
      column(width=6,
        .IGoR$loadBox("distinct","distinct.out"),
        box(width='100%',
          fluidRow(
            column(width=6, radioButtons("distinct.type","", c("Modalités distinctes"=1, "Compter les observations"=2))),
            column(width=6, uiOutput("distinct.more"))
      ) ) )
    ),
    .IGoR$commandBox("distinct")
  )


.IGoR$page$distinct$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"distinct")
  
  output$distinct.more <- renderUI(
    if (.isEQ(input$distinct.type,2)) 
      textInput("distinct.name","Nom de la variable de dénombrement","n")
    else
      checkboxInput("distinct.sort","Trier les modalités",TRUE))
  
  output$distinct.columns <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      selectizeInput("distinct.columns", label=.IGoR$VARS,
                     multiple = TRUE, options = list(placeholder = .IGoR$ALL),
                     choices = .columns(input$main.data)
    ))

  output$distinct.command2 <- renderUI(
   .IGoR$textarea("distinct", "distinct(columns)", 4,
      if (length(input$distinct.type)>0)
        .IGoR$command2(
          if (input$distinct.type==2)
            if (.isNE(input$distinct.name,"n"))
              paste0(
                .IGoR$group_by(input,"distinct"),
                glue("summarise({input$distinct.name}=n())"),
                .IGoR$ungroup(input,"distinct",1)
              )
            else glue("count({.collapse(input$distinct.columns)})")
          else glue("distinct({.collapse(input$distinct.columns)})"),
          if ((input$distinct.type==1)&&.isTRUE(input$distinct.sort))
            paste0(NL,glue("arrange({.collapse(input$distinct.columns)})"))
  ) )   )
  
  observeEvent(input$distinct.command2,
    .IGoR$try(input,output,"distinct",
      .fn=function(x) 
        if (input$distinct.type==2)
          sprintf("NOTE : Le résultat aura %d ligne(s).",nrow(x))
        else
          if (ncol(x)==1)
               sprintf("NOTE : Il y a %d modalité(s) :\n %s.",nrow(x),
                       (if (is.character(x[[1]])||is.factor(x[[1]])) .collapse1 else .collapse)(x[[1]]))
          else sprintf("NOTE : Il y a %d modalité(s) croisée(s).",nrow(x)),
      .subset=glue("select({.collapse(input$distinct.columns)})")
  ))

}