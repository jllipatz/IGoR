
.IGoR$page$join$ui <- function()
  div(id = "bloc_join",
    fluidRow(
      column(width=4, 
        img(src="images/join.png", height = "46px"),
        h3(span("Jointure de deux tables sur égalité de clés", style="color: blue"))
      ),
      column(width=8, 
        p("Le package ", strong("dplyr")," offre plusieurs fonctions d'appariemment de deux tables. ",
          "L'appariemment est conditionné par le fait qu'un certain nombre de variables 'clés' issues de la première table aient des valeurs présentes ", strong("à l'identique"), " dans un ", strong("même nombre"), " de variables 'clés' de la seconde table.", br(),
          "Le résultat est une table contenant l'intégralité des variables des deux tables. En cas de doublon sur un nom de variable, les variables en cause sont suffixées par '.x' et '.y'.", 
          "Le nombre de lignes du résultat effectif dépend de ce qu'on souhaite faire lorsque les valeurs issues des 'clés' d'une table n'ont pas d'écho dans l'autre. ", br(),
          "Les fonctions sont complétées par le cas particulier où le nombre de clés est nul : la fonction ", code("crossing"), "du package ", strong("tidyr"), "produit alors un produit cartésien, ",
          "potentiellement volumineux, mais sur lequel il est ensuite possible de faire un filtre non limité à des égalités de variables.",br(),
          em("NOTE : la page se réinitialise dès que la jointure a fonctionné.")
    ) ) ),
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("join.columns"),
          uiOutput("join.data")
      )),
      column(width=6,
        box(width='100%',
          uiOutput("join.type")
        ),
        .IGoR$loadBox("join","join.out",NULL,.IGoR$s2(.IGoR$OUT))
    )),
    .IGoR$commandBox("join")
  )


.IGoR$page$join$sv <- function(input, output, session) {
  
  .IGoR$jServer(input,output,"join")
  
  output$join.type <- renderUI(
    if (.isNotEmpty(input$join.data)&&
        (length(input$join.columns2)==length(input$join.columns)))
      radioButtons("join.type", .IGoR$s2("Type de jointure :"),
        if (length(input$join.columns)>0)
          c(                         "Uniquement les lignes ayant un echo dans les deux tables" = "inner_join",
            "Les lignes de la table en entrée complétées ou non par celles de la seconde table" = "left_join",
            "Les lignes de la seconde table complétées ou non par celles de la table en entrée" = "right_join",
                 "Les lignes des deux tables qu'elles aient un écho ou non dans leur vis à vis" = "full_join",
                    "Les lignes de la table en entrée n'ayant aucun écho dans la seconde table" = "anti_join",
                         "Les lignes de la table en entrée ayant un écho dans la seconde table" = "semi_join")
        else
          c("Produit cartésien"="crossing")
  ))

  output$join.command2 <- renderUI(
    .IGoR$textarea("join", "...join(table,columns)", 4,
      if (.isNotEmpty(input$join.data)
          &&(length(input$join.columns2)==length(input$join.columns)))
        .IGoR$command2(
          if (length(input$join.columns)>0) {
            by <- .collapse(
                    ifelse(input$join.columns==input$join.columns2,
                           glue("\"{input$join.columns}\""),
                           glue("\"{input$join.columns}\" = \"{input$join.columns2}\"")
                  ) )
            if ((length(input$join.columns)>1)||(input$join.columns!=input$join.columns2)) by <- glue("c({by})")
            glue("{input$join.type}({input$join.data}, by={by})")
          }
          else glue("crossing({input$join.data})")
  ) )   )
  
  observeEvent(input$join.command2,
    .IGoR$try(input,output,"join",
      .fn=function (x) {
        t1 <- select_at(get(input$main.data,envir=.GlobalEnv),input$join.columns)
        t2 <- select_at(get(input$join.data,envir=.GlobalEnv),input$join.columns2)
        sprintf("NOTE : Le résultat va avoir %d x %d = %d lignes.",nrow(t1),nrow(t2),
          if (length(input$join.columns)==0) nrow(t1)*nrow(t2)
          else if (length(input$join.columns)==length(input$join.columns2)) {
            l <- input$join.columns2
            names(l) <- input$join.columns
            nrow(do.call(input$join.type,list(t1,t2,l)))
          }
        )
      }
  ))

}