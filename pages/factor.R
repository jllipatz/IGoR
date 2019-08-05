
### TODO : Les noms de colonnes invalides ne sont pas gérés et doivent être quotés manuellement.

.IGoR$page$factor$ui <- function()
  div(id = "bloc_factor",
    fluidRow(
      column(width=4, 
        img(src="images/rename.png", height = "46px"),
        h3(span("Conversion de variables d'un type vers un autre", style="color: blue"))
      ),
      column(width=8, 
        p("Les fonctions de la famille", code("mutate"), " du package ", strong("dplyr"), "permettent aussi de ",
          span("changer le type d'une variable ou d'un ensemble de variables", style="color:blue"),".",
          "Les variables modifiées peuvent remplacer les anciennes, ou, si on ne réutilise pas leur nom, venir compléter la table résultat.",br(),
          "En R, il n'est généralement pas possible de modifier un objet existant, aussi tout ce que fait la fonction c'est créer une nouvelle table ",
          "contenant les données de l'ancienne table modifiées sur les variables sélectionnées.", br(),
          em("NOTE : si le nom de la table résultat est celui de la table courante, la page se réinitialise dès que la modification a fonctionné.")
    ) ) ),
    uiOutput("factor.control"),
    .IGoR$commandBox("factor")
  )


.IGoR$page$factor$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"factor")
  
  output$factor.control <- renderUI({
    output$factor.comment <- renderText("")
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("factor",buttons.title=.IGoR$s2("Changer le type des variables..."))),
        column(width=6,
          .IGoR$load.ui("factor",input$main.data),
          box(width='100%',
            column(width=6, 
              selectizeInput("factor.class.out",.IGoR$s2("vers le type :"),
                             choices=c("énumération (facteur)"="factor",
                                       "caractère"="as.character",
                                       "numérique double précision"="as.double",
                                       "numérique entier"="as.integer",
                                       "booléen"="as.logical",
                                       "date"="as.Date"))),
            column(width=6,
              checkboxInput("factor.short",.IGoR$s5("Réutiliser les noms"),TRUE),
              uiOutput("factor.empty")
      ) ) ) )
  })
 
  output$factor.empty <- renderUI(
    if (.isEQ(input$factor.class.out,"factor")
      &&(length(input$factor.type)>0)
      &&((input$factor.type!=2)
       ||((input$factor.type==2)
        &&((.isEQ(input$factor.class,"character")&&!input$factor.drop)
         ||(.isNE(input$factor.class,"character")&&input$factor.drop)))))
      checkboxInput("factor.empty",.IGoR$s5("Mettre 'valeur manquante' pour les chaînes vides"),TRUE)
  )

  output$factor.command2 <- renderUI(
    .IGoR$textarea("factor", "mutate(column=factor(column))", 4, 
      if ((length(input$factor.type)>0)&&(length(input$factor.short)>0)&&(length(input$factor.class.out)>0)) {
        na <- if ((input$factor.class.out=="factor")
                &&((input$factor.type!=2)
                 ||((input$factor.type==2)
                  &&((.isEQ(input$factor.class,"character")&&!input$factor.drop)
                   ||(.isNE(input$factor.class,"character")&&input$factor.drop))))
                &&.isTRUE(input$factor.empty)) ", exclude=''" else ""
        .IGoR$command2(
          "mutate",
          if (!input$factor.short) {
            old <- .IGoR$select.columns(input,output,"factor")
            if (length(old)==0) "() # Aucune variable à transformer!"
            else glue("({.collapse(paste0(old,'=',input$factor.class.out,'(',old,na,')'))})")
          }
          else
            paste0(
              if (input$factor.type==2) 
                if (input$factor.drop)
                     glue("_if(Negate(is.{input$factor.class}), ")
                else glue("_if(is.{input$factor.class}, ")
              else 
              if (input$factor.type==3) 
                if (input$factor.drop) 
                     "_at(c(), "
                else "_all("
              else glue("_at({.IGoR$select(input,'factor',vars=TRUE)}, "),
              input$factor.class.out,
              na,
              ")"
        )   ) 
      }
  ) ) 
  
  observeEvent(input$factor.command2,
    .IGoR$try(input,output,"factor",
      .fn=function(x) {
        y <- inner_join(
               do.call(data.frame,Map(class,get(input$main.data,envir=.GlobalEnv))) %>% gather(k,old),
               do.call(data.frame,Map(class,x)) %>% gather(k,new),
               by="k")
        m <- y %>% filter(old!=new) %>% count() %>% pull(n)
        n <- y %>%
             filter((old=="factor")&!(new %in% c("factor","character"))) %>%
             count() %>% pull(n)
        if (m==0) "NOTE : Il n'y a aucune variable à convertir."
        else paste0(
          glue("NOTE : Il y aura {.p('changement',m,FALSE)} de type."),
          if (n>0) "\nATTENTION : Convertir une variable facteur en autre chose que du caractère peut conduire à des résultats inattendus."
        )
      }
  ))

}

