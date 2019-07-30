
### BUG coalesce(a,0) ne marche pas si a est "integer", faire coalesce(a,0L)

.IGoR$page$mutate$ui <- function()
  div(id = "bloc_mutate",
    fluidRow(
      column(width=4, 
        img(src="images/mutate.png", height = "48px"),
        h3(span("Créer ou remplacer une variable", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("mutate"), " du package ", strong("dplyr"), "permet de ", span("créer une nouvelle variable", style="color:blue")," dans une table, ",
          "ou si une variable de même nom existe déjà, de remplacer cette dernière.", br(),
          "Le calcul des données de la nouvelle variable peut être fait en groupant les observations sur les mêmes modalités d'un ensemble de variables.",
          "Dans ce cas le calcul sera réalisé groupe par groupe et la table résultat sera l'agrégation des résultats obtenus sur les différents groupes.",
          "Ceci n'est utile que lorsque le calcul utilise une fonction retournant un indicateur statistique : ce dernier sera produit groupe par groupe.", br(),
          "En R, il n'est généralement pas possible de modifier un objet existant, aussi tout ce que fait la fonction c'est créer une nouvelle table ",
          "contenant les données de l'ancienne table complétées par la nouvelle variable.", br(),
          em("NOTE : si le nom de la table résultat est celui de la table courante, la page se réinitialise dès que la modification a fonctionné.")
    ) ) ),
    uiOutput("mutate.control"),
    .IGoR$commandBox("mutate")
  )


.IGoR$page$mutate$sv <- function(input, output, session) {

  .IGoR$aServer(input,output,"mutate")
    
  output$mutate.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=8, .IGoR$expr.ui(input,"mutate")),
        column(width=4,
          .IGoR$loadBox("mutate",input$main.data),
          box(width='100%',
            textInput("mutate.new",.IGoR$NEWCOL,"mutate.new")
        ))
    ))
  
  output$mutate.expr.what <- renderUI(
    if (length(input$mutate.type)>0)
      if (input$mutate.type==1)
        textInput("mutate.expr","Formule de calcul")
      else
      if (input$mutate.type==2)
        tagList(
          uiOutput("mutate.fun"),
          fluidRow(
            column(width=4,
              selectizeInput("mutate.old", label=.IGoR$INVAR,
                             choices=c(.IGoR$COLV,.columns(input$main.data)))
            ),
            column(width=4,uiOutput("mutate.arg1")),
            column(width=4,uiOutput("mutate.arg2"))
         ))
       else
         tagList(
           column(width=4,
             selectizeInput("mutate.old1", label=.IGoR$INVAR,
                            choices=c(.IGoR$COLV,.columns(input$main.data)))
           ),
           column(width=4,
              selectizeInput("mutate.op",label="Opérateur",
                      choices=c("<operateur>"='',
                                       "plus"="+",
                                      "moins"="-",
                                       "fois"="*",
                                 "divisé par"="/",
                                         "et"="&",
                                         "ou"="|",
                                       "égal"="==",
                               "différent de"="!=",
                                "supérieur à"=">",
                        "supérieur ou égal à"=">=",
                                "concaténé à"=" paste0"))
           ),
           column(width=4,
              selectizeInput("mutate.old2", label=.IGoR$INVAR,
                            choices=c(.IGoR$COLV,.columns(input$main.data)))
           )
         )
  )
  
  output$mutate.fun <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) {
      c <- get(input$main.data,envir=.GlobalEnv)[[input$mutate.old]]
      fluidRow(
        column(width=8,
          selectizeInput("mutate.fun","",
            choices=
              if (is.character(c))
                c("Changer d'encodage..."="c2c>iconv",
                  "Mettre en majuscules"="c0 :str_to_upper",
                  "Mettre en minuscules"="c0 :str_to_lower",
                  "Calculer la longueur"="c0 :str_length",
                  "Extraire la chaîne entre les positions..."="c2n-str_sub",
                  "Extraire la chaîne à partir de la position..."="c1n-str_sub",
                  "Chercher une expression régulière..."="c1c:str_detect",
                  "Extraire une expression régulière..."="c1c:str_extract",
                  "Remplacer une expression régulière..."="c2c>str_replace",
                  "Remplacer les valeurs manquantes..."="p1c=coalesce",
                  "Propager les valeurs non manquantes"="r0 :na.locf")
              else
              if (is.numeric(c))
                c("Somme"="n0 :sum",
                  "Moyenne"="n0 :mean",
                  "Quantile..."="n1n:quantile",
                  "Valeur maximum"="n0 :max",
                  "Valeur minimum"="n0 :min",
                  "Première valeur"="m0 :first",
                  "Dernière valeur"="m0 :last",
                  "Remplacer les valeurs manquantes..."="p1n=coalesce",
                  "Propager les valeurs non manquantes"="r0 :na.locf",
                  "Formatter..."="x1c:sprintf")
              else
                c("Transformer en caractères"="f0 :as.character",
                  "Propager les valeurs non manquantes"="r0 :na.locf")
        )),
        column(width=4, 
          checkboxInput("mutate.pipe","Utiliser le pipe", TRUE),
          uiOutput("mutate.narm")
      ))
  })
  
  output$mutate.narm <- renderUI(
    if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,1,1)=='n'))
      checkboxInput("mutate.narm",.IGoR$NARM,TRUE)
  )

  output$mutate.arg1 <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) 
      if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,2,2)>0))
        if (substr(input$mutate.fun,3,3)=="c")
          textInput("mutate.chr.arg1",
            if (substr(input$mutate.fun,4,4)==">") "de"
            else if (substr(input$mutate.fun,4,4)=="=") "par" else "",
            switch(substring(input$mutate.fun,5),
              iconv="850",
              sprintf="<%5d>"
          ))
        else
          numericInput("mutate.num.arg1",
            if (substr(input$mutate.fun,4,4)=="-") "depuis"
            else if (substr(input$mutate.fun,4,4)=="=") "par" else "",
            switch(substring(input$mutate.fun,5),
              quantile=.5,
              coalesce=0
          ))
  )
  
  output$mutate.arg2 <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) 
      if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,2,2)>1))
        if (substr(input$mutate.fun,3,3)=="c")
          textInput("mutate.chr.arg2",
            if (substr(input$mutate.fun,4,4)==">") "vers" else "",
            if (substring(input$mutate.fun,5)=="iconv") "UTF-8"
          )
        else
          numericInput("mutate.num.arg2",
            if (substr(input$mutate.fun,4,4)=="-") "jusqu'à" else "",
            NULL
          )
  )  
 
  output$mutate.command2 <- renderUI(
    .IGoR$textarea("mutate", "mutate(column=expression)", 3,
      if (length(input$mutate.type)>0)
        .IGoR$command2(
          .IGoR$group_by(input,"mutate"),
          if ((input$mutate.type==1)&&.isNotEmpty(input$mutate.expr))
            glue("mutate({input$mutate.new} = {input$mutate.expr})")
          else
          if ((input$mutate.type==2)&&.isNotEmpty(input$mutate.old)&&.isNotEmpty(input$mutate.fun))
            if ((substring(input$mutate.fun,1,1)=='x')&&(length(input$mutate.chr.arg1)>0))
              if (.isTRUE(input$mutate.pipe))
                   glue("mutate({input$mutate.new} = {input$mutate.old} %>% {substring(input$mutate.fun,5)}(\"{input$mutate.chr.arg1}\",.))")
              else glue("mutate({input$mutate.new} = {substring(input$mutate.fun,5)}(\"{input$mutate.chr.arg1}\",{input$mutate.old}))")
            else
              paste0(
                glue("mutate({input$mutate.new} = "),
                if (.isTRUE(input$mutate.pipe))
                     paste0(input$mutate.old," %>% ",substring(input$mutate.fun,5),'(')
                else paste0(substring(input$mutate.fun,5),'(',input$mutate.old),
                # Contournement d'un pb sur 'first', 'last'; Error in mutate_impl(.data, dots) : bad value
                if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,1,2)=="m0")) '.', 
                if (substr(input$mutate.fun,2,2)>0)
                  paste0(
                    if (.isTRUE(input$mutate.pipe)) '' else ',',
                    if ((substr(input$mutate.fun,3,3)=="c")&&(length(input$mutate.chr.arg1)>0))
                      paste0('"',input$mutate.chr.arg1,'"')
                    else
                    if ((substr(input$mutate.fun,3,3)=="n")&&(length(input$mutate.num.arg1)>0))
                      input$mutate.num.arg1,
                    if (substr(input$mutate.fun,2,2)>1)
                      if ((substr(input$mutate.fun,3,3)=="c")&&(length(input$mutate.chr.arg2)>0))
                        paste0(',"',input$mutate.chr.arg2,'"')
                      else
                      if ((substr(input$mutate.fun,3,3)=="n")&&(length(input$mutate.num.arg2)>0))
                        paste0(',',input$mutate.num.arg2)
                  ),
                if ((substr(input$mutate.fun,1,1)=="n")&&(length(input$mutate.narm)>0))
                  paste0(
                    if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,2,2)==0)) '' else ', ',
                    glue("na.rm={input$mutate.narm}")
                  ),
                if (substr(input$mutate.fun,1,1)=="r")
                  paste0(
                    if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,2,2)==0)) '' else ', ',
                    "na.rm=FALSE"
                  ),
              '))'
              )
            else if ((input$mutate.type==3)&&.isNotEmpty(input$mutate.old1)&&.isNotEmpty(input$mutate.op)&&.isNotEmpty(input$mutate.old2))
              if (substr(input$mutate.op,1,1)==" ")
                   glue("mutate({input$mutate.new} = {substring(input$mutate.op,2)}({input$mutate.old1},{input$mutate.old2}))")
              else glue("mutate({input$mutate.new} = {input$mutate.old1} {input$mutate.op} {input$mutate.old2})"),
            .IGoR$ungroup(input,"mutate")
  ) )   )
  
  # Ce n'est pas tout à fait correct en cas de modification du nom de la colonne dans command2
  observeEvent(input$mutate.command2, 
    .IGoR$try(input,output,"mutate",
      function(x)
        paste(
          if (input$mutate.new %not in% .columns(input$main.data)) ""
          else glue("ATTENTION : La colonne '{input$mutate.new}' était déjà présente et sera remplacée!"),
          glue("NOTE : La colonne '{input$mutate.new}' va être de classe '{class(x[[input$mutate.new]])}'."),
          sep='\n')))
 
}

