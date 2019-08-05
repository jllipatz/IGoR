### 28/06/2019 1.01.2: Correction : une variable de nom k provoquait une erreur

.IGoR$page$mutate2$ui <- function()
  div(id = "bloc_mutate2",
    fluidRow(
      column(width=4, 
        img(src="images/mutate2.png", height = "46px"),
        h3(span("Modifier un ensemble de variables", style="color: blue"))
      ),
      column(width=8, 
        p("Les fonctions ", code("mutate_all"), " , ", code("mutate_at"), " , ", code("mutate_if"), " du package ", strong("dplyr"), 
          " permettent de ",span("modifier des variables", style="color:blue"),
          " présentes dans une table, en les sélectionnant soit toutes, soit par leur nom, soit par leur contenu.", br(),
          "En R, il n'est généralement pas possible de modifier un objet existant, aussi tout ce que font ces fonctions c'est créer une nouvelle table ",
          "contenant les données de l'ancienne table où ont été substituées les variables sélectionnées par des variables de même nom mais de contenu et/ou de type différent.",
          "Il n'est pas possible d'assigner un nouveau nom au nouveau contenu et donc de conserver l'ancien contenu.", br(),
          em("NOTE : si le nom de la table résultat est celui de la table courante, la page se réinitialise dès que la modification a fonctionné.")
    ) ) ),
    uiOutput("mutate2.control"),
    .IGoR$commandBox("mutate2")
  )


.IGoR$page$mutate2$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"mutate2")
  
  output$mutate2.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("mutate2", buttons.title=.IGoR$s2("Modifier les variables..."))),
        column(width=6,
          .IGoR$load.ui("mutate2",input$main.data),
          uiOutput("mutate2.how")
        )))

  classes <- function(input,.page)
    get(input$main.data,envir=.GlobalEnv) %>% head(1) %>%
    select_at(.IGoR$select.columns(input,output,"mutate2")) %>%
    Map(class,.)

  output$mutate2.how <- renderUI(
    if (length(input$mutate2.type)>0) {
      v <- unique(classes(input,".mutate2"))
      box(width='100%',
        if (length(v)==0) tags$hr("Aucune variable sélectionnée!")
        else
        if (length(v)>1) tags$hr("ERREUR : La sélection contient des variables qui ont des types différents.")
        else {
          tagList(
            selectizeInput("mutate2.fun","",
              choices=
                         if (v=="character")
                           c("Remplacer les valeurs manquantes..."="p1c=coalesce",
                             "Propager les valeurs non manquantes"="r0 :na.locf",
                             "Mettre en majuscules"="c0 :str_to_upper",
                             "Mettre en minuscules"="c0 :str_to_lower",
                             "Calculer la longueur"="c0 :str_length",
                             "Extraire la chaîne entre les positions..."="c2n-str_sub",
                             "Extraire la chaîne à partir de la position..."="c1n-str_sub",
                             "Chercher une expression régulière..."="c1c:str_detect",
                             "Extraire une expression régulière..."="c1c:str_extract",
                             "Remplacer une expression régulière..."="c2c>str_replace",
                             "Changer d'encodage..."="c2c>iconv")
                         else if (v %in% c("numeric","integer"))
                           c("Remplacer les valeurs manquantes..."="p1n=coalesce",
                             "Propager les valeurs non manquantes"="r0 :na.locf",
                             "Changer le signe"="r0 :funs(-.)")
                         else
                           c("Transformer en caractères"="f0 :as.character",
                             "Propager les valeurs non manquantes"="r0 :na.locf")
            ),
            fluidRow(
              column(width=6,uiOutput("mutate2.arg1")),
              column(width=6,uiOutput("mutate2.arg2"))
          ))}
      )
  })
  
  output$mutate2.arg1 <- renderUI(
    if ((length(input$mutate2.type)>0)&&(length(unique(classes(input,".mutate2")))==1))
       if ((length(input$mutate2.fun)>0)&&(substr(input$mutate2.fun,2,2)>0))
        if (substr(input$mutate2.fun,3,3)=="c")
          textInput("mutate2.chr.arg1", .IGoR$s2(
                    if (substr(input$mutate2.fun,4,4)==">") "de"
                    else if (substr(input$mutate2.fun,4,4)=="=") "par" else ""),
                    switch(substring(input$mutate2.fun,5),
                           iconv="850",
                           sprintf="<%5d>"
                    ))
        else
          numericInput("mutate2.num.arg1", .IGoR$s2(
                   if (substr(input$mutate2.fun,4,4)=="-") "depuis"
                   else if (substr(input$mutate2.fun,4,4)=="=") "par" else ""),
                   switch(substring(input$mutate2.fun,5),
                          quantile=.5,
                          coalesce=0
                   ))
  )
  
  output$mutate2.arg2 <- renderUI({
    if ((length(input$mutate2.type)>0)&&(length(unique(classes(input,".mutate2")))==1)) 
      if ((length(input$mutate2.fun)>0)&&(substr(input$mutate2.fun,2,2)>1))
        if (substr(input$mutate2.fun,3,3)=="c")
          textInput("mutate2.chr.arg2", .IGoR$s2(
                    if (substr(input$mutate2.fun,4,4)==">") "vers" else ""),
                    if (substring(input$mutate2.fun,5)=="iconv") "UTF-8"
          )
        else
          numericInput("mutate2.num.arg2", .IGoR$s2(
                   if (substr(input$mutate2.fun,4,4)=="-") "jusqu'à" else ""),
                   NULL
           )
  })  
  
  output$mutate2.command2 <- renderUI(
    .IGoR$textarea("mutate2", "mutate...(...)", 3,
      if ((length(input$mutate2.type)>0)&&(length(input$mutate2.fun)>0)&&(length(unique(classes(input,".mutate2")))==1)) {
        .IGoR$command2(
          "mutate",
          if (input$mutate2.type==2)
            if (input$mutate2.drop) 
                 glue("_if(Negate(is.{input$mutate2.class}), ")
            else glue("_if(is.{input$mutate2.class}, ")
          else
          if (input$mutate2.type==3)
            if (input$mutate2.drop)
                 "_at(c(), "
            else "_all("
          else glue("_at({.IGoR$select(input,'mutate2',vars=TRUE)}, "),
          substring(input$mutate2.fun,5),
          if (substr(input$mutate2.fun,2,2)>0)
            if (substr(input$mutate2.fun,3,3)=="c")
              paste0(', "',if (.isNotEmpty(input$mutate2.chr.arg1)) input$mutate2.chr.arg1 else "",'"')
            else
            if (substr(input$mutate2.fun,3,3)=="n")
              paste0(", ",if (.isNotNA(input$mutate2.num.arg1)) input$mutate2.num.arg1 else "NA"),
          if (substr(input$mutate2.fun,2,2)>1)
            if (substr(input$mutate2.fun,3,3)=="c")
              paste0(', "',if (.isNotEmpty(input$mutate2.chr.arg2)) input$mutate2.chr.arg2 else "",'"')
            else
            if (substr(input$mutate2.fun,3,3)=="n")
              paste0(", ",if (.isNotNA(input$mutate2.num.arg2)) input$mutate2.num.arg2 else "NA"),
          ")"
        )}
  )   )   
  
  observeEvent(input$mutate2.command2, 
    .IGoR$try(input,output,"mutate2",
      function (x) {
        v <- classes(input,".mutate2")
        if (length(unique(v))==1)
          if (length(v)==1)
               sprintf("NOTE : Une variable de type '%s' va être modifiée.",v[[1]])
          else sprintf("NOTE : %d variables de type '%s' vont être modifiées.",length(v),v[[1]])
      }
  ))
                         
}