
### 12/07/2019 1.02.0: Protection contre les noms de colonne incorrects

.IGoR$page$rename$ui <- function()
  div(id = "bloc_rename",
    fluidRow(
      column(width=4, 
        img(src="images/rename.png", height = "48px"),
        h3(span("Renommer un ensemble de variables", style="color: blue"))
      ),
      column(width=8, 
        p("Les fonctions de la famille ", code("rename"), " du package ", strong("dplyr"), "permettent de ", span("renommer des variables", style="color:blue"),
          " présentes dans une table, en les sélectionnant soit toutes, soit par leur nom, soit par leur contenu.", br(),
          "Le nouveau nom peut soit être précisé explicitement, soit être déterminé par l'application d'une fonction.", br(),
          "En R, il n'est généralement pas possible de modifier un objet existant, aussi tout ce que fait la fonction c'est créer une nouvelle table ",
          "contenant les données de l'ancienne table associées aux nouveaux noms.", br(),
          em("NOTE : si le nom de la table résultat est celui de la table courante, la page se réinitialise dès que la modification a fonctionné.")
    ) ) ),
    uiOutput("rename.control"),
    .IGoR$commandBox("rename")
  )


.IGoR$page$rename$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"rename")
  
  output$rename.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("rename", "Renommer les variables...")),
        column(width=6,
          .IGoR$loadBox("rename",input$main.data),
          box(width='100%',
            radioButtons("rename.how","",
                         c("Nouveaux noms"=1,
                           "Application d'une fonction"=2)),
            uiOutput("rename.how")
          )
    )))
 
    output$rename.how <- renderUI(
      if (length(input$rename.how)>0)
        if (input$rename.how==1)
          textInput("rename.new","Nouveaux noms (séparés par des espaces)")
        else
          tagList(
            selectizeInput("rename.fun","Fonction",
                           choices=c("Standardiser"="   make.names",
                             "Mettre en minuscules"="   str_to_lower",
                             "Mettre en majuscules"="   str_to_upper",                
                  "Extraire l'expression régulière"="c  str_extract",
                 "Remplacer l'expression régulière"="cc str_replace")),
            uiOutput("rename.args")
    ))
    
    output$rename.args <- renderUI(
      if (.isEQ(input$rename.how,2)&&(length(input$rename.fun)>0))
        fluidRow(
          if (str_sub(input$rename.fun,1,1)=='c')
            column(width=6, textInput("rename.arg1","l'expression")),
          if (str_sub(input$rename.fun,2,2)=='c')
            column(width=6, textInput("rename.arg2","par l'expression"))
    ))
    
    output$rename.command2 <- renderUI(
      .IGoR$textarea("rename", "rename...(...)", 4,
        if ((length(input$rename.type)>0)&&(length(input$rename.how)>0)
          &&(((input$rename.how==1)&&.isNotEmpty(input$rename.new))
           ||((input$rename.how==2)&&.isNotEmpty(input$rename.fun))
           ))
          .IGoR$command2( 
            "rename",
            if (input$rename.how==1) {
              old <- .IGoR$select.columns(input,output,"rename")
              new <- str_split(str_trim(input$rename.new)," +")[[1]] %>% make.names()
              if (length(old)==length(new))
                   glue("({.collapse(paste0(new,' = \"',old,'\"'))})")
              else glue("() # *** ERREUR: Il y a {.p('variable',length(old))} à renommer!")
            } else
              paste0(
                if (input$rename.type==2)
                   if (input$rename.drop)
                        glue("_if(Negate(is.{input$rename.class}), ")
                   else glue("_if(is.{input$rename.class}, ")
                else
                if (input$rename.type==3) 
                   if (input$rename.drop)
                        "_at(c(), "
                   else "_all("
                else glue("_at({.IGoR$select(input,'rename',vars=TRUE)}, "),
                str_sub(input$rename.fun,4),
                if ((str_sub(input$rename.fun,1,1)!=' ')&&(length(input$rename.arg1)>0)) glue(", {shQuote(input$rename.arg1)}"),
                if ((str_sub(input$rename.fun,2,2)!=' ')&&(length(input$rename.arg2)>0)) glue(", {shQuote(input$rename.arg2)}"),
                ")"
              )
  ) )   )
    
  observeEvent(input$rename.command2, 
    .IGoR$try(input,output,"rename",
              .fn=function(x) isolate({
                y <- data.frame(old=names(get(input$main.data,envir=.GlobalEnv)),
                                new=names(x), 
                                stringsAsFactors=FALSE)
                n <- y %>% filter(old!=new) %>% count() %>% pull(n)
                m <- ncol(x) - length(unique(names(x)))
                paste0(
                  if (m>0) sprintf("ATTENTION: Les noms des nouvelles colonnes ne vont pas être uniques!\n"),
                  glue("NOTE : {.p('variable',n)} à renommer.")
                 )
              }
  ) )       )

}