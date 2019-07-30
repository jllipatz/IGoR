
### 25/07/2019 1.03.0 : Mode assisté (keep quiet: temperature was over 35°C!)

.IGoR$page$filter$ui <- function()
  div(id = "bloc_filter",
    fluidRow(
      column(width=4, 
        img(src="images/filter.png", height = "48px"),
        h3(span("Sélectionner des observations", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("filter"), " du package", strong("dplyr"), "construit une",
          span("nouvelle table limitée aux observations satisfaisant une certaine condition.",style="color:blue"), br(),
          "Le test de la condition peut être fait en groupant les observations sur les mêmes modalités d'un ensemble de variables.",
          "Dans ce cas le test sera réalisé groupe par groupe et la table résultat sera l'agrégation des résultats obtenus sur les différents groupes.",
          "Ceci n'est utile que lorsque la condition utilise une fonction retournant un", em("indicateur statistique"), ": son calcul sera fait groupe par groupe et non sur l'ensemble de la table."
    ) ) ),
    fluidRow(
      column(width=8, uiOutput("filter.control")),
      column(width=4, .IGoR$loadBox("filter","filter.out"))
    ),
    .IGoR$commandBox("filter")  
  )


.IGoR$page$filter$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"filter")
  
  output$filter.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$expr.ui(input,"filter","Restreindre aux observations vérifiant la condition")
  )
  
  output$filter.expr.more <- renderUI(
    checkboxInput("filter.drop","Inverser la condition", FALSE)
  )
  
  output$filter.expr.what <- renderUI(
    if (length(input$filter.type)>0)
      if (input$filter.type==1) textInput("filter.where","Formule restituant TRUE ou FALSE")
      else
        tagList(
          column(width=3,
            selectizeInput("filter.arg1", "Variable", choices=c(.IGoR$COLV,.columns(input$main.data)))
          ),
          column(width=5, uiOutput("filter.fun")),
          column(width=4, uiOutput("filter.arg2"))
  )     ) 

  output$filter.fun <- renderUI(
    if ((length(input$filter.type)>0)&&(input$filter.type>1)
      &&.isNotEmpty(input$filter.arg1)) {
      c <- get(input$main.data,envir=.GlobalEnv)[[input$filter.arg1]]
      selectizeInput("filter.fun", label="Opérateur", choices=
              if (is.logical(c))     c("est vrai"=" ",
                                     "est égal à"="on ==",
                                             "et"="on &",
                                             "ou"="on |",
                         "est à valeur manquante"="f  is.na")
         else if (is.character(c)) c("est égal à"="oc ==",
                                   "appartient à"="oC %in%",
                                   "commence par"="fc startsWith",
                                      "finit par"="fc endsWith",
                "contient l'expression régulière"="fc str_detect",
                         "est à valeur manquante"="f  is.na")
         else if (is.numeric(c))   c("est égal à"="on ==",
                                   "appartient à"="oN %in%",
                                "est supérieur à"="on >",
                        "est supérieur ou égal à"="on >=",
                        if (input$filter.type==2)
                        c("est égal au calcul de"="of ==",
                     "est supérieur au calcul de"="of >"),
                         "est à valeur manquante"="f  is.na")
         else if (is.factor(c))    c("est égal à"="oc ==",
                                   "appartient à"="oC %in%",
                         "est à valeur manquante"="f  is.na")
      )
    }
  )
  
  output$filter.arg2 <- renderUI(
    if ((length(input$filter.type)>0)&&(input$filter.type>1)
      &&.isNotEmpty(input$filter.arg1)
      &&(length(input$filter.fun)>0))
      if (input$filter.fun!=' ') {         # else no function expected
        t <- str_sub(input$filter.fun,2,2) # argument type
        if (t!=' ')                        # else no argument expected
          if (input$filter.type==2)
            if (t=='f')
              selectizeInput("filter.arg2","Indicateur statistique",
                            choices=c("sa moyenne"="mean",
                                      "sa médiane"="median",
                            "son dernier quartile"="quantile,.75",
                                     "son maximum"="max",
                                     "son minimum"="min"))
            else
            if (t=='n')
                 numericInput("filter.arg2","Valeur",0)
            else textInput("filter.arg2",if (t %in% c('C','N')) "Valeurs v1,v2,v3,..." else "Valeur")
          else
            if (input$filter.type==3)
              selectizeInput("filter.arg2", "Variable", choices=c(.IGoR$COLV,.columns(input$main.data)))
    }
  )
                                  
  output$filter.command2<- renderUI(
    .IGoR$textarea("filter", "filter(condition)", 3,
      if (length(input$filter.type)>0)
        .IGoR$command2(
          .IGoR$group_by(input,"filter"),
          "filter(",
          {
            drop <- .isTRUE(input$filter.drop)
            e <-  
              if ((input$filter.type==1)&&.isNotEmpty(input$filter.where))
                list(drop,TRUE,input$filter.where)
              else
              if ((input$filter.type>1)&&(length(input$filter.fun)>0)&&(length(input$filter.arg1)>0)) 
                if (input$filter.fun==' ')                             # - no function (logical column only) ------- 
                  list(drop,FALSE,input$filter.arg1)
                else {                                                 # - a function ------------------------------
                  c <- str_sub(input$filter.fun,1,1)                   # call type
                  t <- str_sub(input$filter.fun,2,2)                   # argument type
                  f <- str_sub(input$filter.fun,4)                     # function name
                  if (t==' ')                                          # -- no argument ----------------------------
                    list(drop,FALSE,glue("{f}({input$filter.arg1})"))  
                  else {                                               # -- one argument ---------------------------
                    arg2 <- if (t=='f') {                              # --- argument is a statistical function ----
                      s <- str_split(input$filter.arg2,',')[[1]]
                      if (length(s)==1)
                           glue("{s}({input$filter.arg1})")
                      else glue("{s[1]}({input$filter.arg1},{s[2]})")
                    } else input$filter.arg2
                    q <- if (input$filter.type==3) c('','')            # --- argument quoting ----------------------
                    else switch (t,
                      c = c('"','"'),
                      f =,
                      n = c('',''),
                      C =,
                      N = c('c(',')'))
                    if (c=='f')                                        # --- function call -------------------------
                      list(drop,FALSE,glue("{f}({input$filter.arg1},{q[1]}{arg2}{q[2]})"))
                    else {                                             # --- binary operator -----------------------
                      g <- if (drop) switch(f, "=="="!=", ">"="<=", ">="="<")
                      if (is.null(g))
                           list(drop,TRUE,glue("{input$filter.arg1} {f} {q[1]}{arg2}{q[2]}"))
                      else list(FALSE,NA, glue("{input$filter.arg1} {g} {q[1]}{arg2}{q[2]}"))
                  } }
              }
            if (length(e)>0)                                           # negate result if required  ----------------
              if (e[[1]])
                if (e[[2]]) glue("!({e[[3]]})") else glue("!{e[[3]]}")
              else e[[3]]
          },
          ')',
          if ((input$filter.type==1)&&.isNotEmpty(input$filter.where)) .IGoR$look(input$filter.where),
          .IGoR$ungroup(input,"filter")
  ) )   )

}

