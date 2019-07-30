### Exemple multi_match_fun = function(x,y) x[,"a"]==y[,"b"] à intégrer
### function(x.a,x.b,a.a,y.b,y.c) ... + substitue

.IGoR$page$fuzzyjoin$ui <- function()
  div(id = "bloc_fuzzyjoin",
    fluidRow(
      column(width=4, 
        img(src="images/join.png", height = "46px"),
        h3(span("Jointure de deux tables sur critère quelconque", style="color: blue"))
      ),
     column(width=8, 
       p("Les fonctions du package ", strong("fuzzyjoin")," permettent de compléter les fonctions d'appariement de deux tables, ",
         "en offrant la possibilité de conditionner la correspondance entre observations provenant de chacune des tables par autre chose qu'une égalité de clés. ", 
         "La fonction ", code("fuzzy_join"), "permet d'exprimer le fait qu'il y a correspondance entre deux observations de deux façons :",br(),
         "- soit parce que l'application d'une ",strong("fonction à deux variables"), ", une dans chaque table, renvoie 'TRUE',", br(),
         "- soit parce que le calcul d'une ", strong("expression impliquant un nombre de variables quelconque"), " renvoie 'TRUE'", br(),
         "Le résultat est une table contenant l'intégralité des variables des deux tables. En cas de doublon sur les noms, les variables en cause sont suffixées par '.x' et '.y'.", br(),
         em("NOTE : la page se réinitialise dès que la jointure a fonctionné.")
    ) ) ),
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("fuzzyjoin.columns"),
          hr(),
          uiOutput("fuzzyjoin.data"),
          uiOutput("fuzzyjoin.fun")
      )),
      column(width=6,
        box(width='100%',
          radioButtons("fuzzyjoin.type", "Type de jointure :",
            c("Uniquement les lignes ayant un echo dans les deux tables" = "inner",
              "Les lignes de la table en entrée complétées ou non par celles de la seconde table" = "left",
              "Les lignes de la seconde table complétées ou non par celles de la table en entrée" = "right",
              "Les lignes des deux tables qu'elles aient un écho ou non dans leur vis à vis" = "full",
              "Les lignes de la table en entrée n'ayant aucun écho dans la seconde table" = "anti",
              "Les lignes de la table en entrée ayant un écho dans la seconde table" = "semi"))
        ),
       .IGoR$loadBox("fuzzyjoin","fuzzyjoin.out")
    )),
    .IGoR$commandBox("fuzzyjoin")
  )


.IGoR$page$fuzzyjoin$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"fuzzyjoin")
  
  output$fuzzyjoin.data<- renderUI({
    .IGoR$test$list
    tagList(
      selectizeInput("fuzzyjoin.data", label = "Seconde table en entrée", choices = .tables()),
      uiOutput("fuzzyjoin.columns2")
    )
  })
  
  output$fuzzyjoin.columns <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      selectizeInput("fuzzyjoin.columns", label = "Clés de jointure de la table courante", multiple = TRUE,
                     options = list(placeholder = .IGoR$DISCOLS),
                     choices = .columns(input$main.data,c("factor","character","integer", "logical"))
      )   )
  
  output$fuzzyjoin.columns2 <- renderUI(
    if ((length(input$fuzzyjoin.data>0))&&.IGoR$test$join)
      selectizeInput("fuzzyjoin.columns2", label = "Clés de jointure", multiple = TRUE,
                     options = list(placeholder = .IGoR$DISCOLS),
                     choices = .columns(input$fuzzyjoin.data,c("factor","character","integer", "logical"))
      )   )
  
  output$fuzzyjoin.fun <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1))
      textInput("fuzzyjoin.fun","Fonction de comparaison des clés")
    else
    if ((length(input$fuzzyjoin.columns)>0)&&(length(input$fuzzyjoin.columns2)>0))
      textInput("fuzzyjoin.fun",
                paste0("Condition sur ",
                       .collapse(vars("x",input$fuzzyjoin.columns,input$fuzzyjoin.columns2)),
                       " et ",
                       .collapse(vars("y",input$fuzzyjoin.columns2,input$fuzzyjoin.columns)),
                       "."))
  )
  
  vars <- function(t,l,l2) ifelse(l %in% l2,paste0(l,".",t),l)
  
  expr <- function(expr,lx,ly) {
    f <- function(t,v) substitute(t[,v],list(t=t,v=v))
    g <- function(t,l,l2) {
      m <- if (length(l)==1) list(t)
           else Map(function (x) f(t,x),l)
      names(m) <- vars(as.character(t),l,l2)
      m
    }
    e <- tryCatch(parse(text=expr),error=identity)
    if (is(e,"condition")) "*** ERREUR ***"
    else {
      l <- append(g(quote(x),lx,ly), g(quote(y),ly,lx))
      deparse(
        eval(
          substitute(
            substitute(e, l),
            list(e=e[[1]],l=l)
      )))
  } }
  
  output$fuzzyjoin.command2 <- renderUI(
    .IGoR$textarea("fuzzyjoin", "fuzzy_join(table,mode,columns,function)", 4,
      if ((length(input$fuzzyjoin.columns)>0)
        &&(length(input$fuzzyjoin.columns2)>0)
        &&(length(input$fuzzyjoin.type)>0)
        &&.isNotEmpty(input$fuzzyjoin.fun)) {
        if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)) {
          by <- glue("by=c(\"{input$fuzzyjoin.columns}\"=\"{input$fuzzyjoin.columns2}\"),")
          fun <- glue("match_fun={input$fuzzyjoin.fun})") 
        } else {
          by <- glue("multi_by=list(x={.collapse2(input$fuzzyjoin.columns)},y={.collapse2(input$fuzzyjoin.columns2)}),")
          fun <- glue("multi_match_fun=function (x,y) {expr(input$fuzzyjoin.fun,input$fuzzyjoin.columns,input$fuzzyjoin.columns2)})")
        }
        .IGoR$command2(
          glue("fuzzy_join({input$fuzzyjoin.data}, mode=\"{input$fuzzyjoin.type}\","),
            '\n     ', by,
            '\n     ', fun, .IGoR$look(input$fuzzyjoin.fun)
        )
      }
  ) )
  
  observeEvent(input$fuzzyjoin.command2,.IGoR$try(input,output,"fuzzyjoin"))

}
  
  
