### 28/06/2019 1.01.2: Protection contre les noms de table incorrects

.IGoR$page$create$ui <- function()
  div(id = "bloc_create",
    fluidRow(
      column(width=4, 
        img(src="images/create.png", height = "48px"),
        h3(span("Créer une table ex nihilo", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("read.table"), " est la fonction standard permettant de lire des fichiers texte avec séparateurs ou ",
          "dont les champs sont séparés par un nombre quelconque d'espaces.", 
          "Elle est utilisée ici dans sa capacité à lire à l'intérieur d'une chaîne de caractères comme s'il s'agissait d'un fichier.", br(),
          "La fonction devine le type des colonnes à créer en fonction des données qui lui sont fournies.", br(),
          "Le résultat deviendra la table courante, sauf si une table de même nom éxistait déjà."
    ) ) ),
    fluidRow(
      column(width=6,
        box(width='100%',
          textInput("create.columns",.IGoR$s1("Nom des colonnes (séparés par des espaces)")),
          checkboxInput("create.factors",.IGoR$s4("Convertir les chaînes de caractères en facteurs"),FALSE),
          textInput("create.na.strings",.IGoR$s2("Marqueur pour les valeurs manquantes de type caractère"),"NA")
      )),
      column(width=6,.IGoR$load.ui("create"))
    ),
    .IGoR$commandBox("create")
  )


.IGoR$page$create$sv <- function(input, output, session) {
  
  .IGoR$rLogo(input,output,"create")
  
  na.strings <- function () if (input$create.na.strings=="NA") "" else glue(", na.strings=\"{input$create.na.strings}\"")
  
  command1 <- "read.table(header=TRUE, text=\"{input$create.columns}"
  
  output$create.command1 <- renderText(
    .IGoR$create.command1 <<- paste0(make.names(input$create.out),' <- ',glue(command1)))
  
  output$create.command2 <- renderUI(
    .IGoR$textarea("create", "lignes de données (séparées par des espaces)", 5, ''))
  
  output$create.command3 <- renderText(
    .IGoR$create.command3 <<- glue("\", stringsAsFactors={input$create.factors}{na.strings()})"))
  
  observeEvent({input$create.command2;input$create.out;input$create.columns;input$create.factors},
    output$create.comment <- renderText({
      t <- make.names(input$create.out)
      b <- "create.load"
      s <- paste0(glue(command1),'\n',input$create.command2,glue(.IGoR$create.command3))
      if (nchar(s)>0) {
        x <- tryCatch(eval(parse(text=s),envir=.GlobalEnv),
                      error=function(e) e)
        if (is.data.frame(x)) { 
          output$create.load <- renderUI(actionButton(b,.IGoR$buttonName(input,"create")))
          shinyjs::enable(b)
          shinyjs::show(b)
          sprintf("NOTE : La table '%s' va avoir %d ligne(s) et %d colonne(s).",t,nrow(x),ncol(x))
        }
        else {
          shinyjs::hide(b)
          x$message
      } }
      else {
        shinyjs::hide(b)
        ""
  }}))
  
  observeEvent(input$create.load, 
   .IGoR$do(input,output,"create",
      paste0(.IGoR$create.command1,'\n',input$create.command2,.IGoR$create.command3)
  ))
 
}
          
  