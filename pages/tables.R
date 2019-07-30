### 14/06/2019 1.01.3: Ajout d'une fonction de déchargement du log
### 05/07/2019 1.01.3: Ajout de l'option de sauvegarde automatique en mode batch

.IGoR$page$tables$ui <- function()
  div(id = "bloc_tables",
    fluidRow(
      column(width=4, 
        img(src="images/tables.png", height = "48px"),
        h3(span("Liste des tables", style="color: blue"))
      ),
      column(width=8, 
        p("Cette page recense toutes les tables, objets de type ", strong("data.frame"), " ou mieux, présents en mémoire ",
          "et associés à un symbole de l'",strong("environnement global"), " qui leur sert de nom.", br(),
          "Les tables de test ", em("mtcars1"), " et ", em("nanopop"), " sont initialisées au démarrage de la session IGoR. ",
          "Les autres tables existeront jusqu'à ce que la session R soit fermée : si IGoR est lancé depuis RStudio ou RGui, jusqu'à la fermeture de RStudio ou RGui; ",
          "si IGoR est lancé par un ", em(".bat"), "jusqu'à la sortie d'IGoR.",br(),
          "Il est possible de demander la sauvegarde automatique en fin de session, de l'intégralité des tables dans le fichier ", strong("IGoR.RData"),
          " du répertoire accéssible sous le nom 'home' dans les dialogues des pages de lecture et d'écriture de tables (", code("R.home()"),")."
    ) ) ),
    uiOutput("tables.output"),
    fluidRow(
      column(width=6, actionButton("tables.delete","Supprimer les références sélectionnées")),
      column(width=6, checkboxInput("tables.save","Sauvegarder l'intégralité des tables en fin de session", .IGoR$save))
    ),
    box(width='100%', collapsible=TRUE, collapsed=TRUE,
      column(width=6, actionButton("tables.log","Créer une table à partir du log")),
      column(width=6, textInput("tables.log.table","Mémoriser sous le nom :","log"))
  ))

.IGoR$page$tables$sv <- function(input, output, session) {

  observeEvent(input$tables.save, eval(bquote(.IGoR$save <- .(input$tables.save)), envir=.GlobalEnv))

  output$tables.output <- renderUI({
    .IGoR$test$list
    do.call(tagList,
      imap(.tables(),
           function (x,i) {
             t <- get(x,envir=.GlobalEnv)
             fluidRow(
               column(width=3,checkboxInput(paste0("tables.",i),x,FALSE)),
               column(width=1,tags$td(sprintf("%d colonne(s)",ncol(t)))),
               column(width=1,tags$td(sprintf("%d ligne(s)",nrow(t)))),
               column(width=3,tags$td(attr(t,'source')))
            )}
    ))
  })
  
  observeEvent(input$tables.delete,
    isolate({
      t <- .tables()
      l <- map_if(1:length(t),
                  function(i) input[[paste0("tables.",i)]],
                  function(i) {rm(list=t[i],envir=.GlobalEnv); i})
      if (length(l)>0) {
        eval(quote(.IGoR$test$list<- .IGoR$test$list+1), envir=.GlobalEnv)
        .IGoR$renderTables(input,output)
      }
    })
  )
  
  observeEvent(input$tables.log,
    isolate({
      t <- make.names(input$tables.log.table)
      eval(bquote(.(t) <- .IGoR$log), envir=.GlobalEnv)
      eval(quote(.IGoR$test$list<- .IGoR$test$list+1), envir=.GlobalEnv)
      .IGoR$newTable(input,output,t)
    }))
               
  }
  