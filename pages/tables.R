
### 14/06/2019 1.01.3: Ajout d'une fonction de déchargement du log
### 05/07/2019 1.01.3: Ajout de l'option de sauvegarde automatique en mode batch
### 09/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$tables$ui <- function()
  .IGoR$ui(page="tables", command=FALSE,
    uiOutput("tables.output"),
    fluidRow(
      column(width=6, actionButton("tables.delete",.IGoR$Z$tables$delete)),
      column(width=6, checkboxInput("tables.save", .IGoR$s5(.IGoR$Z$tables$save), .IGoR$save))
    ),
    box(width='100%', collapsible=TRUE, collapsed=TRUE,
      column(width=6, actionButton("tables.log", .IGoR$Z$tables$log)),
      column(width=6, textInput("tables.log.out",.IGoR$s2(.IGoR$Z$tables$log.out),"log"))
  ))

.IGoR$page$tables$sv <- function(input, output, session) {

  observeEvent(input$tables.save, eval(bquote(.IGoR$save <- .(input$tables.save)), envir=.GlobalEnv))

  output$tables.output <- renderUI({
    .IGoR$test$list
    do.call(tags$table,
      list(style = "border: 1px solid black; padding: 10px; width: 100%",
        imap(.tables(),
           function (x,i) {
             t <- get(x,envir=.GlobalEnv)
             tags$tr(
               tags$td(checkboxInput(paste0("tables.",i),x,FALSE)),
               tags$td(sprintf(.IGoR$Z$tables$ncols,ncol(t)), align='right'),
               tags$td(sprintf(.IGoR$Z$tables$nrows,nrow(t)), align='right'),
               tags$td(attr(t,'source'))
            )}
    ) ) )
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
      t <- make.names(input$tables.log.out)
      eval(bquote(.(t) <- .IGoR$log), envir=.GlobalEnv)
      eval(quote(.IGoR$test$list<- .IGoR$test$list+1), envir=.GlobalEnv)
      .IGoR$newTable(input,output,t)
    }))
               
  }
  