### 21/07/2019 1.03.0

.IGoR$page$browse$ui <- function()
  div(id = "bloc_browse",
    fluidRow(
      column(width=4, 
        img(src="images/view.png", height = "46px"),
        h3(span("Visualisation d'une observation", style="color: blue"))
      ),
      column(width=8, 
        p("Cette page permet de ",span("visualiser une table observation par observation", style='color: blue'), 
          " de la même façon que le ", em("fsbrowse")," de SAS (mais en mieux).",
          "Elle reprend les facilités de sélection de variables et d'observations présentes dans les pages d'extraction de tables.",
          "L'affichage peut être partagé entre variables de type caractère sélectionnées sur leur nom (par exemple pour identifier les observations) ",
          "et variables sélectionnées sur leurs caractéristiques."
    ) ) ),
    fluidRow(
      column(width=6, uiOutput("browse.ids")),
      column(width=6, uiOutput("browse.where"))
    ),
    # Initial idea was to nest two boxes (columns and lines selection) into another one
    # but here is an issue where nested shinydashboard boxes are collapsed when some outside reactive value updates,
    # and the nested box will not update accordingly.
    # ==> Github Rstudio/shinydashboard issue #234 
    box(width='100%', collapsible=TRUE, collapsed=TRUE,
      column(width=6, .IGoR$select.ui("browse", box=FALSE,
                                      buttons.title="Afficher les variables...",selected=3)),
      column(width=6,
        tagList( 
          fluidRow(
            column(width=4, radioButtons("browse.row.type","Afficher l'observation...", 
                                         c("de numéro :"=1,"de nom :"=3,"de premier identifiant :"=2))),
            column(width=8, uiOutput("browse.row"))
          ),
          hr(),
          fluidRow(
            column(width=6, sliderInput("browse.n","Nombre de colonnes",1,4,3)),
            column(width=6,
              checkboxInput("browse.label","Utiliser les libellés de variable",FALSE),
              checkboxInput("browse.sort","Trier les variables par leur nom",FALSE)
    ) ) ) ) ),
    fluidRow( 
      column(width=12, htmlOutput("browse.html"))
  ) )

.IGoR$page$browse$sv  <- function(input, output, session) {
  
  build_table <- function(.data,.ids,.row){

    f <- function(.data,.row,.n,.header=FALSE,.class=TRUE) {
    
      type <- function(.i)
        switch(class(.data[[.i]]),
               "logical"="<lgl>",
               "numeric"="<dbl>",
               "integer"="<int>",
             "character"="<chr>",
                "factor"="<fct>",
                  "Date"="<date>")
      
      name <- function(.i) 
        if (.isTRUE(input$browse.label)) {
          a <- attr(.data[[.i]],'label')
          if (is.null(a)) colnames(.data)[.i] else a
        }  
      else colnames(.data)[.i]
      
      if ((ncol(.data)>0)&&(nrow(.data)>0))
        map(1:((ncol(.data)-1)/.n+1),
          function(i)
            do.call(if (.header) tags$th else tags$tr,
              map(1:.n,
                function(j)
                  if (ncol(.data)<(i-1)*.n+j) tagList(tags$td(" "))
                  else tagList(tags$td(tags$span(style="color:blue",name((i-1)*.n+j))),
                               if (.class&&!.isTRUE(input$browse.label))
                                 tags$td(tags$span(style="color:blue",type((i-1)*.n+j))),
                               tags$td(toString(.data[.row,(i-1)*.n+j])))
        )   ) )
    }
      
    # if (length(.row)>0)
    #   do.call(tags$table,
    #           append(
    #             list(style = "border: 1px solid black; padding: 1%; width: 100%; rules: groups;",
    #                  list(tags$thead(f(.ids,.row,1,TRUE,FALSE),style="background: rgb(192,192,192)"))),
    #             f(.data,.row,3)
    #   )         )
    if (length(.row)>0)
      tagList(
        do.call(tags$table,
                list(style = "border: 1px solid black; padding: 1%; width: 100%; background: rgb(224,224,255)",
                     f(.ids,.row,1,TRUE,FALSE))),
        br(),
        do.call(tags$table,
                list(style = "border: 1px solid black; padding: 1%; width: 100%",
                     f(.data,.row,if (length(input$browse.n)==0) 3 else input$browse.n)))
      )         
  }
  
  df <- reactive({
    df <- get(input$main.data,envir=.GlobalEnv)
    l <- names(df)
    lh <- intersect(input$browse.ids,l)  # Il peut y avoir défaut de synchronisation
    df <- df %>% mutate(row.names(.))
    if (.isNotEmpty(input$browse.where)) {
      r <- tryCatch(eval(parse(text=glue("df %>% filter({input$browse.where})"))),
                    error=identity)
      if (is(r,"condition")){
        output$browse.comment <- renderText(r$message)
        df <- df %>% filter(FALSE)
      }
      else {
        output$browse.comment <- renderText(.IGoR$look(input$browse.where))
        df <- r
      } }
    lc <- .IGoR$select.columns(input,output,"browse")
    lc <- setdiff(lc,lh)
    if (.isTRUE(input$browse.sort)) lc <- sort(lc)
    list(data=df[,lc,drop=FALSE],ids=df[,c("row.names(.)",lh),drop=FALSE])
  })
  
  no <- reactive(
    if (nrow(df()$data)>0) {
      i <- 
        if ((input$browse.row.type==1)&&(length(input$browse.row)>0)) input$browse.row
        else 
        if ((input$browse.row.type==2)&&(length(input$browse.ids)>0)&&(length(input$browse.value1)>0))
          which(df()$ids[input$browse.ids[1]]==input$browse.value1)
        else
        if ((input$browse.row.type==3)&&(length(input$browse.row.name)>0))
          which(df()$ids["row.names(.)"]==input$browse.row.name)
        else 1
      if (length(i)>1) {
        output$browse.comment <- renderText(sprintf("Il y a %d observations correspondant à cet identifiant.",length(i)))
        i[1]
      } else {
        output$browse.comment <- renderText("")
        i
      } }
  )
  
  .IGoR$select.what(input,output,"browse")
  
  output$browse.where <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        textInput(width='100%',"browse.where","Restreindre aux observations vérifiant la condition"),
        verbatimTextOutput("browse.comment")
  )   )
  
  output$browse.row <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta) {
      df <- df()
      tagList(
        strong(glue("parmi {.p('observation',nrow(df$data))}.")),
        if (input$browse.row.type==1) 
          sliderInput("browse.row","",1,nrow(df$data),1,step=1,round=TRUE)
        else 
        if ((input$browse.row.type==2)&&(length(input$browse.ids)>0))
          textInput("browse.value1",input$browse.ids[1],iconv(df$ids[1,input$browse.ids[1]],from="UTF-8"))
        else
        if (input$browse.row.type==3)
          textInput("browse.row.name","row.names(.)",df$ids[1,"row.names(.)"])
      )
    }
  )
  
  output$browse.ids <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta) 
      selectizeInput("browse.ids","Variables d'identification",
                   multiple=TRUE,  options = list(placeholder = '<colonnes de type caractère>'),
                   choices=.columns(input$main.data,"character"))
  )

  output$browse.html <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      build_table(df()$data,df()$ids,no())
  )
  
}
             