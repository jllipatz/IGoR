###
### Exemple de construction dynamique d'une table
###

### 04/07/2019 1.01.3: Ajout d'une pagination
### 16/07/2019 1.02.1: Correction - sélection d'une seule ligne invisible
### 17/07/2019 1.02.1: Ajout d'une visualisation par groupe et améliorations diverses

.IGoR$page$view$ui <- function()
  div(id = "bloc_view", 
    fluidRow(
      column(width=4,
        img(src="images/view.png", height = "46px"),
        h3(span("Données de la table courante", style="color: blue"))
      ),
      column(width=8,
        p("Les colonnes ont des noms, les lignes aussi (",em("rownames"),"), par défaut le numéro d'observation.",
          "Les modifications apportées à une table conservent rarement les noms des lignes, mais ceux ci sont également rarement utilisés.", br(), 
          "La fonction ", code("group_by"), "du package ", strong("dplyr"), " permet de considérer une table comme comme un empilement de sous-tables, ",
          "une pour chaque modalité du croisement des variables citées, et ceci sans qu'aucun réordonnancement de la table ne soit nécessaire. ",
          "L'applicaton d'une autre fonction de ", strong("dplyr"), " se comportera alors comme une application de la fonction à chacune des sous-tables, ",
          "puis en la concaténation des différents résultats.", br(),
          "Cette fonctionnalité est reprise ici à titre purement démonstratif."
    ) ) ),
    fluidRow(
      column(width=6,
        fluidRow(
          column(width=6, uiOutput("view.rownames")),
          column(width=6, uiOutput("view.columns"))
        ),
        uiOutput("view.where")
      ),
      column(width=6, uiOutput("view.control"))
    ),
    htmlOutput("view.html.table")
  )

.IGoR$page$view$sv <- function(input, output, session) {

  build_table <- function(.data){
    cols <- input$view.columns
    rows <- input$view.page.size
 
    l1 <- do.call(tags$tr,
                  map(c(sprintf("%dx%d",nrow(.data),ncol(.data)),cols),tags$th)
          )
    top <- min((input$view.page.no-1)*rows+1,nrow(.data))
    end <- min(input$view.page.no*rows,nrow(.data))
    l2 <- if (end>=top)
      map(seq(top,end),
              function (i) {
                l <- if (length(cols)>0) as.list(.data[i,cols,drop=FALSE])
                names(l) <- NULL
                do.call(tags$tr,
                        append(
                          list(tags$td(tags$span(style="color:blue",rownames(.data)[i]))),
                          map(l,tags$td)
                )       )
              }
          )
    
    do.call(tags$table,
            append(
               list(style = "border: 1px solid black; padding: 1%; width: 100%;",
                    l1),
               l2
    )       )
  }  
  
  df <- reactive({
    df <- get(input$main.data,envir=.GlobalEnv)
    df1 <- df %>%
      mutate(..row.names=row.names(.)) %>%
      group_by_at(input$view.group)          # perd les row.names sans possibilité de les remettre
    if ((length(input$view.rownames)==0)&&.isNotEmpty(input$view.where)) {
      df2 <- tryCatch(eval(parse(text=glue("df1 %>% filter({input$view.where})"))),
                      error=identity)
      if (is(df2,"condition"))
        output$view.comment <- renderText(df2$message)
      else {
        output$view.comment <- renderText(.IGoR$look(input$view.where))
        df1 <- df2
        df <- as.data.frame(df1)
        row.names(df) <- df$..row.names
        df$..row.names <- NULL
      }
    }
    else
      output$view.comment <- renderText('')
    if ((length(input$view.rownames)==0)&&(length(input$view.group)>0)) {
      list(data=df,groups=length(attr(df1,"group_sizes")),
           groups_size=attr(df1,"group_sizes"),
           groups_rows=Map(function(x) x+1, attr(df1,"indices")))
    }
    else 
      list(data=df,groups=0)
  })
  
  output$view.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
    box(width='100%',
      fluidRow(
        column(width=6, uiOutput("view.group")),
        column(width=6, uiOutput("view.group.no"))
      ),
      fluidRow(
        column(width=6, numericInput("view.page.size","Lignes par page",10)),
        column(width=6, uiOutput("view.page.no"))
  ) ) )
  
  output$view.group <- renderUI(
    if ((length(input$view.rownames)==0)&&.IGoR$test$meta)
      selectizeInput("view.group", label=.IGoR$GROUPS,
                     multiple = TRUE, options = list(placeholder = .IGoR$DISCOLS),
                     choices = .columns(input$main.data,c("factor","character","integer","logical")))
  )
    
  output$view.group.no <- renderUI(
    if ((length(input$view.rownames)==0)&&(length(input$view.group)>0))
      sliderInput("view.group.no","Groupe",1,df()$groups,1, step=1, round=TRUE)
  )

  output$view.page.no <- renderUI(
    if ((length(input$main.data)>0)&&.isNotNA(input$view.page.size)) {
      df <- df()
      n <- if (length(input$view.rownames)>0) length(input$view.rownames)
      else if((df$groups==0)||(length(input$view.group.no)==0)) nrow(df$data)
            else df$groups_size[input$view.group.no]
      m <- trunc((n-1)/input$view.page.size)+1
      sliderInput("view.page.no","Page",1,m,1,step=1, round=TRUE, sep="")
    }
  )
  
  output$view.rownames <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      if (nrow(get(input$main.data,envir=.GlobalEnv))<100)
        selectizeInput("view.rownames","Observations :",
          multiple = TRUE, options = list(placeholder = sprintf("rownames (%d)",nrow(get(input$main.data,envir=.GlobalEnv)))),
          choices = rownames(get(input$main.data,envir=.GlobalEnv))
      )
      else h4(sprintf("%d observations",nrow(get(input$main.data,envir=.GlobalEnv))))
  )
  
  output$view.columns <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      selectizeInput("view.columns", "Variables :",
        multiple = TRUE, options = list(placeholder = sprintf("colonnes (%d)",ncol(get(input$main.data,envir=.GlobalEnv)))),
        choices = .columns(input$main.data)
    )
  )
  
  output$view.where <- renderUI(
    if ((length(input$main.data)>0)&&(length(input$view.rownames)==0))
      box(width='100%',
        fluidRow(column(width=12, textInput("view.where","Restreindre aux observations vérifiant la condition"))),
        fluidRow(column(width=12, verbatimTextOutput("view.comment")))
  )   )
  
  output$view.html.table <- renderUI(
      if ((length(input$main.data)>0)&&.isNotNA(input$view.page.size)&&.isNotNA(input$view.page.no)
        &&(length(setdiff(input$view.columns,names(get(input$main.data,envir=.GlobalEnv))))==0)) # Il peut y avoir défaut de synchronisation
    {
    
    input$main.data
    input$view.rownames
    input$view.columns
    input$view.group
    input$view.group.no
    input$view.where
    input$view.page.size
    input$view.page.no
    
    isolate({
      df <- df()
      if (length(input$view.rownames)>0) 
        build_table(subset(df$data,rownames(df$data) %in% input$view.rownames))
      else 
      if ((df$groups>0)&&(length(input$view.group.no)>0))
        build_table(df$data[df$groups_rows[[input$view.group.no]],])
      else 
        build_table(df$data)
     })
  })
}

  