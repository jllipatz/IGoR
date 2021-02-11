###
### Fonctions partagées
###

### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 10/07/2019 1.02.0: Conversion en UTF-8 pour le champ command2
### 18/07/2019 1.02.2: Paramétrage de la hauteur des graphiques
### 25/07/2019 1.03.0: Desactivation du correcteur orthographique avec .IGoR$textarea
### 01/08/2019 1.03.2: Introduction des styles pour les titres des champs
### 06/08/2019 1.04.0: dropdown buttons
### 13/08/2019 1.04.2: Externalisation des libellés en français
### 13/02/2020 1.06.2: Réécriture de l'appel à ShinyFileSave pour corriger un bug de synchronisation
### 03/07/2020 1.08.0: Rajout d'un type enum pour la page 'line'
### 03/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
### 18/12/2020 1.11.0: .columns: Protection contre les classes multiples
### 11/02/2021 1.11.3: Ajout de .nameg pour les fonctions graphiques

.version <- paste0(version$major,".",version$minor)

.IGoR$MAXROWNAMES = 100              # Maximum rows number for a row.names menu ('browse','view')
.IGoR$Z.FILE      = "IGoR.json"      # File containing translatable titles and texts
.IGoR$COLORS      = c("black","red","green","blue","white","yellow","pink")

.IGoR$Z <- jsonlite::fromJSON(.IGoR$Z.FILE)

## First item for column selection, when multiple=FALSE
.IGoR$NONE        = '' %>% {names(.)<- .IGoR$Z$any$none        ; .}
.IGoR$TABLE       = '' %>% {names(.)<- .IGoR$Z$any$table       ; .}
.IGoR$COLV        = '' %>% {names(.)<- .IGoR$Z$any$col         ; .}
.IGoR$CHRCOLV     = '' %>% {names(.)<- .IGoR$Z$any$col.chr     ; .}
.IGoR$QALCOLV     = '' %>% {names(.)<- .IGoR$Z$any$col.discrete; .}
.IGoR$NUMCOLV     = '' %>% {names(.)<- .IGoR$Z$any$col.numeric ; .}
.IGoR$ENUMCOLV    = '' %>% {names(.)<- .IGoR$Z$any$col.enum    ; .}

.column   <- function(input) c(.IGoR$COLV,.columns(input$main.data))
.discrete <- function(input,none=.IGoR$QALCOLV) c(none,.columns(input$main.data,"discrete"))
.numeric  <- function(input,none=.IGoR$NUMCOLV) c(none,.columns(input$main.data,"numeric"))
.enum     <- function(input,none=.IGoR$ENUMCOLV) c(none,.columns(input$main.data,c("factor","character")))

.IGoR$Znames <- function(page,item,items) {
  names(items)<- Vectorize(function(x) .IGoR$Z[[page]][[paste0(item,'.',x)]])(items)
  items
}

.IGoR$Zrename <- function(items) {
  names(items)<- Vectorize(function (x) .IGoR$Z$any$fun.name[[x]])(names(items))
  items
}

.IGoR$hr <- function() hr(style='border:0; margin:0; width:100%; height:1px; background:blue;')

.IGoR$s1 <- function(s) strong(span(s, style='color:red'))  # mandatory field without default setting
.IGoR$s2 <- function(s) strong(span(s, style='color:blue')) # mandatory field with default setting
.IGoR$s3 <- function(s) em(s)                               # control field with default setting
.IGoR$s4 <- function(s) em(span(s, style='color:blue'))     # optional field without default setting
.IGoR$s5 <- function(s) span(s, style='color:blue')         # optional field with default setting

##
## The user interface builder
##
.IGoR$ui <- function(..., page, icon=page,
                     command=TRUE,
                     graphics=FALSE, subtitle=TRUE,
                     control=graphics, save=graphics|(page=="tabular"))
  div(id=paste0("div_",page),
    fluidRow(
      column(width=4,
        img(src=paste0("images/",icon,".png"), height = "48px"),
        h3(span(.IGoR$Z[[page]]$page.title, style="color: blue"))
      ),
      column(width=8,
        HTML(paste0("<p align='justify'>",paste(.IGoR$Z[[page]]$info,collapse=" ")))
    ) ),
    hr(),
    if (control) uiOutput(paste0(page,".control")),
    ...,
    if (command) .IGoR$commandBox(page),
    # Button is designed to be hidden, but when hidden, activating it activates the file dialog twice
#    if (save) extendShinyjs(text = paste0("shinyjs.",page,"SaveButton=function(){ $('#",page,"SaveButton').click(); }")),
    if (save) shinySaveButton(paste0(page,"SaveButton"), label=.IGoR$Z$any$browse,
                       title=if (graphics) .IGoR$Z$all$graphics.save.as else .IGoR$Z$tabular$save.as,
                       filetype=if (graphics) list(png="png") else (html="html")),
     if (graphics)
      fluidRow(
        column(width=1,
          .IGoR$dropdownButton(page=paste0(page,"_titles"), width=NULL, title=.IGoR$Z$all$titles,
            textInput(paste0(page,".title"),.IGoR$s4(.IGoR$Z$any$title),""),
            if (subtitle) textInput(paste0(page,".subtitle"),.IGoR$s4(.IGoR$Z$all$subtitle),""),
            textInput(paste0(page,".source"),.IGoR$s4(.IGoR$Z$all$source),""),
            .IGoR$hr(),
            sliderInput(paste0(page,".height"),.IGoR$s2(.IGoR$Z$all$height),1,16,4)
          ),
          uiOutput(paste0(page,".dropdown"))
        ),
        column(width=11, uiOutput(paste0(page,"..plot")))
  )   )

.IGoR$dropdownButton <- function(..., page, width="800px", title=.IGoR$Z$all$graphics.options, status="info")
  tagList(
    tags$style(HTML(paste0("#dropdown-menu-",page," {border: 1px solid blue ;background-color: #F0F0FF ;}"))),
    dropdownButton(inputId=page, width=width, tooltip=tooltipOptions(title=title), status=status,
                   circle=FALSE,
                   em(title),
                   .IGoR$hr(),
                   ...),
    br()
  )

.IGoR$commandBox <- function(page)
  tagList(
    box(width=12, collapsible=TRUE,
      column(width=2,
        imageOutput(paste0(page,".R"), height='128px'),
        actionButton(paste0(page,".copy"),.IGoR$Z$all$copy)
      ),
      column(width=10,
        verbatimTextOutput(paste0(page,".command1")), # immutable command header
        uiOutput(paste0(page,".command2")),           # modifiable command text
        verbatimTextOutput(paste0(page,".command3"))  # only for 'create'
    ) ),
    verbatimTextOutput(paste0(page,".comment")),      # messages from execution of the command or of its test
    verbatimTextOutput(paste0(page,".preview"))       # display of the command result
  )

## Used in the 'command2' fields.
.IGoR$textarea <- function (page,placeholder,rows,text)
  tagList(
    tags$style(type="text/css",
               "textarea {font-family: 'Courier New'; width: 100%; background: rgb(245,245,245); border-color: rgb(204,204,204); }"),
    tags$textarea(id=paste0(page,".command2"), spellcheck='false',
                  placeholder=placeholder, rows=rows,
                  text)
  )

## Widget to create the output table
.IGoR$load.ui <- function(page, out=paste0(page,".out"))
  box(width='100%', NULL,
    column(width=8, textInput(paste0(page,".out"), .IGoR$s2(.IGoR$Z$any$out), out)),
    column(width=4, uiOutput(paste0(page,".load")))
  )

## textInput widget with title before cell instead of above
.IGoR$label.ui <- function(page,var,value,
                           title=.IGoR$s2(.IGoR$Z$any$titleh), suffix=".label") {
  id <- paste0(page,".",var,suffix)
  div_id <- str_replace_all(id,'\\.','_')
  tagList(
    tags$head(
      tags$style(type="text/css",
        paste0("#",div_id," label{ display: table-cell; text-align: center; vertical-align: middle; }
                #",div_id," .form-group { display: table-row;}"))),
    tags$div(id=div_id, textInput(id,title,value))
  )
}

._     <- function(x,page,field)       x[[paste0(page,".",as.character(as.list(match.call())$field))]]
`._<-` <- function(x,page,field,value) x[[paste0(page,".",as.character(as.list(match.call())$field))]]<- value

## textInput widget with default text column label if some exists or column name if not
.IGoR$gLabel.ui <- function(input,page,var) {
  v <- input[[paste0(page,".",var)]]
  d <- get(input$main.data,envir=.GlobalEnv)
  l <- attr(d[[v]],'label')
  textInput(paste0(page,".",var,".label"),.IGoR$Z$any$title,if (is.null(l)) v else l)
}

## (graphics) Builds the additional arg to set a title from a column name or label or user text
.IGoR$gLabel.arg <- function(input,page,var,name)
  if (.isNE(input[[paste0(page,'.',var,'.label')]],input[[paste0(page,'.',var)]]))
    paste0('\n     ',name,'=',shQuote(input[[paste0(page,'.',var,'.label')]]))

## selectizeInput for group columns
.IGoR$group.ui <- function (input,page, box=TRUE){
  f <- function()
    selectizeInput(paste0(page,".group"), label=.IGoR$s3(.IGoR$Z$any$group),
                   multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                   choices = .columns(input$main.data,"discrete"))

  if (box) box(width='100%', f()) else f()
}

## Columns selection box
## Used by 'browse', 'factor', 'gather', 'skim', 'mutate2', 'rename', 'select', "summarise'
.IGoR$select.ui <- function(page, title=NULL, box=TRUE,
                            buttons.title=NULL, selected=1,
                            buttons.all=TRUE, buttons.class=TRUE, buttons.range=FALSE,
                            drop=TRUE) {
  f <- function ()
    tagList(
      fluidRow(
        column(width=6,
          radioButtons(paste0(page,".type"),buttons.title,
                       .IGoR$Znames("all","select",c(1,if (buttons.class) 2,if (buttons.all) 3,4:7,if (buttons.range) 0)),
                       selected=selected)),
        column(width=6,
          if (drop)
               checkboxInput(paste0(page,".drop"),.IGoR$s4(.IGoR$Z$any$drop),FALSE)
          else uiOutput(paste0(page,".drop")),
          uiOutput(paste0(page,".columns.what")),     # Auxiliary input field for type 1,2, 4:7 selections
          uiOutput(paste0(page,".columns.more"))      # Auxiliary input field ('select')
      ) ),
      verbatimTextOutput(paste0(page,".columns.why")) # Messages from type 7 selection
    )

  if (box) box(width='100%', title=title, f()) else f()
}

## Builds the auxiliary input 'drop' checkbox for column selection box
.IGoR$select.drop <- function(input,output,page)
  ._(output,page,drop) <- renderUI(
    if (length(._(input,page,type))>0)
      if (((._(input,page,type)==1)&&(length(._(input,page,columns))>0))   # selection by list (allows no selection)
        ||(._(input,page,type)==2)
        ||((._(input,page,type)>=4)&&.isNotEmpty(._(input,page,pattern)))  # selection by name
      )
        checkboxInput(paste0(page,".drop"),.IGoR$s4(.IGoR$Z$any$drop),FALSE)
  )

## Builds the auxiliary input field for column selection box
.IGoR$select.what <- function(input,output,page,
                              columns.all=FALSE)
  ._(output,page,columns.what) <- renderUI(
    if (length(._(input,page,type))>0)
      if (._(input,page,type)==0)
        fluidRow(
          column(width=6, numericInput(paste0(page,".start"),.IGoR$s2(.IGoR$Z$all$select.range.start),1)),
          column(width=6, numericInput(paste0(page,".end"),  .IGoR$s2(.IGoR$Z$all$select.range.end),  ncol(get(input$main.data,envir=.GlobalEnv))))
        )
      else
      if (._(input,page,type)==1) {
        .IGoR$test$meta
        selectizeInput(paste0(page,".columns"),"",
                       multiple = TRUE,  options = list(placeholder = if (columns.all) .IGoR$Z$any$all else .IGoR$Z$any$cols),
                       choices = .columns(input$main.data))

      }
      else
      if (._(input,page,type)==2)
        selectizeInput(paste0(page,".class"),"",
                       choices=.IGoR$Znames("all","select.type",c("character","numeric","double","integer","logical","factor","Date")))
      else
      if (._(input,page,type)>=4)
        textInput(paste0(page,".pattern"),"")
  )

## ('filter', 'mutate') Widget to select expression writing
.IGoR$expr.type.ui <- function (page,title=NULL)
  radioButtons(paste0(page,".type"),title,.IGoR$Znames("all","expr",0:3))

## (graphics, 'tabular') Widget to allow saving results
.IGoR$save.ui <- function (page,.title=.IGoR$Z$all$graphics.save)
  box(width='100%',
      column(width=6, checkboxInput(paste0(page,".save"), .title, FALSE)),
      column(width=6, uiOutput(paste0(page,".save")))
  )


.tables <- function () {
  f <- Vectorize(function(x) is.data.frame(get(x,envir=.GlobalEnv)))
  g <- Vectorize(function(x) {
    y <- attr(get(x,.GlobalEnv),"created")
    if (is.null(y)) 0 else y
  })

  l <- ls(envir=.GlobalEnv)
  l <- l[f(l)]
  t <- data.frame(name=l,time=g(l),stringsAsFactors=FALSE)
  t[order(t$time),"name"]
}

.columns <- function (.table,.class,.sort=TRUE) {
  .table <- get(.table,envir=.GlobalEnv)
  if (!missing(.class)&&(length(.class)==1))
         if (.class=="numeric")  .class <- c("numeric","integer","Date")
    else if (.class=="discrete") .class <- c("factor","character","integer","logical","Date")
    else if (.class=="double")   .class <- "numeric"
  c <- names(if (missing(.class)) .table
             else select_if(.table,function (x) length(intersect(class(x),.class))>0)
       )
  if (.sort) c <- sort(c)
  names(c) <- c
  c
}

.name <- function(x) ifelse(x==make.names(x),x,paste0('`',x,'`'))
.nameg <- function(x) ifelse(x==make.names(x),x,paste0('.$`',x,'`'))

.collapse0 <- function(x) paste(x,collapse=', ')
.collapse  <- function(x) paste(.name(x),collapse=', ')
.collapse1 <- function(x) paste(ifelse(is.na(x),"NA",paste0('"',x,'"')),collapse=', ')
.collapse2 <- function(x)
  if (length(x)==0) ""    else if (length(x)==1) paste0('"',x,'"') else paste0("c(",.collapse1(x),")")
.collapse3 <- function(x)
  if (length(x)==0) "c()" else if (length(x)==1) paste0('"',x,'"') else paste0("c(",.collapse1(x),")")

.isTRUE     <- function (x) (length(x)>0)&&x
.isFALSE    <- function (x) (length(x)>0)&&!x
.isNotNA    <- function (x) (length(x)>0)&&!is.na(x)
.isNotEmpty <- function (s) (length(s)>0)&&(nchar(s)>0)
.isFile     <- function (x) (length(x)>0)&&str_detect(x,"\\.")
.isEQ     <- function (x,y) (length(x)>0)&&(!is.na(x))&&(x==y)
.isNE     <- function (x,y) (length(x)>0)&&(!is.na(x))&&(x!=y)
.isIn     <- function (x,y) (length(x)>0)&&(x %in% y)
.inOrNULL <- function (x,y) (length(x)==0)||(x %in% y)

## En 3.5, as_tibble efface la classe skim_df quand elle est présente
as_tibble <- function(.data) if ("tbl_df" %in% class(.data)) .data else dplyr::as_tibble(.data)

.IGoR$newTable <- function(input,output,.table,.select=FALSE) {
  a <- if (.table %in% .IGoR$tables) "updated"
  else {
    output$main.data <-  renderUI(
      selectizeInput("main.data", label = .IGoR$Z$all$main.data,
                     selected=if (.select) .table else input$main.data,
                     choices = eval(quote(.IGoR$tables <- .tables()),envir=.GlobalEnv)
      ))
    "created"
  }
  eval(parse(text=glue("attr({.table},'{a}')<- Sys.time()")),envir=.GlobalEnv)
}

# Appelé par les fonctions d'import de table
.IGoR$do <- function(input,output,.page,.command)
  isolate({
    t <- make.names(input[[paste0(.page,".out")]])
    x <- tryCatch(eval(parse(text=.command),envir=.GlobalEnv),
                  error=identity)
    if (is(x,"condition")) {
      output[[paste0(.page,".preview")]] <- renderText("")
      output[[paste0(.page,".comment")]] <- renderText(x$message)
    }
    else {
      if (.page=="import") eval(parse(text=glue("attr({make.names(input$import.out)},'source')<- '{input$import.file}'")),envir=.GlobalEnv)
      eval(bquote(.IGoR$log <- add_row(.IGoR$log,time=Sys.time(),page=.(.page),command=.(.command))) ,envir=.GlobalEnv)
      shinyjs::disable(paste0(.page,".load"))
      if (t==input$main.data)
        eval(quote(.IGoR$test$meta <- .IGoR$test$meta+1), envir=.GlobalEnv)
      eval(quote(.IGoR$test$list <- .IGoR$test$list+1), envir=.GlobalEnv)
      .IGoR$newTable(input,output,t,.select=TRUE)
      d <- get(t,envir=.GlobalEnv)
      output[[paste0(.page,".preview")]] <- renderPrint(d %>% as_tibble() %>% print())
      output[[paste0(.page,".comment")]] <- renderText(sprintf(.IGoR$Z$all$msg.result, t, nrow(d), ncol(d)))
    }
})

# Appelé par les fonctions qui ne créent pas de table
.IGoR$do1 <- function(input,output,.page,.command,.fn=NULL) {
   x <- tryCatch(eval(parse(text=.command),envir=.GlobalEnv),
            error=identity)
   if (is(x,"condition")) {
     output[[paste0(.page,".comment")]] <- renderText(x$message)
     NULL
   } else {
      eval(bquote(.IGoR$log <- add_row(.IGoR$log,time=Sys.time(),page=.(.page),command=.(.command))) ,envir=.GlobalEnv)
      output[[paste0(.page,".comment")]] <- renderText(if (is.null(.fn)) "" else .fn(x))
      x
   }
}

.IGoR$do2 <- function (input,output,.page,.source=TRUE,.signal=TRUE) {
  t <- make.names(input[[paste0(.page,".out")]])
  a <- input[[paste0(.page,".command2")]]
  c <- glue("{t} <- {input$main.data} %>% {a}")
  d <- eval(parse(text=c),envir=.GlobalEnv)
  eval(bquote(.IGoR$log <- add_row(.IGoR$log,time=Sys.time(),page=.(.page),command=.(c))) ,envir=.GlobalEnv)
  eval(quote(.IGoR$test$list <- .IGoR$test$list+1), envir=.GlobalEnv)
  if (!.source) eval(parse(text=glue("attr({t},'source')<- NULL")),envir=.GlobalEnv)
  if ((t==input$main.data)&&.signal)
    eval(quote(.IGoR$test$meta <- .IGoR$test$meta+1), envir=.GlobalEnv)
  else .IGoR$newTable(input,output,t)
  output[[paste0(.page,".preview")]] <- renderPrint(d %>% as_tibble() %>% print())
  output[[paste0(.page,".comment")]] <- renderText(
    sprintf(.IGoR$Z$all$msg.result, t, nrow(d), ncol(d))
  )
  shinyjs::disable(paste0(.page,".load"))
}

.IGoR$look <- function(text) {
  v <- tryCatch(getParseData(parse(text=text,keep.source=TRUE)) %>%
                  .[.$token %in% c("OR2","AND2","EQ_ASSIGN"),"text"],
                error=identity)
  if ((length(v)>0)&&!is(v,"condition")) sprintf(.IGoR$Z$all$msg.warning,.collapse(v)) else ""
}

.IGoR$try <- function (input,output,.page,.fn=NULL,.subset="head(1)") {
  output[[paste0(.page,".preview")]] <- renderText("")
  output[[paste0(.page,".comment")]] <- renderText({
    b <- paste0(.page,".load")
    s <- input[[paste0(.page,".command2")]]
    if ((nchar(s)>0)&&(s!='   ')) {
      x <- tryCatch(
             eval(parse(text=glue("{input$main.data} %>% {.subset} %>%\n{s}")),envir=.GlobalEnv),
             error=function(e) e)
      if (is.data.frame(x)) {
        output[[b]] <- renderUI(actionButton(b,.IGoR$buttonName(input,.page)))
        shinyjs::enable(b)
        shinyjs::show(b)
        if (!is.null(.fn)) .fn(x) else ""
      }
      else {
        shinyjs::hide(b)
        x$message
    } }
    else {
      shinyjs::hide(b)
      ""
  }})
}

.IGoR$buttonName <- function(input,.page)
  if (make.names(input[[paste0(.page,".out")]]) %not in% .IGoR$tables) "Créer la table" else "Remplacer"

.IGoR$renderTables <- function(input,output)
  output$main.data <-   renderUI({
    eval(quote(.IGoR$tables <- .tables()),envir=.GlobalEnv)
    selectizeInput("main.data", label = .IGoR$Z$all$main.data,
                   selected=if (length(input$main.data)>0) input$main.data,
                   choices = .IGoR$tables
    )
  })

.IGoR$metaChanged <- function(input,.page)
  if (input$main.data==make.names(input[[paste0(.page,".out")]]))
    eval(quote(.IGoR$test$meta <- .IGoR$test$meta+1), envir=.GlobalEnv)

.IGoR$test<- reactiveValues(meta=1,list=1,join=1)
.IGoR$log <- data.frame(time=Sys.time(),page="",command="#Yes master!",stringsAsFactors = FALSE)
.IGoR$save <- !rstudioapi::isAvailable()

NL <- ' %>%\n   '

.IGoR$rLogo <- function(input,output,.page) {

  output[[paste0(.page,".R")]] <- renderImage(list(src="images/R_logo.png"),deleteFile = FALSE)

  observeEvent(input[[paste0(.page,".copy")]], {
    text <- paste(glue(.IGoR[[paste0(.page,".command1")]]),
                  input[[paste0(.page,".command2")]],
                  glue(.IGoR[[paste0(.page,".command3")]]),
                  sep='\n')
    tmp  <- getParseData(parse(text=text, keep.source = TRUE))
    funs <- unique(tmp$text[tmp$token=="SYMBOL_FUNCTION_CALL"]) # Ne ramasse pas les opérateurs tels que %>%
    pkgs <- Map(find,funs)
    pkgs <- Map(function(x) x[1], pkgs)                         # Pour les fonctions présentes dans plusieurs packages
    pkgs <- pkgs %>% str_replace("^package:","") %>% sort() %>% unique()
    text <- paste0('# Utilise : ',paste(pkgs[pkgs!="base"],collapse=", "),'.\n',text)
    writeClipboard(text)
  })
}

## Fonctions pour les pages créant une nouvelle table
.IGoR$aServer <- function(input,output,.page,.signal=TRUE) {

  .IGoR$rLogo(input,output,.page)

  output[[paste0(.page,".command1")]] <- renderText(
    if (length(input$main.data)>0)
      eval(bquote(.IGoR[[.(paste0(.page,".command1"))]] <-
                  .(glue("{make.names(input[[paste0(.page,'.out')]])} <- {input$main.data} %>%"))),
                  envir=.GlobalEnv)
  )

  observeEvent(input[[paste0(.page,".load")]], isolate(.IGoR$do2(input,output,.page,.signal=.signal)))

}

.IGoR$aaServer <- function(input,output,.page,.signal=TRUE) {

  .IGoR$aServer(input,output,.page,.signal)

  observeEvent(input[[paste0(.page,".command2")]], .IGoR$try(input,output,.page))

}

## Fonctions pour les pages ne créant pas de nouvelle table
.IGoR$bServer <- function(input,output,.page) {

  .IGoR$rLogo(input,output,.page)

  output[[paste0(.page,".command1")]] <- renderText(
    if (length(input$main.data)>0)
      eval(bquote(.IGoR[[.(paste0(.page,".command1"))]] <-
                    .(glue("{input$main.data} %>%"))),
           envir=.GlobalEnv)
  )
}




## >>> command2 helpers (return strings)
##
## dplyr grouping
## Used by 'distinct', filter', 'mutate', 'summarise'
.IGoR$group_by <- function(input,page)
  if ((length(._(input,page,group))>0)&&(nchar(._(input,page,group))>0))
    paste0(glue("group_by({.collapse(._(input,page,group))})"),NL)

.IGoR$ungroup  <- function(input,page,n=0)
  if ((length(._(input,page,group))>n)&&(nchar(._(input,page,group)[1])>0))
    paste0(NL,"ungroup()")

## dplyr variables selection
## Used by 'factor', 'gather', 'mutate2', 'rename', 'skim', 'summarise'
.IGoR$select <- function(input,page,
                         vars=FALSE) {
  if (._(input,page,type)==1)
    if (vars)
      .IGoR$columns(input,page)
    else
      if ((length(._(input,page,columns))==0)||(length(._(input,page,drop))==0)) ""
      else if (._(input,page,drop)) 
             .collapse0(paste0('-',.name(._(input,page,columns))))
        else .collapse(._(input,page,columns))
  else
  if ((._(input,page,type)>3)&&(length(._(input,page,pattern))>0)) {
    f <- if (._(input,page,type)==4) "starts_with"
    else if (._(input,page,type)==5) "ends_with"
    else if (._(input,page,type)==6) "contains"
    else                             "matches"
    if (.isTRUE(._(input,page,drop))) f <- paste0('-',f)
    if (vars)
         glue("vars({f}({shQuote(._(input,page,pattern))}))")
    else glue("{f}({shQuote(._(input,page,pattern))})")
  } }


## <<< command2 helpers


## *** Gets the list of selected columns, from the columns menu only
.IGoR$columns <- function(input,page)
  .collapse3(
    if (._(input,page,drop)) setdiff(.columns(input$main.data),._(input,page,columns)) else ._(input,page,columns)
  )

## *** Gets the list of selected columns, from the columns selection box
## Checks for a valid regexpr
.IGoR$select.columns <- function(input,output,page) {
  why <- ""
  l0 <- .columns(input$main.data,.sort=FALSE)
  l1 <-
    if (length(._(input,page,type))==0) l0
    else
    if (._(input,page,type)==1) ._(input,page,columns)
    else
    if ((._(input,page,type)==2)&&(length(._(input,page,class))>0))
      .columns(input$main.data,._(input,page,class),.sort=FALSE)
    else
    if (._(input,page,type)==3) l0
    else
    if ((._(input,page,type)>3)&&(length(._(input,page,pattern))>0)) {
      p <- ._(input,page,pattern)
      f <- if (._(input,page,type)==4) startsWith
      else if (._(input,page,type)==5) endsWith
      else if (nchar(p)==0)            function (x,y) TRUE
      else if (._(input,page,type)==6) function(x,y) grepl(y,x,fixed=TRUE)
      else                             function(x,y) grepl(y,x)
      tryCatch(l0[f(l0,p)], error=function(e) {why <<- e$message; character(0)})
    }
  ._(output,page,columns.why) <- renderText(why)
  if (.isTRUE(._(input,page,drop))) setdiff(l0, l1) else l1
}

.IGoR$vServer <- function(input,output,page) {

  .IGoR$aServer(input,output,page)

  .IGoR$select.what(input,output,page)

}

## Functions for 'join' and 'fuzzy join'
.IGoR$jServer <- function (input,output,page) {

  .IGoR$aServer(input,output,page)

  output[[paste0(page,".data")]]<- renderUI({
    .IGoR$test$list
    fluidRow(
      column(width=6, selectizeInput(paste0(page,".data"), .IGoR$s1(.IGoR$Z$all$join.data), choices=c(.IGoR$TABLE,.tables()))),
      column(width=6, uiOutput(paste0(page,".columns2")))
    )
  })

  output[[paste0(page,".columns2")]]<- renderUI(
    if (.isNotEmpty(._(input,page,data))&&.IGoR$test$join)
      selectizeInput(paste0(page,".columns2"), .IGoR$s1(.IGoR$Z$all$join.keys),
                     multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                     choices = .columns(._(input,page,data),"discrete")
  )    )

  output[[paste0(page,".columns")]]<- renderUI(
   if ((length(input$main.data)>0)&&.IGoR$test$meta)
     fluidRow(
       column(width=6, .IGoR$s2(.IGoR$Z$all$main.data)),
       column(width=6, selectizeInput(paste0(page,".columns"), .IGoR$s1(.IGoR$Z$all$join.keys),
                                      multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols.discrete),
                                      choices = .columns(input$main.data,"discrete"))
 )   ) )


}

## Fonctions pour les pages graphiques

.IGoR$gVarLabelUI <- function(input,output,page,var)
  output[[paste0(page,".",var,".label")]] <- renderUI({
    v <- input[[paste0(page,".",var)]]
    if (.isNotEmpty(v))
      textInput(paste0(page,".",var,".label"),.IGoR$s2("Titre"),{
        d <- get(input$main.data,envir=.GlobalEnv)
        l <- attr(d[[v]],'label')
        if (is.null(l)) v else l
      })
  })

.IGoR$command2 <- function (...) iconv(paste0('   ',...), from="UTF-8")

.IGoR$gTitleCmd <- function(input,page,labels=NULL,X=FALSE,Y=FALSE) {
  l <-  c(if (.isNotEmpty(._(input,page,title)))    glue("title={shQuote(._(input,page,title))}"),
          if (.isNotEmpty(._(input,page,subtitle))) glue("subtitle={shQuote(._(input,page,subtitle))}"),
	        if (.isNotEmpty(._(input,page,source)))   glue("caption={shQuote(paste0('Source : ',._(input,page,source)))}"),
          if (X&&.isNE(._(input,page,X.label),.nameg(._(input,page,X)))) glue("x={shQuote(._(input,page,X.label))}"),
          if (Y&&.isNE(._(input,page,Y.label),.nameg(._(input,page,Y)))) glue("y={shQuote(._(input,page,Y.label))}"),
          labels)
  if (length(l)>0) paste0(NL,glue("gf_labs({paste(l,collapse=', ')})"))
 }

.IGoR$gSaveCmd <- function(input,page)
  if (.isTRUE(input[[paste0(page,".save")]])) {
    f <- parseSavePath(.IGoR$volumes,input[[paste0(page,"SaveButton")]])$datapath
    if (.isNotEmpty(f)) paste0(NL,"{",glue("ggsave(\"{f}\",device='png')","; .}"))
  }

## Gestion des éléments standard des pages graphiques
.IGoR$gServer <- function(input,output,page) {

  .IGoR$bServer(input,output,page)

  output[[paste0(page,"..plot")]] <- renderUI({
    h <- input[[paste0(page,".height")]]*100
    if (is.na(h)||(h<100)) h <- 400
    plotOutput(paste0(page,".plot"),height=sprintf("%dpx",h))
  })

  observeEvent(input[[paste0(page,".command2")]],
               output[[paste0(page,".plot")]] <- renderPlot(
                 if (.isNotEmpty(input[[paste0(page,".command2")]]))
                   .IGoR$do1(input,output,page,paste(input$main.data,'%>%',input[[paste0(page,".command2")]]))
               ) )

  .IGoR$saveButton(input,output,page)

}


## Save file server is permanent, so the button must be permanent too.
## To simulate a conditional button, the button is hidden and a click is simulated when necessary
## Used in graphics pages and in 'tabular'.
.IGoR$saveButton <- function(input,output,page){

  shinyFileSave(input,paste0(page,"SaveButton"), roots=.IGoR$volumes, defaultRoot='home')

  # observe(
  #   if (.isTRUE(input[[paste0(page,".save")]]))
  #     eval(parse(text=paste0("js$",page,"SaveButton()")))
  # )

}

## Widgets within dropdownButtons are not refreshed until they are shown,
## so the input files may conserve a value entered before the data table has changed and may lead to inconsistence.
## To bypass, we remember the name of the table and the columns change state when creating the widgets.
##  see the 'browse' page for examples of use.
.IGoR$sync <- list()
.IGoR$do.sync <- function (input,id) {
  .IGoR$sync[[id]] <<- list(input$main.data, .IGoR$test$meta)
  id                                   # meant to be used as widget id
}
.IGoR$is.sync <- function (input,id)
  identical(.IGoR$sync[[id]], list(input$main.data, .IGoR$test$meta))
.IGoR$if.sync <- function (input,id) {
  x <- input[[id]]                     # in case of not refreshed, will cause to refresh
  if (.IGoR$is.sync(input,id)) x       # if not refreshed returns NULL else its value
}


