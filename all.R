###
### Fonctions partagée 
###

### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 10/07/2019 1.02.0: Conversion en UTF-8 pour le champ command2
### 18/07/2019 1.02.2: Paramétrage de la hauteur des graphiques
### 25/07/2019 1.03.0: Desactivation du correcteur orthographique avec .IGoR$textarea

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
             else select_if(.table,function (x) class(x) %in% .class))
  if (.sort) c <- sort(c)
  names(c) <- c
  c
} 

.collapse  <- function(x) paste(x,collapse=', ')
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

.p <- function(s,n,f=TRUE)
  if (n==0) glue("aucun{if (f) 'e' else ''} {s}") else
  if (n==1) glue("un{if (f) 'e' else ''} {s}")    else
            glue("{n} {s}s")

## En 3.5, as_tibble efface la classe skim_df quand elle est présente
as_tibble <- function(.data) if ("tbl_df" %in% class(.data)) .data else dplyr::as_tibble(.data)

.IGoR$newTable <- function(input,output,.table,.select=FALSE) {
  a <- if (.table %in% .IGoR$tables) "updated"
  else {
    output$main.data <-  renderUI(
      selectizeInput("main.data", label = .IGoR$IN,
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
      output[[paste0(.page,".comment")]] <- renderText(sprintf("NOTE : La table '%s' comporte %d ligne(s) et %d colonne(s).", t, nrow(d), ncol(d)))
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
    sprintf("NOTE : La table '%s' comporte %d ligne(s) et %d colonne(s).", t, nrow(d), ncol(d))
  )
  shinyjs::disable(paste0(.page,".load"))
}

.IGoR$look <- function(text) {
  v <- tryCatch(getParseData(parse(text=text,keep.source=TRUE)) %>%
                  .[.$token %in% c("OR2","AND2","EQ_ASSIGN"),"text"],
                error=identity)
  if ((length(v)>0)&&!is(v,"condition")) paste0(" # *** Maître! Etes vous sûr de vouloir employer ",.collapse(v)," ?") else ""
}

.IGoR$textarea <- function (page,placeholder,rows,text)
  tagList(
    tags$style(type="text/css",
               "textarea {font-family: 'Courier New'; width: 100%; background: rgb(245,245,245); border-color: rgb(204,204,204); }"),
    tags$textarea(id=paste0(page,".command2"), spellcheck='false',
                  placeholder=placeholder, rows=rows, 
                  text)    
  )

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
    selectizeInput("main.data", label = .IGoR$IN,
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

.IGoR$IN       = "Table courante"
.IGoR$OUT      = "Mémoriser le résultat sous le nom :"
.IGoR$OUTNCOL  = "NOTE : Le résultat aura %d colonne(s)."
.IGoR$ALLV     = "<toutes"
.IGoR$NEWCOL   = "Colonne à créer ou remplacer"
.IGoR$INVAR    = "Variable en entrée"
.IGoR$OLDVAR   = "Variable à convertir"
.IGoR$NUMERIC  = "Variable quantitative"
.IGoR$NUMVAR1  = "Variable quantitative (*)"
.IGoR$NUMVARY1 = "Variable quantitative en ordonnée (*)"
.IGoR$QALVAR   = "Variable qualitative"
.IGoR$QALVAR1  = "Variable qualitative (*)"
.IGoR$QALVARX1 = "Variable qualitative en abscisse (*)"
.IGoR$QALVARS  = "Variables qualitatives"
.IGoR$VARS     = "Variables"
.IGoR$ALL      = "<toutes>"
.IGoR$COLS     = "<colonnes>"
.IGoR$COLV     = c("<colonne>"='')
.IGoR$QALCOLV  = c("<colonne de type 'factor' ou 'character'>"='')
.IGoR$FACTOR   = "Variable qualitative (énumération)"
.IGoR$FCTCOLS  = "<colonnes de type 'factor'>"
.IGoR$CHRCOLV  = c("<colonne de type caractère>"='')
.IGoR$NUMCOLV  = c("<colonne de type numérique>"='')
.IGoR$NUMCOLS  = "<colonnes numériques>"
.IGoR$GROUP    = "Grouper par modalité de la variable"
.IGoR$GROUPS   = "Grouper par modalité de :"
.IGoR$WEIGHT   = "Pondérer avec :"
.IGoR$DISCOLS  = "<colonnes (hors type 'double')>"
.IGoR$DISCOLV  = c("<colonne (hors type 'double')>"='')
.IGoR$NARM     = "Ignorer les valeurs manquantes" 
.IGoR$BROWSE   = "Parcourir..."
.IGoR$GSAVE0   = "Sauvegarder le graphique"
.IGoR$GSAVE    = "Sauvegarder le graphique sous :"
.IGoR$TSAVE0   = "Sauvegarder le tableau"
.IGoR$COLORS = c("black","red","green","blue","white","yellow","pink")
.IGoR$STATS  = '<fonctions>'
.IGoR$STATSV = c("Somme"="sum",
                 "Moyenne"="mean",
                 "Mediane"="median",
                 "Premier quartile"="q1",
                 "Dernier quartile"="q3",
                 "Premier décile"="p10",
                 "Dernier décile"="p90",
                 "Ecart type"="sd",
                 "Variance"="var",
                 "Minimum"="min",
                 "Maximum"="max",
                 "Premiere valeur"="first",
                 "Derniere valeur"="last")

NL <- ' %>%\n   '

.IGoR$commandBox <- function(page)
  tagList(
   box(width=12, collapsible=TRUE,
     column(width=2, 
       imageOutput(paste0(page,".R"), height='128px'),
       actionButton(paste0(page,".copy"),"Copier le code")
     ),
     column(width=10,
       verbatimTextOutput(paste0(page,".command1")),
       uiOutput(paste0(page,".command2")),
       verbatimTextOutput(paste0(page,".command3"))
     )),
   verbatimTextOutput(paste0(page,".comment")),
   verbatimTextOutput(paste0(page,".preview"))
 )

.IGoR$loadBox <- function(page, out=paste0(page,".out"))
  box(width='100%', title=.IGoR$OUT,
    column(width=8, textInput(paste0(page,".out"),NULL,out)),
    column(width=4, uiOutput(paste0(page,".load")))
)

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
._     <- function(x,page,field)       x[[paste0(page,".",as.character(as.list(match.call())$field))]]
`._<-` <- function(x,page,field,value) x[[paste0(page,".",as.character(as.list(match.call())$field))]]<- value

## *** Columns selection box
## Used by 'browse', 'factor', 'gather', 'skim', 'mutate2', 'rename', 'select', "summarise'
.IGoR$select.ui <- function(page,title=NULL,box=TRUE,
                            buttons.title=NULL,selected=1,
                            buttons.all=TRUE, buttons.class=TRUE,
                            drop=TRUE) {
  f <- function ()
    tagList(
      fluidRow(
        column(width=6,
          radioButtons(paste0(page,".type"),buttons.title,
                   c("sélectionnées..."=1,
                     if (buttons.class) c("du type..."=2),
                     if (buttons.all)   c("toutes"=3),
                     "dont le nom débute par..."=4,
                     "dont le nom finit par..."=5,
                     "dont le nom contient..."=6,
                     "dont le nom contient l'expression régulière..."=7),
                   selected=selected)),
        column(width=6,
          if (drop) 
               checkboxInput(paste0(page,".drop"),"Inverser la sélection",FALSE)
          else uiOutput(paste0(page,".drop")),
          uiOutput(paste0(page,".columns.what")),
          uiOutput(paste0(page,".columns.more"))
      ) ),
      verbatimTextOutput(paste0(page,".columns.why"))
    )
  
  if (box) box(width='100%', title=title, f()) else f()
}

## *** Builds the auxiliary input field for column selection
.IGoR$select.what <- function(input,output,page,
                              columns.all=FALSE, buttons.class=TRUE)
  ._(output,page,columns.what) <- renderUI(
    if (length(._(input,page,type))>0)
      if (._(input,page,type)==1) {
        .IGoR$test$meta
        selectizeInput(paste0(page,".columns"),"",
                       multiple = TRUE,  options = list(placeholder = if (columns.all) '<toutes>' else '<colonnes>'),
                       choice = .columns(input$main.data))
        
      }
      else
      if ((._(input,page,type)==2)&&buttons.class)
        selectizeInput(paste0(page,".class"),"",
                       choices=c("numérique"="numeric",
                                 "numérique virgule flottante"="double",
                                 "numérique entier"="integer",
                                 "énumération (facteur)"="factor",
                                 "caractère"="character",
                                 "booléen"="logical"))
      else
      if (._(input,page,type)!=3)
        textInput(paste0(page,".pattern"),"")
  )

## *** Builds the 'auxiliary input'drop' field for column selection
.IGoR$select.drop <- function(input,output,page)
  ._(output,page,drop) <- renderUI(
    if (length(._(input,page,type))>0)
      if (((._(input,page,type)==1)&&(length(._(input,page,columns))>0))
          ||((._(input,page,type)>=4)&&(.isNotEmpty(._(input,page,pattern))))
      ) 
        checkboxInput(paste0(page,".drop"),"Inverser la selection",FALSE)
  )

## >>> command2 helpers (return strings)
##
## dplyr grouping
## Used by 'distinct', filter', 'mutate', 'summarise'
.IGoR$group_by <- function(input,page)
  if (length(._(input,page,group))>0) paste0(glue("group_by({.collapse(._(input,page,group))})"),NL)

.IGoR$ungroup  <- function(input,page,n=0)
  if (length(._(input,page,group))>n) paste0(NL,"ungroup()")

## dplyr variables selection
## Used by 'factor', 'gather', 'mutate2', 'rename', 'skim', 'summarise'
.IGoR$select <- function(input,page,
                         vars=FALSE) {
  if (._(input,page,type)==1)
    if (vars)
      .IGoR$columns(input,page)
    else 
      if ((length(._(input,page,columns))==0)||(length(._(input,page,drop))==0)) ""
      else .collapse(if (._(input,page,drop)) paste0('-',._(input,page,columns)) else ._(input,page,columns))
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

## Widgets for writing assisted expressions
## used by 'filter', 'mutate'
.IGoR$expr.ui <- function(input,page,buttons.title="")
  tagList(
    box(width='100%',
      fluidRow(
        column(width=8, radioButtons(paste0(page,".type"),buttons.title,
          c("Expression non assistée"=1,
            "Mode assisté sur une variable"=2,
            "Mode assisté entre deux variables"=3))
        ),
        column(width=4, uiOutput(paste0(page,".expr.more")))
      ),
      uiOutput(paste0(page,".expr.what"))
    ),
    box(width='100%',
      selectizeInput(paste0(page,".group"), label=.IGoR$GROUPS,
        multiple = TRUE, options = list(placeholder = .IGoR$DISCOLS),
        choices = .columns(input$main.data,c("factor","character","integer","logical"))
  ) ) )

.IGoR$save.ui <- function (page,.title=.IGoR$GSAVE0)
  box(width='100%',
      column(width=6, checkboxInput(paste0(page,".save"), .title, FALSE)),
      column(width=6, uiOutput(paste0(page,".save")))
  )


## Fonctions pour les pages graphiques
.IGoR$gUI <- function(page,.title,.text)
  div(id = paste0("bloc_",page), 
    if (missing(.text)) h3(.title)
    else
      fluidRow(
        column(width=4, 
          img(src=paste0("images/",page,".png"), height = "48px"),
          h3(span(.title, style="color: blue"))
        ),
        column(width=8, .text)
      ),
    box(width='100%', collapsible=TRUE, collapsed=TRUE,
	    fluidRow(
	      column(width=4, textInput(paste0(page,".title"),"Titre ","")),
	      column(width=4, textInput(paste0(page,".subtitle"),"Sous-titre","")),
	      column(width=4, textInput(paste0(page,".source"),"Source",""))
	    ),
	    column(width=4, numericInput(paste0(page,".height"),"Hauteur du graphique (x100 pixels)",4))
	  ),
    uiOutput(paste0(page,".control")),
    .IGoR$commandBox(page),
    uiOutput(paste0(page,"..plot"))
  )

.IGoR$gVarLabelUI <- function(input,output,page,var)
  output[[paste0(page,".",var,".label")]] <- renderUI({
    v <- input[[paste0(page,".",var)]]
    if (.isNotEmpty(v))
      textInput(paste0(page,".",var,".label"),"Titre",{
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
          if (X&&.isNE(._(input,page,X.label),._(input,page,X))) glue("x={shQuote(._(input,page,X.label))}"),
          if (Y&&.isNE(._(input,page,Y.label),._(input,page,Y))) glue("y={shQuote(._(input,page,Y.label))}"),
          labels)
  if (length(l)>0) paste0(NL,glue("gf_labs({paste(l,collapse=', ')})"))
 }
 
.IGoR$gSaveCmd <- function(input,page)
  if (.isTRUE(input[[paste0(page,".save")]])) {
    f <- parseSavePath(.IGoR$volumes,input[[page]])$datapath
    if (.isNotEmpty(f)) paste0(NL,"{",glue("ggsave(\"{f}\",device='png')","; .}"))
  }

## Gestion des élements standard des pages graphiques
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
  
  output[[paste0(page,".save")]] <- renderUI(
    if (.isTRUE(input[[paste0(page,".save")]]))
      shinySaveButton(page, .IGoR$BROWSE, .IGoR$GSAVE, filetype=list(png="png"))
  )
  
  shinyFileSave(input,page, roots=.IGoR$volumes, defaultPath='', defaultRoot='home')
  
}


