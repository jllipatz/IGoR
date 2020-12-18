
### 10/08/2019 1.04.2: Externalisation des libellés en français
### 16/12/2020 1.11.0: Traitement spécifique des chaînes de caractères 1-1

### BUG: fuzzyjoin ne supporte pas les noms non normalisés, même hors condition.

### Exemple multi_match_fun = function(x,y) x[,"a"]==y[,"b"] à intégrer
### function(x.a,x.b,a.a,y.b,y.c) ... + substitute

.IGoR$page$fuzzyjoin$ui <- function()
  .IGoR$ui(page="fuzzyjoin",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("fuzzyjoin.columns"),
          uiOutput("fuzzyjoin.data"),
          uiOutput("fuzzyjoin.fun")
      )),
      column(width=6,
        .IGoR$load.ui("fuzzyjoin"),
        box(width='100%',
          radioButtons("fuzzyjoin.type", .IGoR$s2(.IGoR$Z$join$type),.IGoR$Znames("join","type",c("inner","left","right","full","anti","semi")))
        )
  ) ) )


.IGoR$page$fuzzyjoin$sv <- function(input, output, session) {
  
  .IGoR$jServer(input,output,"fuzzyjoin")
  
  is.string <- function(theColumn)
    "character" %in% class(get(input$main.data,envir=.GlobalEnv)[[theColumn]])
  
  strings <- function(l) {
    n <- names(l)
    names(n) <- l
    n
  } 

  output$fuzzyjoin.fun <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1))
      list(
        fluidRow(
          column(width=6, radioButtons("fuzzyjoin.funType",.IGoR$s2(.IGoR$Z$fuzzyjoin$fun),strings(.IGoR$Z$fuzzyjoin$type))),
          column(width=6, uiOutput("fuzzyjoin.stringfun"))
        ),
        uiOutput("fuzzyjoin.dist")
      )
    else
    if ((length(input$fuzzyjoin.columns)>0)&&(length(input$fuzzyjoin.columns2)>0))
      textInput("fuzzyjoin.fun",
                .IGoR$s1(
                  paste0(.IGoR$Z$fuzzyjoin$condition,
                       .collapse(vars("x",input$fuzzyjoin.columns,input$fuzzyjoin.columns2)),
                       .IGoR$Z$fuzzyjoin$and,
                       .collapse(vars("y",input$fuzzyjoin.columns2,input$fuzzyjoin.columns)),
                       "."
  )   )         ) )
  
  output$fuzzyjoin.stringfun <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
       &&is.string(input$fuzzyjoin.columns)&&is.string(input$fuzzyjoin.columns2))
      if (.isEQ(input$fuzzyjoin.funType,"dist"))
        selectizeInput("fuzzyjoin.dist","",choices=strings(.IGoR$Z$fuzzyjoin$method))
      else
      if (.isEQ(input$fuzzyjoin.funType,"menu"))
        selectizeInput("fuzzyjoin.fun","",choices=strings(.IGoR$Z$fuzzyjoin$funs))
      else textInput("fuzzyjoin.fun","")
    else textInput("fuzzyjoin.fun",.IGoR$s1(.IGoR$Z$fuzzyjoin$fun))
  )
  
  output$fuzzyjoin.dist <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
        &&is.string(input$fuzzyjoin.columns)&&is.string(input$fuzzyjoin.columns2)
        &&.isEQ(input$fuzzyjoin.funType,"dist")
        &&(length(input$fuzzyjoin.dist)>0))
      list(
        fluidRow(
          column(width=6, textInput("fuzzyjoin.dist.col",.IGoR$s2(.IGoR$Z$fuzzyjoin$dist.col),"fuzzyjoin.dist")),
          column(width=6, numericInput("fuzzyjoin.dist.max",.IGoR$s2(.IGoR$Z$fuzzyjoin$dist.max), NA))
        ),
        uiOutput("fuzzyjoin.dist.parm")
  )   ) 
  
  output$fuzzyjoin.dist.parm <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)
        &&is.string(input$fuzzyjoin.columns)&&is.string(input$fuzzyjoin.columns2)
        &&.isEQ(input$fuzzyjoin.funType,"dist")
        &&(length(input$fuzzyjoin.dist)>0))
      if (input$fuzzyjoin.dist %in% c("qgram","cosine"))
          column(width=3,numericInput("fuzzyjoin.parm.q",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.q),1))
      else
      if (input$fuzzyjoin.dist=="jw")
        fluidRow(
          column(width=3,numericInput("fuzzyjoin.parm.p",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.p),0)),
          column(width=3,numericInput("fuzzyjoin.parm.bt",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.bt),0))
        )
      else
      if (input$fuzzyjoin.dist %in% c("osa","dl"))
        fluidRow(
          column(width=3,numericInput("fuzzyjoin.parm.w.d",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.w.d),1)),
          column(width=3,numericInput("fuzzyjoin.parm.w.i",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.w.i),1)),
          column(width=3,numericInput("fuzzyjoin.parm.w.s",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.w.s),1)),
          column(width=3,numericInput("fuzzyjoin.parm.w.t",.IGoR$s2(.IGoR$Z$fuzzyjoin$parm.w.t),1))
  )     )
  
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
    if (is(e,"condition")) .IGoR$Z$any$error
    else {
      l <- append(g(quote(x),lx,ly), g(quote(y),ly,lx))
      deparse(
        eval(
          substitute(
            substitute(e, l),
            list(e=e[[1]],l=l)
        ) ),
        width.cutoff=500
      )
  } }
  
  output$fuzzyjoin.command2 <- renderUI(
    .IGoR$textarea("fuzzyjoin", "fuzzy_join(table,mode,columns,function)", 4,
      if (.isNotEmpty(input$fuzzyjoin.data)
        &&(length(input$fuzzyjoin.columns)>0)
        &&(length(input$fuzzyjoin.columns2)>0)
        &&(length(input$fuzzyjoin.type)>0)) {
        key <- "fuzzy"
        parm <- ""
        col <- ""
        max <- ""
        if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1)) {
          by <- glue("by=c(\"{input$fuzzyjoin.columns}\"=\"{input$fuzzyjoin.columns2}\")")
          if (is.string(input$fuzzyjoin.columns)&&is.string(input$fuzzyjoin.columns2))
            if (length(input$fuzzyjoin.funType)==0) key <- "" # Not ready yet
            else # --- 1-1 strings column, distance ---------------------------
            if (input$fuzzyjoin.funType=="dist") 
              if (length(input$fuzzyjoin.dist)==0) key <- "" # Not ready yet
              else {
                key <- "stringdist"
                fun <- glue("method=\"{input$fuzzyjoin.dist}\"")
                parm <- 
                  if (input$fuzzyjoin.dist %in% c("osa","dl"))
                     glue("weight=c(d={input$fuzzyjoin.parm.w.d}, i={input$fuzzyjoin.parm.w.i}, s={input$fuzzyjoin.parm.w.s}, t={input$fuzzyjoin.parm.w.t})")
                  else 
                  if (input$fuzzyjoin.dist=="jw")
                     glue("p={input$fuzzyjoin.parm.p}, bt={input$fuzzyjoin.parm.bt}")
                  else
                  if (input$fuzzyjoin.dist %in% c("qgram","cosine"))
                    glue("q={input$fuzzyjoin.parm.q}")
                  else ''
                if (.isNotEmpty(parm)) parm <- paste0(', ',parm)
                if (.isNotEmpty(input$fuzzyjoin.dist.col)) 
                  col<- paste0(',\n     ',glue("distance_col=\"{make.names(input$fuzzyjoin.dist.col)}\""))
                if (!is.na(input$fuzzyjoin.dist.max))
                  max<- paste0(',\n     ',glue("max_dist={input$fuzzyjoin.dist.max}"))
              }
            else # --- 1-1 strings column, function from menu or input --------
              if ((length(input$fuzzyjoin.fun)==0)
                ||(nchar(input$fuzzyjoin.fun)==0)) key <- "" # Not ready yet
              else {
                f <- input$fuzzyjoin.fun
                     if (f=="contains") f <- "function(x,y) grepl(y,x,fixed=TRUE)"
                else if (f=="example")  f <- "function(x,y) (x==y)|((x=='20')&(y %in% c('2A','2B')))"
                fun <- glue("match_fun={f}")
              }
          else # ----- 1-1 nonstrings column ----------------------------------
              if ((length(input$fuzzyjoin.fun)==0)
                ||(nchar(input$fuzzyjoin.fun)==0)) key <- "" # Not ready yet
              else 
                 fun <- glue("match_fun={input$fuzzyjoin.fun}")
        }
        else # ------- n-m columns --------------------------------------------
        if (length(input$fuzzyjoin.fun)==0) key <- "" # Not ready yet
        else {
          by <- glue("multi_by=list(x={.collapse2(input$fuzzyjoin.columns)},y={.collapse2(input$fuzzyjoin.columns2)}),")
          fun <-paste0(
            "multi_match_fun=function (x,y) ",
            expr(input$fuzzyjoin.fun,.name(input$fuzzyjoin.columns),.name(input$fuzzyjoin.columns2))
          )
        }
        if (key!="") 
          .IGoR$command2(
            key,
            glue("_join({input$fuzzyjoin.data}, mode=\"{input$fuzzyjoin.type}\""),
            ',\n     ',by,
            ',\n     ',fun,
            parm, 
            max,
            col,
            ')'
          )
      }
  ) )
  
  observeEvent(input$fuzzyjoin.command2,.IGoR$try(input,output,"fuzzyjoin"))

}
  
  
