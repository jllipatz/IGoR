
### 10/08/2019 1.04.2: Externalisation des libellés en français

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

  output$fuzzyjoin.fun <- renderUI(
    if ((length(input$fuzzyjoin.columns)==1)&&(length(input$fuzzyjoin.columns2)==1))
      textInput("fuzzyjoin.fun",.IGoR$s1(.IGoR$Z$fuzzyjoin$fun))
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
      )))
  } }
  
  output$fuzzyjoin.command2 <- renderUI(
    .IGoR$textarea("fuzzyjoin", "fuzzy_join(table,mode,columns,function)", 4,
      if (.isNotEmpty(input$fuzzyjoin.data)
        &&(length(input$fuzzyjoin.columns)>0)
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
          if (nchar(msg<-.IGoR$look(input$fuzzyjoin.fun))>0) paste0(str_sub(msg,2),"\n   "),
          glue("fuzzy_join({input$fuzzyjoin.data}, mode=\"{input$fuzzyjoin.type}\","),
            '\n     ', by,
            '\n     ', fun
        )
      }
  ) )
  
  observeEvent(input$fuzzyjoin.command2,.IGoR$try(input,output,"fuzzyjoin"))

}
  
  
