
### 12/07/2019 1.02.0: Protection contre les noms de colonne incorrects
### 10/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$rename$ui <- function() .IGoR$ui(page="rename", control=TRUE)


.IGoR$page$rename$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"rename")
  
  TITLE1 = c("e"=.IGoR$Z$rename$expression,"p"=.IGoR$Z$rename$prefix,"s"=.IGoR$Z$rename$suffix)
  FUNS = c("make.names"="    make.names",
         "str_to_lower"="    str_to_lower",
         "str_to_upper"="    str_to_upper",                
          "str_extract"="c e str_extract",
          "str_replace"="cce str_replace",
            ".renumber"="c p function(x,y) paste0(y,seq_len(length(x)))",
              ".prefix"="c p function(x,y) paste0(y,x)",
              ".suffix"="c s paste0")
  
  output$rename.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("rename", buttons.title=.IGoR$s2(.IGoR$Z$rename$rename), buttons.range=TRUE)),
        column(width=6,
          .IGoR$load.ui("rename",input$main.data),
          box(width='100%',
            radioButtons("rename.how",NULL,.IGoR$Znames("rename","how",c("names","fun"))),
            uiOutput("rename.how")
  )   ) ) )
 
  output$rename.how <- renderUI(
    if (length(input$rename.how)>0)
      if (input$rename.how=="names")
        textInput("rename.names",.IGoR$s1(.IGoR$Z$rename$names))
      else
          fluidRow(
            column(width=6,
              selectizeInput("rename.fun",.IGoR$s2(.IGoR$Z$any$fun),choices=.IGoR$Zrename(FUNS))),
            column(width=6, uiOutput("rename.args"))
    )    ) 
    
    output$rename.args <- renderUI(
      if (.isEQ(input$rename.how,"fun")&&(length(input$rename.fun)>0))
        fluidRow(
          if (str_sub(input$rename.fun,1,1)=='c')
            column(width=6, textInput("rename.arg1", .IGoR$s1(TITLE1[str_sub(input$rename.fun,3,3)]))),
          if (str_sub(input$rename.fun,2,2)=='c')
            column(width=6, textInput("rename.arg2", .IGoR$s1(.IGoR$Z$rename$to.expr)))
    ))
    
    output$rename.command2 <- renderUI(
      .IGoR$textarea("rename", "rename...(...)", 4,
        if ((length(input$rename.type)>0)&&(length(input$rename.how)>0)
          &&(((input$rename.how=="names")&&.isNotEmpty(input$rename.names))
           ||((input$rename.how=="fun")&&.isNotEmpty(input$rename.fun))
           ))
          .IGoR$command2( 
            if (input$rename.type==0) 
              if (is.null(input$rename.end)||is.null(input$rename.start)) ""
              else {
                m <- input$rename.end - input$rename.start + 1
                s <- glue(
                       if (.isTRUE(input$rename.drop))
                         if (m==1) "-{input$rename.start}"
                         else      "-({input$rename.start}:{input$rename.end})"
                       else
                         if (m==1) "{input$rename.start}"
                         else      "{input$rename.start}:{input$rename.end}")
                if (input$rename.how=="names") {
                  new <- str_split(str_trim(input$rename.names)," +")[[1]] %>% make.names()
                  n <- if (.isTRUE(input$rename.drop)) ncol(get(input$main.data,envir=.GlobalEnv)) - m else m
                  if (n!=length(new)) paste0("identity() # ",sprintf(.IGoR$Z$rename$msg.badnames,n))
                  else paste0(
                    "{names(.)[",s,"]<- ",
                    .collapse2(new),
                    "; .}")
                }
             else {
                fun <- str_sub(input$rename.fun,5)
                paste0(
                  "{names(.)[",s,"]<- ",
                  if (startsWith(fun,"function")) paste0('(',fun,')') else fun,
                  '(',
                  "names(.)[",s,"]",
                  if ((str_sub(input$rename.fun,1,1)!=' ')&&(length(input$rename.arg1)>0)) glue(", {shQuote(input$rename.arg1)}"),
                  if ((str_sub(input$rename.fun,2,2)!=' ')&&(length(input$rename.arg2)>0)) glue(", {shQuote(input$rename.arg2)}"),
                  ')',
                  "; .}")
            } }
            else 
            if (input$rename.how=="names") {
              new <- str_split(str_trim(input$rename.names)," +")[[1]] %>% make.names()
              old <- .IGoR$select.columns(input,output,"rename")
              n <- length(old) # also for .IGoR$Z$rename$msg.badnames
              if (n==length(new))
                   glue("rename({.collapse(paste0(new,' = \"',old,'\"'))})")
              else paste0("rename() # ",sprintf(.IGoR$Z$rename$msg.badnames,n))
            }
            else
              paste0("rename",
                if (input$rename.type==2)
                   if (input$rename.drop)
                        glue("_if(Negate(is.{input$rename.class}), ")
                   else glue("_if(is.{input$rename.class}, ")
                else
                if (input$rename.type==3) 
                   if (input$rename.drop)
                        "_at(c(), "
                   else "_all("
                else glue("_at({.IGoR$select(input,'rename',vars=TRUE)}, "),
                str_sub(input$rename.fun,5),
                if ((str_sub(input$rename.fun,1,1)!=' ')&&(length(input$rename.arg1)>0)) glue(", {shQuote(input$rename.arg1)}"),
                if ((str_sub(input$rename.fun,2,2)!=' ')&&(length(input$rename.arg2)>0)) glue(", {shQuote(input$rename.arg2)}"),
                ")"
              )
  ) )   )
    
  observeEvent(input$rename.command2, 
    .IGoR$try(input,output,"rename",
              .fn=function(x) isolate({
                y <- data.frame(old=names(get(input$main.data,envir=.GlobalEnv)),
                                new=names(x), 
                                stringsAsFactors=FALSE)
                n <- y %>% filter(old!=new) %>% count() %>% pull(n)
                m <- ncol(x) - length(unique(names(x)))
                paste0(
                  if (m>0) paste0(.IGoR$Z$rename$msg.duplicated,"\n"),
                  sprintf(.IGoR$Z$rename$msg.result,n)
                 )
              }
  ) )       )

}