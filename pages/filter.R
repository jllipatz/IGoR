
### 25/07/2019 1.03.0: Mode assisté (keep quiet: temperature was over 35°C!)
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 14/08/2019 1.04.3: row_number
### 02/12/2019 1.04.6: désactivation du group-by lors d'un deuxième passage

.IGoR$page$filter$ui <- function() .IGoR$ui(page="filter", control=TRUE)


.IGoR$page$filter$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"filter")
  
  output$filter.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        fluidRow(
          column(width=6, box(width='50%', checkboxInput("filter.drop",.IGoR$s4(.IGoR$Z$filter$drop), FALSE))),
          column(width=6, .IGoR$load.ui("filter"))
        ),
        box(width='100%',
          column(width=3, .IGoR$expr.type.ui("filter",.IGoR$s2(.IGoR$Z$filter$filter))),
          column(width=3, uiOutput("filter.group")),
          column(width=6, uiOutput("filter.expr.what")
  )   ) ) ) 

  output$filter.expr.what <- renderUI(
    if (length(input$filter.type)>0)
           if (input$filter.type==0) textInput("filter.where",.IGoR$s1(.IGoR$Z$filter$where))
      else if (input$filter.type==1) numericInput("filter.no",.IGoR$s2(.IGoR$Z$filter$no),1)
      else
        tagList(
          column(width=3,
            selectizeInput("filter.arg1",.IGoR$s1(.IGoR$Z$any$var), choices=.column(input))
          ),
          column(width=5, uiOutput("filter.fun")),
          column(width=4, uiOutput("filter.arg2"))
  )     ) 

  output$filter.fun <- renderUI(
    if ((length(input$filter.type)>0)&&(input$filter.type>=2)
      &&.isNotEmpty(input$filter.arg1)) {
      c <- get(input$main.data,envir=.GlobalEnv)[[input$filter.arg1]]
      selectizeInput("filter.fun", .IGoR$s2(.IGoR$Z$any$operator),
        choices=.IGoR$Zrename(
              if (is.logical(c)) c(isTRUE=" ",
                                     isEQ="on ==",
                                      and="on &",
                                       or="on |",
                                     isNA="f  is.na")
         else if (is.character(c)) c(isEQ="oc ==",
                                belongsTo="oC %in%",
                               startsWith="fc startsWith",
                                 endsWith="fc endsWith",
                                  matches="fc str_detect",
                                     isNA="f  is.na")
         else if (is.numeric(c))   c(isEQ="on ==",
                                belongsTo="oN %in%",
                                     isGT="on >",
                                     isGE="on >=",
                        if (input$filter.type==2)
                              c(isEQ.stat="of ==",
                                isGT.stat="of >"),
                                     isNA="f  is.na")
         else if (is.factor(c))    c(isEQ="oc ==",
                                belongsTo="oC %in%",
                                     isNA="f  is.na")
      ))
    }
  )
  
  output$filter.arg2 <- renderUI(
    if ((length(input$filter.type)>0)&&(input$filter.type>=2)
      &&.isNotEmpty(input$filter.arg1)
      &&(length(input$filter.fun)>0))
      if (input$filter.fun!=' ') {         # else no function expected
        t <- str_sub(input$filter.fun,2,2) # argument type
        if (t!=' ')                        # else no argument expected
          if (input$filter.type==2)
            if (t=='f')
              selectizeInput("filter.arg2", .IGoR$s2(.IGoR$Z$filter$stat),
                choices=.IGoR$Zrename(c(mean="mean",
                                      median="median",
                                          q3="quantile,.75",
                                         p90="quantile,.90",
                                         max="max",
                                         min="min")))
            else
            if (t=='n')
                 numericInput("filter.arg2",.IGoR$s2(.IGoR$Z$filter$value),0)
            else textInput("filter.arg2",if (t %in% c('C','N')) .IGoR$Z$filter$values else .IGoR$Z$filter$value)
          else
            if (input$filter.type==3)
              selectizeInput("filter.arg2",.IGoR$s1(.IGoR$Z$any$var), choices=c(.IGoR$COLV,.columns(input$main.data)))
    }
  )
  
  output$filter.group <- renderUI(
    if (length(input$filter.type)>0)
      if (((input$filter.type==0)&&.isNotEmpty(input$filter.where))
        || (input$filter.type==1)
        ||((input$filter.type==2)&&(length(input$filter.fun)>0)&&(substr(input$filter.fun,2,2)=='f'))
         ) .IGoR$group.ui(input,"filter", box=FALSE)
      else if (length(input$filter.group)>0) hidden(textInput("filter.group","",""))
  )
                                  
  output$filter.command2<- renderUI(
    .IGoR$textarea("filter", "filter(condition)", 3,
      if (length(input$filter.type)>0)
        .IGoR$command2(
          .IGoR$group_by(input,"filter"),
          if ((input$filter.type==1)&&.isNotEmpty(input$filter.where)&&(nchar(msg<-.IGoR$look(input$filter.where))>0))
            paste0(str_sub(msg,2),"\n   "),
          "filter(",
          {
            drop <- .isTRUE(input$filter.drop)
            e <-   if ((input$filter.type==0)&&.isNotEmpty(input$filter.where))
                list(drop,TRUE,input$filter.where)
              else if ((input$filter.type==1)&&.isNotNA(input$filter.no))
                list(FALSE,NA,glue("row_number(){if (drop) '!' else '='}={input$filter.no}"))
              else if ((input$filter.type>=2)
                &&.isNotEmpty(input$filter.arg1)
                &&.isNotEmpty(input$filter.fun)
                &&((input$filter.type==2)||.isNotEmpty(input$filter.arg2))) 
                if (input$filter.fun==' ')                             # - no function (logical column only) ------- 
                  list(drop,FALSE,input$filter.arg1)
                else {                                                 # - a function ------------------------------
                  c <- str_sub(input$filter.fun,1,1)                   # call type
                  t <- str_sub(input$filter.fun,2,2)                   # argument type
                  f <- str_sub(input$filter.fun,4)                     # function name
                  if (t==' ')                                          # -- no argument ----------------------------
                    list(drop,FALSE,glue("{f}({input$filter.arg1})"))  
                  else {                                               # -- one argument ---------------------------
                    arg2 <- if (t=='f') {                              # --- argument is a statistical function ----
                      s <- str_split(input$filter.arg2,',')[[1]]
                      if (length(s)==1)
                           glue("{s}({input$filter.arg1})")
                      else glue("{s[1]}({input$filter.arg1},{s[2]})")
                    } else input$filter.arg2
                    q <- if (input$filter.type==3) c('','')            # --- argument quoting ----------------------
                    else switch (t,
                      c = c('"','"'),
                      f =,
                      n = c('',''),
                      C =,
                      N = c('c(',')'))
                    if (c=='f')                                        # --- function call -------------------------
                      list(drop,FALSE,glue("{f}({input$filter.arg1},{q[1]}{arg2}{q[2]})"))
                    else {                                             # --- binary operator -----------------------
                      g <- if (drop) switch(f, "=="="!=", ">"="<=", ">="="<")
                      if (is.null(g))
                           list(drop,TRUE,glue("{input$filter.arg1} {f} {q[1]}{arg2}{q[2]}"))
                      else list(FALSE,NA, glue("{input$filter.arg1} {g} {q[1]}{arg2}{q[2]}"))
                  } }
                }
            if (length(e)>0)                                           # negate result if required  ----------------
              if (e[[1]])
                if (e[[2]]) glue("!({e[[3]]})") else glue("!{e[[3]]}")
              else e[[3]]
          },
          ')',
          .IGoR$ungroup(input,"filter")
  ) )   )

}

