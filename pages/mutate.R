
### 07/08/2019 1.04.2: str_split, just for the fun!
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 14/08/2019 1.04.3: row_number, identity, possibilité d'ajouter un label

### BUG coalesce(a,0) ne marche pas si a est "integer", faire coalesce(a,0L)
### TODO Possibilité d'effacer le label?

.IGoR$page$mutate$ui <- function() .IGoR$ui(page="mutate", control=TRUE)


.IGoR$page$mutate$sv <- function(input, output, session) {

  .IGoR$aServer(input,output,"mutate")
    
  output$mutate.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        fluidRow(
          column(width=6,
            box(width='100%',
              column(width=6, textInput("mutate.new",  .IGoR$s2(.IGoR$Z$any$new.col),"mutate.new")),
              column(width=6, textInput("mutate.label",.IGoR$s3(.IGoR$Z$mutate$label), ''))
          ) ),
          column(width=6, .IGoR$load.ui("mutate", out=input$main.data))
        ),
        box(width='100%', 
          column(width=3, .IGoR$expr.type.ui("mutate")),
          column(width=3, uiOutput("mutate.group")),
          column(width=6, uiOutput("mutate.expr.what"))
  )   ) )
  
  output$mutate.expr.what <- renderUI(
    if (length(input$mutate.type)>0)
           if (input$mutate.type==0) textInput("mutate.expr",.IGoR$s1(.IGoR$Z$mutate$expr))
      else if (input$mutate.type==2)
        tagList(
          fluidRow(
            column(width=4, selectizeInput("mutate.old", label=.IGoR$s1(.IGoR$Z$any$var), .column(input))),
            column(width=4, uiOutput("mutate.arg1")),
            column(width=4, uiOutput("mutate.arg2"))
          ),
          uiOutput("mutate.fun")
        )
       else if (input$mutate.type==3)
         tagList(
           column(width=4, selectizeInput("mutate.old1", label=.IGoR$s1(.IGoR$Z$any$var), .column(input))),
           column(width=4,
              selectizeInput("mutate.op", .IGoR$s1(.IGoR$Z$any$operator),
                choices=.IGoR$Zrename(c(none.op='',
                                           plus="+",
                                          minus="-",
                                          times="*",
                                      dividedBy="/",
                                            and="&",
                                             or="|",
                                           isEQ="==",
                                           isNE="!=",
                                           isGT=">",
                                           isGE=">=",
                                       pastedTo=" paste0"))
           ) ),
           column(width=4, selectizeInput("mutate.old2", label=.IGoR$s1(.IGoR$Z$any$var), .column(input)))
         )
  )
  
  output$mutate.fun <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) {
      c <- get(input$main.data,envir=.GlobalEnv)[[input$mutate.old]]
      fluidRow(
        column(width=8,
          selectizeInput("mutate.fun",NULL,
            choices=.IGoR$Zrename(
             c(none.fun='',
               identity="c0 : identity",
              if (is.character(c))
                c(iconv="c2c>>iconv",
           str_to_upper="c0 : str_to_upper",
           str_to_lower="c0 : str_to_lower",
               str_sub2="c2n--str_sub",
               str_sub1="c1n- str_sub",
            str_extract="c1c< str_extract",
            str_replace="c2c<=str_replace",
              str_split="c1c< str_split",
             str_detect="c1c< str_detect",
             str_length="c0 : str_length",
               coalesce="p1c= coalesce")
              else
              if (is.numeric(c))
                  c(sum="n0 : sum",
                   mean="n0 : mean",
               quantile="n1n: quantile",
                    max="n0 : max",
                    min="n0 : min",
                  first="m0 : first",
                   last="m0 : last",
               coalesce="p1n= coalesce",
                 format="x1c: sprintf")
              else
         c(as.character="f0 : as.character"),
                na.locf="r0 : na.locf")
        ))),
        column(width=4, 
          checkboxInput("mutate.pipe",.IGoR$s5(.IGoR$Z$mutate$pipe), TRUE),
          uiOutput("mutate.narm")
      ))
  })
  
  output$mutate.narm <- renderUI(
    if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,1,1)=='n'))
      checkboxInput("mutate.narm",.IGoR$s5(.IGoR$Z$any$na.rm),TRUE)
  )

  output$mutate.arg1 <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) 
      if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,2,2)>0))
        if (substr(input$mutate.fun,3,3)=="c")
          textInput("mutate.chr.arg1", .IGoR$s2(
                 if (substr(input$mutate.fun,4,4)==">") .IGoR$Z$any$from
            else if (substr(input$mutate.fun,4,4)=="=") .IGoR$Z$any$by
            else if (substr(input$mutate.fun,4,4)=="<") .IGoR$Z$any$prx
            else ""),
            switch(substring(input$mutate.fun,6),
              iconv="850",
              sprintf="<%5d>"
          ))
        else
          numericInput("mutate.num.arg1", .IGoR$s2(
                 if (substr(input$mutate.fun,4,4)=="-") .IGoR$Z$any$from
            else if (substr(input$mutate.fun,4,4)=="=") .IGoR$Z$any$by
            else ""),
            switch(substring(input$mutate.fun,6),
              quantile=.5,
              coalesce=0
          ))
  )
  
  output$mutate.arg2 <- renderUI(
    if (.isEQ(input$mutate.type,2)&&.isNotEmpty(input$mutate.old)) 
      if ((length(input$mutate.fun)>0)&&(substr(input$mutate.fun,2,2)>1))
        if (substr(input$mutate.fun,3,3)=="c")
          textInput("mutate.chr.arg2", .IGoR$s2(
                 if (substr(input$mutate.fun,5,5)=="=") .IGoR$Z$any$by
            else if (substr(input$mutate.fun,5,5)==">") .IGoR$Z$any$into
            else ""),
            if (substring(input$mutate.fun,6)=="iconv") "UTF-8"
          )
        else
          numericInput("mutate.num.arg2", .IGoR$s2(
            if (substr(input$mutate.fun,5,5)=="-") .IGoR$Z$any$to else ""),
            NULL
          )
  )  
  
  output$mutate.group <- renderUI(
    if (length(input$mutate.type)>0)
      if (((input$mutate.type==0)&&.isNotEmpty(input$mutate.expr))
        || (input$mutate.type==1)
        ||((input$mutate.type==2)&&(length(input$mutate.fun)>0)&&(substr(input$mutate.fun,1,1) %in% c('n','m')))
         ) .IGoR$group.ui(input,"mutate", box=FALSE)
  )
  
  label <- function() paste0('{',glue("attr(.${input$mutate.new},'label')<- {shQuote(input$mutate.label)}"),'; .}')
 
  output$mutate.command2 <- renderUI(
    .IGoR$textarea("mutate", "mutate(column=expression)", 3,
      if ((length(input$mutate.type)>0)
        &&(((input$mutate.type==0)&&.isNotEmpty(input$mutate.expr))
         || (input$mutate.type==1)
         ||((input$mutate.type==2)&&.isNotEmpty(input$mutate.fun)&&.isNotEmpty(input$mutate.old))
         ||((input$mutate.type==3)&&.isNotEmpty(input$mutate.old1)&&.isNotEmpty(input$mutate.op)&&.isNotEmpty(input$mutate.old2))
         ))
        if ((length(input$mutate.label)>0) 
          &&(((input$mutate.type==0)&&(input$mutate.new==str_squish(input$mutate.expr)))
           ||((input$mutate.type==2)&&(substring(input$mutate.fun,6)=="identity")&&(input$mutate.old==input$mutate.new))
           )) .IGoR$command2(label())
        else
        .IGoR$command2(
          .IGoR$group_by(input,"mutate"),
          "mutate(",
          input$mutate.new,
          " = ",
               if (input$mutate.type==0) input$mutate.expr
          else if (input$mutate.type==1) "row_number()"
          else if (input$mutate.type==2)
            if (substring(input$mutate.fun,6)=="identity") input$mutate.old
            else
            if ((substring(input$mutate.fun,1,1)=='x')&&(length(input$mutate.chr.arg1)>0))
              if (.isTRUE(input$mutate.pipe))
                   glue("{input$mutate.old} %>% {substring(input$mutate.fun,6)}(\"{input$mutate.chr.arg1}\",.)")
              else glue("{substring(input$mutate.fun,6)}(\"{input$mutate.chr.arg1}\",{input$mutate.old})")
            else
              paste0(
                if (.isTRUE(input$mutate.pipe))
                     paste0(input$mutate.old," %>% ",substring(input$mutate.fun,6),'(')
                else paste0(substring(input$mutate.fun,6),'(',input$mutate.old),
                # Contournement d'un pb sur 'first', 'last'; Error in mutate_impl(.data, dots) : bad value
                if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,1,2)=="m0")) '.', 
                if (substr(input$mutate.fun,2,2)>0)
                  paste0(
                    if (.isTRUE(input$mutate.pipe)) '' else ',',
                    if ((substr(input$mutate.fun,3,3)=="c")&&(length(input$mutate.chr.arg1)>0))
                      paste0('"',input$mutate.chr.arg1,'"')
                    else
                    if ((substr(input$mutate.fun,3,3)=="n")&&(length(input$mutate.num.arg1)>0))
                      input$mutate.num.arg1,
                    if (substr(input$mutate.fun,2,2)>1)
                      if ((substr(input$mutate.fun,3,3)=="c")&&(length(input$mutate.chr.arg2)>0))
                        paste0(',"',input$mutate.chr.arg2,'"')
                      else
                      if ((substr(input$mutate.fun,3,3)=="n")&&(length(input$mutate.num.arg2)>0))
                        paste0(',',input$mutate.num.arg2)
                  ),
                if ((substr(input$mutate.fun,1,1)=="n")&&(length(input$mutate.narm)>0))
                  paste0(
                    if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,2,2)==0)) '' else ', ',
                    glue("na.rm={input$mutate.narm}")
                  ),
                if (substr(input$mutate.fun,1,1)=="r")
                  paste0(
                    if (.isTRUE(input$mutate.pipe)&&(substr(input$mutate.fun,2,2)==0)) '' else ', ',
                    "na.rm=FALSE"
                  ),
              ')'
              )
            else if (input$mutate.type==3)
              if (substr(input$mutate.op,1,1)==" ")
                   glue("{substring(input$mutate.op,2)}({input$mutate.old1},{input$mutate.old2})")
              else glue("{input$mutate.old1} {input$mutate.op} {input$mutate.old2}"),
            ')',
            .IGoR$ungroup(input,"mutate"),
            if (.isNotEmpty(input$mutate.label)) paste0(NL,label())
  ) )   )
  
  # Ce n'est pas tout à fait correct en cas de modification du nom de la colonne dans command2
  observeEvent(input$mutate.command2, 
    .IGoR$try(input,output,"mutate",
      function(x)
        paste(
          if (input$mutate.new %not in% .columns(input$main.data)) ""
          else sprintf(.IGoR$Z$mutate$msg.duplicated,input$mutate.new),
          sprintf(.IGoR$Z$mutate$msg.result,input$mutate.new,class(x[[input$mutate.new]])),
          sep='\n')))
 
}

