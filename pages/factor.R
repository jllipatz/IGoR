
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 03/08/2020 1.09.3: Protection contre les noms de colonnes non normalisés

.IGoR$page$factor$ui <- function() .IGoR$ui(page="factor", icon="rename", control=TRUE)


.IGoR$page$factor$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"factor")
  
  output$factor.control <- renderUI({
    output$factor.comment <- renderText("")
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("factor",buttons.title=.IGoR$s2(.IGoR$Z$factor$factor))),
        column(width=6,
          .IGoR$load.ui("factor",input$main.data),
          box(width='100%',
            column(width=6, 
              selectizeInput("factor.class.out",.IGoR$s2(.IGoR$Z$factor$class),
                choices=c('' %>% {names(.)<- .IGoR$Z$factor$class.none;.},
                         .IGoR$Znames("factor","class",c("factor","as.character","as.double","as.integer","as.logical","as.Date"))))
            ),
            column(width=6,
              checkboxInput("factor.short",.IGoR$s5(.IGoR$Z$factor$short),TRUE),
              uiOutput("factor.empty")
      ) ) ) )
  })
 
  output$factor.empty <- renderUI(
    if (.isEQ(input$factor.class.out,"factor")
      &&(length(input$factor.type)>0)
      &&((input$factor.type!=2)
       ||((input$factor.type==2)
        &&((.isEQ(input$factor.class,"character")&&!input$factor.drop)
         ||(.isNE(input$factor.class,"character")&&input$factor.drop)))))
      checkboxInput("factor.empty",.IGoR$s5(.IGoR$Z$factor$empty),TRUE)
  )

  output$factor.command2 <- renderUI(
    .IGoR$textarea("factor", "mutate(column=as...(column))", 4, 
      if ((length(input$factor.type)>0)&&.isNotEmpty(input$factor.class.out)) {
        na <- if ((input$factor.class.out=="factor")
                &&((input$factor.type!=2)
                 ||((input$factor.type==2)
                  &&((.isEQ(input$factor.class,"character")&&!input$factor.drop)
                   ||(.isNE(input$factor.class,"character")&&input$factor.drop))))
                &&.isTRUE(input$factor.empty)) ", exclude=''" else ""
        .IGoR$command2(
          "mutate",
          if (!.isTRUE(input$factor.short)) {
            old <- .name(.IGoR$select.columns(input,output,"factor"))
            if (length(old)==0) paste0("() # ",.IGoR$Z$factor$nop)
            else glue("({.collapse0(paste0(old,'=',input$factor.class.out,'(',old,na,')'))})")
          }
          else
            paste0(
              if (input$factor.type==2) 
                if (input$factor.drop)
                     glue("_if(Negate(is.{input$factor.class}), ")
                else glue("_if(is.{input$factor.class}, ")
              else 
              if (input$factor.type==3) 
                if (input$factor.drop) 
                     "_at(c(), "
                else "_all("
              else glue("_at({.IGoR$select(input,'factor',vars=TRUE)}, "),
              input$factor.class.out,
              na,
              ")"
        )   ) 
      }
  ) ) 
  
  observeEvent(input$factor.command2,
    .IGoR$try(input,output,"factor",
      .fn=function(x) {
        y <- inner_join(
               do.call(data.frame,Map(class,get(input$main.data,envir=.GlobalEnv))) %>% gather(k,old),
               do.call(data.frame,Map(class,x)) %>% gather(k,new),
               by="k")
        m <- y %>% filter(old!=new) %>% count() %>% pull(n)
        n <- y %>%
             filter((old=="factor")&!(new %in% c("factor","character"))) %>%
             count() %>% pull(n)
        if (m==0) .IGoR$Z$factor$msg.nop
        else paste0(
          sprintf(.IGoR$Z$factor$msg.result,m),
          if (n>0) paste0("\n",.IGoR$Z$factor$msg.factor)
        )
      }
  ))

}

