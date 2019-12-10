
### 03/06/2019 1.01.0: Formattage des pourcentages
### 14/06/2019 1.01.1: Justification des comptages avec séparateurs des milliers
### 04/08/2019 1.03.3: Transformation optionnelle en facteurs
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 10/12/2019 1.04.6: Messages d'erreur en clair

### TODO Offrir la possibilité de convertir le résultat en data.frame
### TODO Protéger contre les modalités de facteur sous forme de chaine vide 'attempt to use zero-length variable name'

.IGoR$page$tabular$ui <- function()
  .IGoR$ui(page="tabular", control=TRUE, command=FALSE,
    .IGoR$commandBox("tabular"),
    tableOutput("tabular.output")
  )


.IGoR$page$tabular$sv <- function(input, output, session) {
  
  .IGoR$bServer(input,output,"tabular")
  
  wtd.percent <- function (x,y) 100*sum(x)/sum(y)
  
  stat <- function(.i,.w) glue(
    if (nchar(.w)==0) 
      c(count="",
        all="Percent('all')",
        row="Percent('row')",
        col="Percent('col')",
        mean="mean",
        median="median",
        q1="Heading('q1')*quantile*Arguments(p=.25)",
        q3="Heading('q3')*quantile*Arguments(p=.75)",
        p10="Heading('p10')*quantile*Arguments(p=.1)",
        p90="Heading('p90')*quantile*Arguments(p=.9)")[.i]
    else 
      c(count="sum*{.w}",
        all="Percent('all',fn=wtd.percent)*{.w}",
        row="Percent('row',fn=wtd.percent)*{.w}",
        col="Percent('col',fn=wtd.percent)*{.w}",
        mean="Heading('mean')*wtd.mean*Arguments(w={.w})",
        median="Heading('median')*wtd.quantile*Arguments(p=.5,w={.w})",
        q1="Heading('q1')*wtd.quantile*Arguments(p=.25,w={.w})",
        q3="Heading('q3')*wtd.quantile*Arguments(p=.75,w={.w})",
        p10="Heading('p10')*wtd.quantile*Arguments(p=.1,w={.w})",
        p90="Heading('p90')*wtd.quantile*Arguments(p=.9,w={.w})")[.i]
  )

  output$tabular.control <- renderUI(
    if ((length(input$main.data>0))&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, .IGoR$s1(.IGoR$Z$tabular$vars.qual)),
              column(width=6, checkboxInput("tabular.factor",.IGoR$s4(.IGoR$Z$tabular$factor),FALSE))
            ),
            fluidRow(
              column(width=6, uiOutput("tabular.X")),
              column(width=6, uiOutput("tabular.X.nest"))
            ),
            fluidRow(
              column(width=6, uiOutput("tabular.Y")),
              column(width=6, uiOutput("tabular.Y.nest"))
            ), 
            hr(),
            fluidRow(
              column(width=6, selectizeInput("tabular.W", .IGoR$s3(.IGoR$Z$any$weight), .numeric(input)))
        ) ) ),
        column(width=6,
          box(width='100%',
            column(width=6,radioButtons("tabular.type",.IGoR$s2(.IGoR$Z$tabular$type),.IGoR$Znames("tabular","type",c("count","all","row","col","var")))),
            column(width=6,uiOutput("tabular.args"))
          ),
          uiOutput("tabular.save.control")
  )   ) )
  
  output$tabular.X <- renderUI(
    selectizeInput("tabular.X", .IGoR$Z$tabular$col, 
                   multiple = TRUE, 
                   options = list(placeholder = .IGoR$any[[if (.isTRUE(input$tabular.factor)) "cols.discrete" else "cols.fct"]]),
                   choices = .columns(input$main.data,     if (.isTRUE(input$tabular.factor)) "discrete"      else "factor")
  ) )

  output$tabular.X.nest <- renderUI(
    if (length(input$tabular.X)>1)
      checkboxInput("tabular.X.nest",.IGoR$s5(.IGoR$Z$tabular$nest),TRUE)
  )
  
  output$tabular.Y <- renderUI(
    selectizeInput("tabular.Y", .IGoR$Z$tabular$row, 
                   multiple = TRUE, 
                   options = list(placeholder = .IGoR$any[[if (.isTRUE(input$tabular.factor)) "cols.discrete" else "cols.fct"]]),
                   choices = .columns(input$main.data,     if (.isTRUE(input$tabular.factor)) "discrete"      else "factor")
  ) )
  
  output$tabular.Y.nest <- renderUI(
    if (length(input$tabular.Y)>1)
      checkboxInput("tabular.Y.nest",.IGoR$s5(.IGoR$Z$tabular$nest),TRUE)
  )
  
  output$tabular.args <- renderUI(
    if (.isEQ(input$tabular.type,'var')&&(length(input$tabular.W)>0))
      tagList(
        selectizeInput("tabular.Z", .IGoR$s1(.IGoR$Z$tabular$Z), .numeric(input)),
        selectizeInput("tabular.Z.fun", .IGoR$s1(.IGoR$Z$tabular$Z.fun), 
                  multiple=TRUE, options = list(placeholder = .IGoR$Z$any$funs),
                  choices=.IGoR$Zrename(c(mean=stat("mean",input$tabular.W),
                                        median=stat("median",input$tabular.W),
                                            q1=stat("q1",input$tabular.W),
                                            q3=stat("q3",input$tabular.W),
                                           p10=stat("p10",input$tabular.W),
                                           p90=stat("p90",input$tabular.W),
                                           min="min",
                                           max="max")))
        )
    else
    if (.isIn(input$tabular.type,c("col","row","all")))
      sliderInput("tabular.digits",.IGoR$s2(.IGoR$Z$tabular$digits),0,2,2)
    else
    if (.isEQ(input$tabular.type,"count"))
      checkboxInput("tabular.sep",.IGoR$s4(.IGoR$Z$tabular$sep),FALSE)
  )
  
  output$tabular.save.control <- renderUI(if (.isNotEmpty(input$tabular.X)) .IGoR$save.ui("tabular",.title=.IGoR$Z$tabular$save))
  
  output$tabular.save <- renderUI(
    if (.isTRUE(input$tabular.save))
      shinySaveButton("tabular", .IGoR$Z$any$browse, .IGoR$Z$tabular$save.as, filetype=list(html="html"))
  )
  
  shinyFileSave(input, "tabular", roots=.IGoR$volumes, defaultPath='', defaultRoot='home')

  output$tabular.command2 <- renderUI(
    .IGoR$textarea("tabular", "tabular(...)", 2, 
      if (length(input$tabular.type)>0) {
        z <- if (input$tabular.type=='var')
               if (.isNotEmpty(input$tabular.Z)&&(length(input$tabular.Z.fun)>0))
                 paste0(input$tabular.Z,'*',
                        if (length(input$tabular.Z.fun)>1)
                             paste0('(',paste(input$tabular.Z.fun, collapse='+'),')')
                        else input$tabular.Z.fun
                 )
               else ""
             else 
              stat(input$tabular.type,input$tabular.W)
        if ((input$tabular.type %in% c('col','row','all'))&&(length(input$tabular.digits)>0))
          z <- paste0(z,glue("*Format(partial(sprintf,\"%.{input$tabular.digits}f\")())*Justify(r)"))
        y <- if (length(input$tabular.Y)==0) ""
             else paste0(
               if (length(input$tabular.Y)>1)
                 paste(input$tabular.Y,
                       collapse=if (!.isTRUE(input$tabular.Y.nest)) '+' else '*')
               else input$tabular.Y,
               '+1')
        x <- if (length(input$tabular.X)==0) ""
             else paste0(
               if (length(input$tabular.X)>1)
                 paste(input$tabular.X,
                       collapse=if (!.isTRUE(input$tabular.X.nest)) '+' else '*')
               else input$tabular.X,
               '+1')
        if ((nchar(z)==0)&&(nchar(x)==0)) z <- "1"
        else if ((nchar(z)>0)&&(nchar(x)>0)) x <- glue("*({x})")
        x <- paste0(z,x)
        if ((input$tabular.type=='count')&&.isTRUE(input$tabular.sep))
          x <- paste0(if (x=="1") x else glue("({x})"),"*Format(partial(format,big.mark=' ')())*Justify(r)")
        .IGoR$command2(
          if (.isTRUE(input$tabular.factor)) {
            df <- get(input$main.data,envir=.GlobalEnv)
            l <- c(input$tabular.X,input$tabular.Y)
            l <- l[map_lgl(l,function(x) !is.factor(df[[x]]))]
            if (length(l)>0) paste0(glue("mutate_at({.collapse2(l)}, as.factor)"), NL)
          },
          glue("tabular({y} ~ {x}, . )"),
          if (.isTRUE(input$tabular.save)) {
            f  <- parseSavePath(.IGoR$volumes,input$tabular)$datapath
            if (.isNotEmpty(f)) paste0(NL,"{",glue("Hmisc::html(.,\"{f}\")"),";.}")
          }
        )
      }
) )
                  
observeEvent(input$tabular.command2,
    isolate(
      output$tabular.output <- renderPrint(
        if (nchar(input$tabular.command2)>0) {
          x <- tryCatch(eval(parse(text=paste0(input$main.data,' %>% ',input$tabular.command2))),
                        error=function(e) {
                          if (e$message %in% c("node stack overflow", "pile de noeuds débordée vers le haut"))
                            e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error1)
                          else if (e$message=="No levels in 'x'!")
                            e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error0)
                          else if (e$message=="attempt to use zero-length variable name")
                            e$message <- paste0(e$message,"\n",.IGoR$Z$tabular$msg.error2)
                          output$tabular.comment <- renderText(e$message)
                          NULL
                })
          if (!is.null(x)) {
           output$tabular.comment <- renderText("")
           html(x,options=htmloptions(pad=TRUE, HTMLleftpad=TRUE))
          }
})))
  
}
