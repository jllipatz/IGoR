
### 03/06/2019 1.01.0: Formattage des pourcentages
### 14/06/2019 1.01.1: Justification des comptages avec séparateurs des milliers
### 04/08/2019 1.03.3: Transformation optionnelle en facteurs
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 10/12/2019 1.04.6: Messages d'erreur en clair
### 14/01/2020 1.05.3: Résolution d'un conflit sur 'html'
### 16/01/2020 1.05.3: Ajout d'options sur les totaux, sur les titres
### 13/02/2020 1.06.2: Réécriture de l'appel à ShinyFileSave pour corriger un bug de synchronisation
### 03/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
### 17/08/2020 1.10.1: Prise en compte des versions >= 3.6

### TODO Offrir la possibilité de convertir le résultat en data.frame
### TODO Protéger contre les modalités de facteur sous forme de chaine vide 'attempt to use zero-length variable name'
### TODO Rajouter à la demande les titres des varaibles pris dans l'attribut label

.IGoR$page$tabular$ui <- function()
  .IGoR$ui(page="tabular", control=TRUE, command=FALSE,
    .IGoR$commandBox("tabular"),
    tableOutput("tabular.output")
  )


.IGoR$page$tabular$sv <- function(input, output, session) {
  
  .IGoR$bServer(input,output,"tabular")
  
  wtd.percent <- function (x,y) 100*sum(x)/sum(y)

  statsv <- c("n","all","row","col","mean","median","q1","q3","p10","p90","min","max") %>%
  {names(.) <- .; .} %>% .IGoR$Zrename()
  
  statNames <- function(l) names(statsv)[Vectorize(function (x) which(x==statsv))(l)]
  
  stat <- function(i,w) 
    if (nchar(w)==0) 
      c(n="",
        all="Percent('all')",
        row="Percent('row')",
        col="Percent('col')",
        mean="mean",
        median="median",
        q1="quantile*Arguments(p=.25)",
        q3="quantile*Arguments(p=.75)",
        p10="quantile*Arguments(p=.1)",
        p90="quantile*Arguments(p=.9)",
        min="min",
        max="max")[i]
    else {
      w <- .name(w)
      glue(
      c(n="sum*{w}",
        all="Percent('all',fn=wtd.percent)*{w}",
        row="Percent('row',fn=wtd.percent)*{w}",
        col="Percent('col',fn=wtd.percent)*{w}",
        mean="wtd.mean*Arguments(w={w})",
        median="wtd.quantile*Arguments(p=.5,w={w})",
        q1="wtd.quantile*Arguments(p=.25,w={w})",
        q3="wtd.quantile*Arguments(p=.75,w={w})",
        p10="wtd.quantile*Arguments(p=.1,w={w})",
        p90="wtd.quantile*Arguments(p=.9,w={w})",
        min="min",
        max="max")[i]
      )}

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
              column(width=6, uiOutput("tabular.X.all"),
                              uiOutput("tabular.X.nest"))
            ),
            fluidRow(
              column(width=6, uiOutput("tabular.Y")),
              column(width=6, uiOutput("tabular.Y.all"),
                              uiOutput("tabular.Y.nest"))
            ), 
            hr(),
            fluidRow(
              column(width=6, selectizeInput("tabular.W", .IGoR$s3(.IGoR$Z$any$weight), .numeric(input)))
        ) ) ),
        column(width=6,
          box(width='100%',
            column(width=6,radioButtons("tabular.type",.IGoR$s2(.IGoR$Z$tabular$type),.IGoR$Znames("tabular","type",c("n","all","row","col","var")))),
            column(width=6,uiOutput("tabular.args"))
          ),
          box(width='100%',
            checkboxInput("tabular.heading",.IGoR$s4(.IGoR$Z$tabular$heading),FALSE)
          ),
          .IGoR$save.ui("tabular",.title=.IGoR$Z$tabular$save)
  )   ) )
  
  output$tabular.X <- renderUI(
    selectizeInput("tabular.X", .IGoR$Z$tabular$col, 
                   multiple = TRUE, 
                   options = list(placeholder = .IGoR$any[[if (.isTRUE(input$tabular.factor)) "cols.discrete" else "cols.fct"]]),
                   choices = .columns(input$main.data,     if (.isTRUE(input$tabular.factor)) "discrete"      else "factor")
  ) )
  
  output$tabular.X.all <- renderUI(
    if (length(input$tabular.X)>0)
      checkboxInput("tabular.X.all",.IGoR$s4(.IGoR$Z$tabular$all),TRUE)
  )
  
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
  
  output$tabular.Y.all <- renderUI(
    if (length(input$tabular.Y)>0)
      checkboxInput("tabular.Y.all",.IGoR$s4(.IGoR$Z$tabular$all),TRUE)
  )
  
  output$tabular.Y.nest <- renderUI(
    if (length(input$tabular.Y)>1)
      checkboxInput("tabular.Y.nest",.IGoR$s5(.IGoR$Z$tabular$nest),TRUE)
  )
  
  output$tabular.args <- renderUI(
    if (.isEQ(input$tabular.type,'var')&&(length(input$tabular.W)>0))
      tagList(
        selectizeInput("tabular.Z", .IGoR$s1(.IGoR$Z$tabular$Z), .numeric(input)),
        selectizeInput("tabular.Z.funs", .IGoR$s1(.IGoR$Z$tabular$Z.funs), 
                  multiple=TRUE, options = list(placeholder = .IGoR$Z$any$funs),
                  choices=.IGoR$Zrename(c(mean="mean",
                                        median="median",
                                            q1="q1",
                                            q3="q3",
                                           p10="p10",
                                           p90="p90",
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
  
  .IGoR$saveButton(input,output,"tabular")

  output$tabular.command2 <- renderUI(
    .IGoR$textarea("tabular", "tabular(...)", 2, 
      if (length(input$tabular.type)>0) {
        X <- .name(input$tabular.X)
        Y <- .name(input$tabular.Y)
        Z <- .name(input$tabular.Z)
        all <- if (.isTRUE(input$tabular.heading)) glue('Heading("{.IGoR$Z$tabular$all.heading}")*1') else "1"
        z <- if (input$tabular.type=='var')
               if (.isNotEmpty(input$tabular.Z)&&(length(input$tabular.Z.funs)>0)) {
                 h <- paste0('Heading("',
                             if (.isTRUE(input$tabular.heading)) statNames(input$tabular.Z.funs)
                             else input$tabular.Z.funs,
                             '")*')
                 g <- map_chr(input$tabular.Z.funs,function(x) stat(x,input$tabular.W))
                 f <- paste0(
                   if (.isTRUE(input$tabular.heading)) h else ifelse(str_detect(g,'\\*'),h,''),
                   g
                 )
                 paste0(Z,'*',
                        if (length(input$tabular.Z.funs)==1) f
                        else paste0('(',paste(f, collapse='+'),')')
                 )
               }
               else ""
             else 
              paste0(
                if ((input$tabular.type!='n')&&.isTRUE(input$tabular.heading))
                  glue('Heading("{statNames(input$tabular.type)}")*'),
                stat(input$tabular.type,input$tabular.W),
                if ((input$tabular.type!='n')&&(length(input$tabular.digits)>0))
                  glue('*Format(partial(sprintf,"%.{input$tabular.digits}f")())*Justify(r)')
              )
        y <- if (length(Y)==0) ""
             else paste0(
               if (length(Y)>1)
                    paste(Y,collapse=if (!.isTRUE(input$tabular.Y.nest)) '+' else '*')
               else Y,
               if (.isTRUE(input$tabular.Y.all)) paste0('+',all)
             )
        x <- if (length(X)==0) ""
             else paste0(
               if (length(X)>1)
                    paste(X,collapse=if (!.isTRUE(input$tabular.X.nest)) '+' else '*')
               else X,
               if (.isTRUE(input$tabular.X.all)) paste0('+',all)
             )
        if ((nchar(z)==0)&&(nchar(x)==0)) z <- all
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
            f  <- parseSavePath(.IGoR$volumes,input$tabularSaveButton)$datapath
            if (.isNotEmpty(f)) paste0(NL,"{",glue("Hmisc::html(.,\"{f}\")"),";.}")
          }
        )
      }
) )
  
render <- if ((version$major<"4")&&(version$minor<"6.0")) renderPrint else renderText
toHtml <- if ((version$major<"4")&&(version$minor<"6.0")) Hmisc::html else tables::html.tabular
                  
observeEvent(input$tabular.command2,
    isolate(
      output$tabular.output <- render(
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
           toHtml(x,options=htmloptions(pad=TRUE, HTMLleftpad=TRUE))
          }
})))
  
}
