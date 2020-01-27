
### 09/08/2019 1.04.2: Externalisation des libellés en français
### 27/01/2020 1.05.5: Ajout du tri

.IGoR$page$contents$ui <- function()
  .IGoR$ui(page="contents",icon="table",command=FALSE,
    fluidRow(
      column(width=2,radioButtons("contents.sort",.IGoR$s2(.IGoR$Z$contents$sort),unname(.IGoR$Z$contents$columns))),
      column(width=10,tableOutput("contents"))
  ))


.IGoR$page$contents$sv <- function(input, output, session) {

  output$contents <- renderTable(
    if ((length(input$main.data)>0)&&.IGoR$test$meta) {
      df <- get(input$main.data,envir=.GlobalEnv)
      f <- Vectorize(function(nom) attr(df[[nom]],"label") %>% ifelse(is.null(.),NA,.))
      dt <- data.frame(1:ncol(df),
                       colnames(df),
                       unlist(Map(class,df), use.names=FALSE),
                       f(colnames(df)))
      names(dt) <- .IGoR$Z$contents$columns
      dt[order(dt[[input$contents.sort]]),]
    }
  )

  
}

