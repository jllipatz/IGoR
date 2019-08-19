
### 09/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$contents$ui <- function()
  .IGoR$ui(page="contents",icon="table",command=FALSE,
    tableOutput("contents")
  )


.IGoR$page$contents$sv <- function(input, output, session) {

  output$contents <- renderTable(
    if ((length(input$main.data)>0)&&.IGoR$test$meta) {
      df <- get(input$main.data,envir=.GlobalEnv)
      f <- Vectorize(function(nom) attr(df[[nom]],"label") %>% ifelse(is.null(.),NA,.))
      dt <- data.frame(colnames(df),
                       unlist(Map(class,df), use.names=FALSE),
                       f(colnames(df)))
      names(dt) <- c(.IGoR$Z$contents$name,.IGoR$Z$contents$class,.IGoR$Z$contents$label)
      dt
    }
  )

  
}

