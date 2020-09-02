
### 10/08/2019 1.04.2: Externalisation des libellés en français
### 10/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
###                    Protection contre les caractères accentués

.IGoR$page$arrange$ui <- function() .IGoR$ui(page="arrange", control=TRUE)


.IGoR$page$arrange$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"arrange",FALSE)
  
  output$arrange.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("arrange.columns", label=.IGoR$s1(.IGoR$Z$any$vars),
                           multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols),
                           choices = .columns(input$main.data))),
            column(width=6, uiOutput("arrange.desc"))
        )),
        column(width=6,
          .IGoR$load.ui("arrange",input$main.data)    
        )
  ))
  
  output$arrange.desc <- renderUI(
    if (length(input$arrange.columns)>0)
      selectizeInput("arrange.desc", .IGoR$s3(.IGoR$Z$arrange$desc),
                     multiple = TRUE, options = list(placeholder = .IGoR$Z$any$cols),
                     choices = iconv(input$arrange.columns,from="UTF-8"))
  )

  output$arrange.command2 <- renderUI(
    .IGoR$textarea("arrange", "arrange(columns)", 3,
      if (length(input$arrange.columns)>0) {
        l <- ifelse(input$arrange.columns %in% input$arrange.desc,
                    glue("desc({.name(input$arrange.columns)})"),
                    .name(input$arrange.columns))
        .IGoR$command2(glue("arrange({.collapse0(l)})"))
      }
  ) )

}