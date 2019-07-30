
.IGoR$page$arrange$ui <- function()
  div( id = "bloc_arrange",
    fluidRow(
      column(width=4, 
        img(src="images/arrange.png", height = "48px"),
        h3(span("Trier la table courante", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction", code("arrange"), " du package ", strong("dplyr"), "construit une nouvelle",
          span("table réordonnée seleon l'ordre des modalités", style='color:blue'), "prises par une ou plusieurs variables."
    ) ) ),
    uiOutput("arrange.control"),
    .IGoR$commandBox("arrange")
  )


.IGoR$page$arrange$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"arrange",FALSE)
  
  output$arrange.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("arrange.columns", label=.IGoR$VARS,
                           multiple = TRUE, options = list(placeholder = .IGoR$COLS),
                           choices = .columns(input$main.data))),
            column(width=6, uiOutput("arrange.desc"))
        )),
        column(width=6,
          .IGoR$loadBox("arrange",input$main.data)    
        )
  ))
  
  output$arrange.desc <- renderUI(
    if (length(input$arrange.columns)>0)
      selectizeInput("arrange.desc", label="Ordre décroissant pour les variables :",
                     multiple = TRUE, options = list(placeholder = .IGoR$COLS),
                     choices = input$arrange.columns)
  )

  output$arrange.command2 <- renderUI(
    .IGoR$textarea("arrange", "arrange(columns)", 3,
      if (length(input$arrange.columns)>0) {
        l <- ifelse(input$arrange.columns %in% input$arrange.desc,
                    glue("desc({input$arrange.columns})"),
                    input$arrange.columns)
        .IGoR$command2(glue("arrange({.collapse(l)})"))
      }
  ) )

}