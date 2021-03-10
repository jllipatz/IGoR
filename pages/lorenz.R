
### 11/08/2019 1.04.2: Externalisation des libellés en français
### 09/03/2021 1.11.6: Protection contre les noms non normalisés


.IGoR$page$lorenz$ui <- function() .IGoR$ui(page="lorenz", icon="paresseux", graphics=TRUE)


.IGoR$page$lorenz$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"lorenz")
  
  output$lorenz.save.control <- renderUI(if (.isNotEmpty(input$lorenz.X)) .IGoR$save.ui("lorenz"))
 
  output$lorenz.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("lorenz.X", label=.IGoR$s1(.IGoR$Z$any$var.quan), .numeric(input))),
            column(width=6, uiOutput("lorenz.X.label"))
        ) ),
        column(width=6, uiOutput("lorenz.save.control"))
  )   )

  .IGoR$gVarLabelUI(input,output,"lorenz","X")
  
  output$lorenz.command2 <- renderUI(
    .IGoR$textarea("lorenz", "gf_lorenz(~x)", 3,
      if (.isNotEmpty(input$lorenz.X)) 
        .IGoR$command2(
          glue("gf_lorenz( ~ {.nameg(input$lorenz.X)})"),
			    .IGoR$gTitleCmd(input,"lorenz",X=TRUE),
          .IGoR$gSaveCmd(input,"lorenz")
  ) )   )
  
}

