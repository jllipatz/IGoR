
.IGoR$page$lorenz$ui <- function() .IGoR$gUI("lorenz","'gf_lorenz' : Courbe de Lorenz")


.IGoR$page$lorenz$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"lorenz")
  
  output$lorenz.save.control <- renderUI(if (.isNotEmpty(input$lorenz.X)) .IGoR$save.ui("lorenz"))
 
  output$lorenz.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("lorenz.X",  label=.IGoR$NUMVAR1,
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer"))))),
            column(width=6, uiOutput("lorenz.X.label"))
          ),
          uiOutput("lorenz.save.control")
        )
      ))

  .IGoR$gVarLabelUI(input,output,"lorenz","X")
  
  output$lorenz.command2 <- renderUI(
    .IGoR$textarea("lorenz", "gf_lorenz(~x)", 3,
      if (.isNotEmpty(input$lorenz.X)) 
        .IGoR$command2(
          glue("gf_lorenz( ~ {input$lorenz.X})"),
			    .IGoR$gTitleCmd(input,"lorenz",X=TRUE),
          .IGoR$gSaveCmd(input,"lorenz")
  ) )   )
  
}

