
.IGoR$page$bin2d$ui <- function() .IGoR$gUI("bin2d","'gf_bin2d' : Carte de densité")


.IGoR$page$bin2d$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"bin2d")
  
  output$bin2d.save.control <- renderUI(if (.isNotEmpty(input$bin2d.X)&&.isNotEmpty(input$bin2d.Y)) .IGoR$save.ui("bin2d"))
  
  output$bin2d.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            selectizeInput("bin2d.X",  label=.IGoR$NUMVAR1,
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer")))),
            selectizeInput("bin2d.Y",  label=.IGoR$NUMVAR1,
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer"))))
          ),
          uiOutput("bin2d.save.control")
        )
      ))
  
  output$bin2d.command2 <- renderUI(
    .IGoR$textarea("bin2d", "gf_bin2d(y~x)", 3,
      if (.isNotEmpty(input$bin2d.X)&&.isNotEmpty(input$bin2d.Y)) 
        .IGoR$command2(
          glue("gf_bin2d({input$bin2d.Y} ~ {input$bin2d.X})"),
			    .IGoR$gTitleCmd(input,"bin2d"),
          .IGoR$gSaveCmd(input,"bin2d")
  ) )   )
  
}
