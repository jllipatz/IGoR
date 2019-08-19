
### 11/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$bin2d$ui <- function() .IGoR$ui(page="bin2d", icon="paresseux", graphics=TRUE)


.IGoR$page$bin2d$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"bin2d")
  
  output$bin2d.save.control <- renderUI(if (.isNotEmpty(input$bin2d.X)&&.isNotEmpty(input$bin2d.Y)) .IGoR$save.ui("bin2d"))
  
  output$bin2d.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("bin2d.X", .IGoR$s1(.IGoR$Z$any$var.quan.x), .numeric(input))),
              column(width=6, uiOutput("bin2d.X.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("bin2d.Y", .IGoR$s1(.IGoR$Z$any$var.quan.y), .numeric(input))),
              column(width=6, uiOutput("bin2d.Y.label"))
        ) ) ),
        column(width=6, uiOutput("bin2d.save.control"))
  )   )
  
  .IGoR$gVarLabelUI(input,output,"bin2d","X")
  
  .IGoR$gVarLabelUI(input,output,"bin2d","Y")
  
  output$bin2d.command2 <- renderUI(
    .IGoR$textarea("bin2d", "gf_bin2d(y~x)", 3,
      if (.isNotEmpty(input$bin2d.X)&&.isNotEmpty(input$bin2d.Y)) 
        .IGoR$command2(
          glue("gf_bin2d({input$bin2d.Y} ~ {input$bin2d.X})"),
			    .IGoR$gTitleCmd(input,"bin2d",X=TRUE,Y=TRUE),
          .IGoR$gSaveCmd(input,"bin2d")
  ) )   )
  
}
