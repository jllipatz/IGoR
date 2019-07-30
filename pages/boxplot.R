### 17/06/2019 1.01.1: Ajout des libellés de variable

.IGoR$page$boxplot$ui <- function() .IGoR$gUI("boxplot", "'gf_boxplot' : Distributions d'une variable quantitative")


.IGoR$page$boxplot$sv <- function(input, output, session) {

  .IGoR$gServer(input,output,"boxplot")

  output$boxplot.save.control <- renderUI(if (.isNotEmpty(input$boxplot.Y)) .IGoR$save.ui("boxplot"))

  output$boxplot.control <- renderUI(
    if ((length(input$main.data>0))&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("boxplot.Y", label=.IGoR$NUMVARY1,
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
            column(width=6, uiOutput("boxplot.Y.label"))
          ),
          uiOutput("boxplot.save.control")
        ),
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("boxplot.X", label="Variable qualitative en abscisse (optionnelle)",
                                           choices=c(.IGoR$QALCOLV,.columns(input$main.data,c("factor","character"))))),
              column(width=6, uiOutput("boxplot.X.label"))
            ),
            checkboxInput("boxplot.coordflip","Graphique horizontal",FALSE)
        ))
  ))
  
  .IGoR$gVarLabelUI(input,output,"boxplot","Y")
  
  .IGoR$gVarLabelUI(input,output,"boxplot","X")
  
  output$boxplot.command2 <- renderUI(
    .IGoR$textarea("boxplot", "gf_boxplot(y~x)", 3,
      if (.isNotEmpty(input$boxplot.Y))
        .IGoR$command2(
          glue(if (.isNotEmpty(input$boxplot.X))
                    "gf_boxplot({input$boxplot.Y} ~ {input$boxplot.X})"
               else "gf_boxplot( ~ {input$boxplot.Y})"),
          if (input$boxplot.coordflip) paste0(NL,"gf_refine(coord_flip())"),
 		       .IGoR$gTitleCmd(input,"boxplot",Y=TRUE,X=.isNotEmpty(input$boxplot.X)),
		      .IGoR$gSaveCmd(input,"boxplot")
  ) )   )

}