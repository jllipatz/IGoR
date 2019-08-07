### 17/06/2019 1.01.1: Ajout des libellés de variable

.IGoR$page$boxplot$ui <- function() 
  .IGoR$gUI("boxplot", " Distributions d'une variable quantitative",
   p("La fonction ", code("gf_boxplot"), " du package ", strong("ggformula"), "permet de construire des représentations en",
    span("boites à moustaches", style="color:blue"), "résumant les caractéristiques essentielles de la distribution d'une variable qualitative.", br(),
   "Cette représentation est particulièrement utile pour comparer plusieurs sous-populations qui peuvent ici être déterminées ",
   "par les valeurs des modalités d'une seconde variable qui sera, elle, de type qualitatif."
   ),
   dropdown=TRUE
  )


.IGoR$page$boxplot$sv <- function(input, output, session) {

  .IGoR$gServer(input,output,"boxplot")

  output$boxplot.save.control <- renderUI(if (.isNotEmpty(input$boxplot.Y)) .IGoR$save.ui("boxplot"))

  output$boxplot.control <- renderUI(
    if ((length(input$main.data>0))&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("boxplot.Y", label=.IGoR$s1(.IGoR$NUMVARY1),
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("boxplot.Y.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("boxplot.X", label=.IGoR$s3(.IGoR$QALVARX1),
                                           choices=c(.IGoR$QALCOLV,.columns(input$main.data,c("factor","character"))))),
              column(width=6, uiOutput("boxplot.X.label"))
        ) ) ),
        column(width=6, uiOutput("boxplot.save.control"))
  )   )
  
  output$boxplot.dropdown <- renderUI(
    .IGoR$dropdownButton(page="boxplot",
      checkboxInput("boxplot.coordflip",.IGoR$s4("Graphique horizontal"),FALSE)
  ) )
  
  .IGoR$gVarLabelUI(input,output,"boxplot","Y")
  
  .IGoR$gVarLabelUI(input,output,"boxplot","X")
  
  output$boxplot.command2 <- renderUI(
    .IGoR$textarea("boxplot", "gf_boxplot(y~x)", 3,
      if (.isNotEmpty(input$boxplot.Y))
        .IGoR$command2(
          "gf_boxplot(",
          glue(if (.isNotEmpty(input$boxplot.X))
                    "{input$boxplot.Y} ~ {input$boxplot.X}"
               else " ~ {input$boxplot.Y}"),
          ")",
          if (.isTRUE(input$boxplot.coordflip)) paste0(NL,"gf_refine(coord_flip())"),
 		       .IGoR$gTitleCmd(input,"boxplot",Y=TRUE,X=.isNotEmpty(input$boxplot.X)),
		      .IGoR$gSaveCmd(input,"boxplot")
  ) )   )

}