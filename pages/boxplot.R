
### 17/06/2019 1.01.1: Ajout des libellés de variable
### 11/08/2019 1.04.2: Externalisation des libellés en français
### 09/03/2021 1.11.6: Protection contre les noms non normalisés


.IGoR$page$boxplot$ui <- function() .IGoR$ui(page="boxplot", graphics=TRUE)


.IGoR$page$boxplot$sv <- function(input, output, session) {

  .IGoR$gServer(input,output,"boxplot")

  output$boxplot.save.control <- renderUI(if (.isNotEmpty(input$boxplot.Y)) .IGoR$save.ui("boxplot"))

  output$boxplot.control <- renderUI(
    if ((length(input$main.data>0))&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("boxplot.Y", .IGoR$s1(.IGoR$Z$any$var.quan.y), .numeric(input))),
              column(width=6, uiOutput("boxplot.Y.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("boxplot.X", .IGoR$s3(.IGoR$Z$any$var.qual.x), .discrete(input))),
              column(width=6, uiOutput("boxplot.X.label"))
        ) ) ),
        column(width=6, uiOutput("boxplot.save.control"))
  )   )
  
  output$boxplot.dropdown <- renderUI(
    .IGoR$dropdownButton(page="boxplot",
      checkboxInput("boxplot.coordflip",.IGoR$s4(.IGoR$Z$boxplot$coordflip),FALSE)
  ) )
  
  .IGoR$gVarLabelUI(input,output,"boxplot","Y")
  
  .IGoR$gVarLabelUI(input,output,"boxplot","X")
  
  output$boxplot.command2 <- renderUI(
    .IGoR$textarea("boxplot", "gf_boxplot(y~x)", 3,
      if (.isNotEmpty(input$boxplot.Y))
        .IGoR$command2(
          "gf_boxplot(",
          glue(if (.isNotEmpty(input$boxplot.X))
                    "{.nameg(input$boxplot.Y)} ~ {.nameg(input$boxplot.X)}"
               else " ~ {.nameg(input$boxplot.Y)}"),
          ")",
          if (.isTRUE(input$boxplot.coordflip)) paste0(NL,"gf_refine(coord_flip())"),
 		       .IGoR$gTitleCmd(input,"boxplot",Y=TRUE,X=.isNotEmpty(input$boxplot.X)),
		      .IGoR$gSaveCmd(input,"boxplot")
  ) )   )

}