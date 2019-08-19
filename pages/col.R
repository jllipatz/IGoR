
### 10/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$col$ui <- function() .IGoR$ui(page="col", graphics=TRUE)


.IGoR$page$col$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"col")
  
  output$col.save.control <- renderUI(if (.isNotEmpty(input$col.X)&&.isNotEmpty(input$col.N)) .IGoR$save.ui("col"))
 
  output$col.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
		          column(width=6, selectizeInput("col.X", .IGoR$s1(.IGoR$Z$any$var.qual.x), choices=.discrete(input))),
			        column(width=6, selectizeInput("col.reorder", .IGoR$s3(.IGoR$Z$col$reorder), choices=.numeric(input)))
            ),
            .IGoR$s1(.IGoR$Z$any$y),
            fluidRow(
              column(width=6, selectizeInput("col.N", .IGoR$s1(.IGoR$Z$col$var.N), choices=.numeric(input))),
              column(width=6, selectizeInput("col.N.color", .IGoR$s2(.IGoR$Z$any$color), choices=.IGoR$COLORS))
            ),
            fluidRow(
              column(width=6, selectizeInput("col.M", .IGoR$s3(.IGoR$Z$col$var.M), choices=.numeric(input))),
              column(width=6, uiOutput("col.M.color"))
        ) ) ),
        column(width=6, uiOutput("col.save.control"))
  )   )
  
  output$col.dropdown <- renderUI(
    .IGoR$dropdownButton(page="col",
      checkboxInput("col.coordflip",.IGoR$s4(.IGoR$Z$col$coordflip),FALSE)
  ) )
  
  output$col.M.color <- renderUI(
    if (.isNotEmpty(input$col.M)) selectizeInput("col.M.color",.IGoR$s2(.IGoR$Z$any$color), choices=.IGoR$COLORS)
  )
  
  output$col.command2 <- renderUI(
    .IGoR$textarea("col", "gf_col(y~x)", 6,
      if (.isNotEmpty(input$col.X)&&.isNotEmpty(input$col.N)) {
        s <- c(if (.isNotEmpty(input$col.M))
               paste0(
                 "\n     ",
                 "scale_y_continuous(\n       labels=abs,\n       ",
                 glue("limits=with({input$main.data},partial(max,na.rm=TRUE)(c({input$col.N},{input$col.M}))) %>% max() %>% c(-.,.)"),
                 ")"
               ),
               if (.isTRUE(input$col.coordflip)) "\n     coord_flip()"
        )
        cN <- if (.isEQ(input$col.N.color,"black")) "" else glue(", fill=\"{input$col.N.color}\"")
		    x <- if (.isNotEmpty(input$col.reorder)) glue("reorder({input$col.X},{input$col.reorder})") else input$col.X
		    .IGoR$command2(
          glue("gf_col({input$col.N} ~ {x}{cN})"),
          if (.isNotEmpty(input$col.M)) {
            cM <- if (.isEQ(input$col.M.color,"black")) "" else glue(", fill=\"{input$col.M.color}\"")
            paste0(NL,glue("gf_col(-{input$col.M} ~ {input$col.X}{cM})"))
          },
          if (.isNotEmpty(input$col.M)) paste0(NL,glue("gf_labs(y=\"{input$col.M}     |     {input$col.N}\")")),
          if (length(s)>0) paste0(NL,glue("gf_refine({.collapse(s)})")),
			    .IGoR$gTitleCmd(input,"col"),
          .IGoR$gSaveCmd(input,"col")
        )}
  ) )
  
}
  