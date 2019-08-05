
.IGoR$page$col$ui <- function()
  .IGoR$gUI("col","Distribution d'une variable qualitative",
    p("La fonction ", code("gf_col"), " du package ", strong("ggformula"), "permet de construire un graphique de ",
      span("barres de taille proportionnelle à une variable quantitative", style="color:blue"), " supposée être un dénombrement sur une donnée qualitative.", br(),
      "La page permet de mettre en vis à vis une seconde variable de cumuls."
  ) )

.IGoR$page$col$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"col")
  
  output$col.save.control <- renderUI(if (.isNotEmpty(input$col.X)&&.isNotEmpty(input$col.N)) .IGoR$save.ui("col"))
 
  output$col.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
		    column(width=6,
				selectizeInput("col.X", label=.IGoR$s1(.IGoR$QALVARX1),
                           choices=c(.IGoR$DISCOLV,.columns(input$main.data,"discrete")))),
			column(width=6,
				selectizeInput("col.reorder", .IGoR$s3("Trier par :"),
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric"))))
          ),
          uiOutput("col.save.control")
        ),
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6,
                selectizeInput("col.N", .IGoR$s1("Données cumulées"),
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, selectizeInput("col.N.color",.IGoR$s2("Couleur"),choices=.IGoR$COLORS))
            ),
            fluidRow(
              column(width=6,
                selectizeInput("col.M", .IGoR$s3("Données cumulées en vis à vis"),
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("col.M.color"))
            ),
            checkboxInput("col.coordflip",.IGoR$s4("Barres horizontales"),FALSE)
        ) )
  ) )
  
  output$col.M.color <- renderUI(
    if (.isNotEmpty(input$col.M)) selectizeInput("col.M.color",.IGoR$s2("Couleur"),.IGoR$COLORS)
  )
  
  output$col.command2 <- renderUI(
    .IGoR$textarea("col", "gf_col(y~x)", 5,
      if (.isNotEmpty(input$col.X)&&.isNotEmpty(input$col.N)) {
        s <- c(if (.isNotEmpty(input$col.M))
               paste0(
                 "\n     ",
                 "scale_y_continuous(\n       labels=abs,\n       ",
                 glue("limits=max(max({input$main.data}${input$col.N},na.rm=TRUE),max({input$main.data}${input$col.M},na.rm=TRUE)) %>% c(-.,.)"),
                 ")"
               ),
               if (input$col.coordflip) "\n     coord_flip()"
        )
        cN <- if (.isEQ(input$col.N.color,"black")) "" else glue(", fill=\"{input$col.N.color}\"")
		    x <- if (.isNotEmpty(input$col.reorder)) glue("reorder({input$col.X},{input$col.reorder})") else input$col.X
		    .IGoR$command2(
          glue("gf_col({input$col.N} ~ {x}{cN})"),
          if (.isNotEmpty(input$col.M)) {
            cM <- if (.isEQ(input$col.M.color,"black")) "" else glue(", fill=\"{input$col.M.color}\"")
            paste0(NL,glue("gf_col(-{input$col.M} ~ {input$col.X}{cM})"))
          },
          if (.isNotEmpty(input$col.M)) paste0(NL,glue("gf_labs(y=\"{input$col.M} - {input$col.N}\")")),
          if (length(s)>0) paste0(NL,glue("gf_refine({.collapse(s)})")),
			    .IGoR$gTitleCmd(input,"col"),
          .IGoR$gSaveCmd(input,"col")
        )}
  ) )
  
}
  