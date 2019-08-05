### 14/06/2019 1.01.1: Libellé de variable
### 19/06/2019 1.01.1: Graduations entières, libellé de l'axe y
### 18/07/2019 1.02.2: Possibilité de tri des barres

.IGoR$page$bar$ui <- function() 
  .IGoR$gUI("bar","Distribution d'une variable qualitative",
    p("La fonction ", code("gf_bar"), " du package ", strong("ggformula"), "permet de construire un graphique de ",
      span("barres de taille proportionnelle au nombre d'observations", style="color:blue"), " ventilées selon les modalités d'une variable qualitative.", br(),
      "Une seconde variable qualitative peut être utilisée pour ventiler les effectifs à l'intérieur de chaque modalité de la première variable."
  ) )


.IGoR$page$bar$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"bar")
  
  output$bar.save.control <- renderUI(if (.isNotEmpty(input$bar.X)) .IGoR$save.ui("bar"))
  
  output$bar.color <- renderUI(
    if (.IGoR$test$meta&&(length(input$bar.color.control)>0))
      selectizeInput("bar.color","", 
                     choices=
                       if (input$bar.color.control==1)
                          c("(aucune)",.columns(input$main.data,c("factor","character")))
                     else .IGoR$COLORS
  ))

  output$bar.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("bar.X", label=.IGoR$s1(.IGoR$QALVARX1),
                                           choices=c(.IGoR$QALCOLV, .columns(input$main.data,c("factor","character","integer"))))),
              column(width=6, uiOutput("bar.X.label"))
          ) ),
          uiOutput("bar.save.control")
        ),
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, radioButtons("bar.color.control", .IGoR$s2("Couleur de remplissage des barres"),
                                          c("En fonction de la variable..."=1,
                                           "Uniforme..."=2))),
              column(width=6,
                uiOutput("bar.color"),
                uiOutput("bar.color.position")
            )),
            fluidRow(
              column(width=6, checkboxInput("bar.coordflip",.IGoR$s4("Barres horizontales"),FALSE)),
              column(width=6, checkboxInput("bar.reorder",.IGoR$s4("Trier les barres"),FALSE))
            ),
            hr(),
            tags$b("Ordonnées"),
            fluidRow(
              column(width=6, checkboxInput("bar.breaks",.IGoR$s4("Graduations entières"),FALSE)),
              tags$head(
                tags$style(type="text/css", "#bar_Y_label label{ display: table-cell; text-align: center; vertical-align: middle; } 
                                             #bar_Y_label .form-group { display: table-row;}")
              ),
              tags$div(id = "bar_Y_label", textInput("bar.Y.label",.IGoR$s2("Titre :"),"count"))
            )
        ))
  ))
  
  .IGoR$gVarLabelUI(input,output,"bar","X")
  
  output$bar.color.position <- renderUI(
    if (.isEQ(input$bar.color.control,1)&&.isNE(input$bar.color,'(aucune)'))
      radioButtons("bar.color.position","",
                   c("Une barre pour toutes les modalités"=1,
                     "Une barre pour chaque modalité"=2))
  )
  
  output$bar.command2 <- renderUI(
    .IGoR$textarea("bar", "gf_bar(~x)", 3,
      if (.isNotEmpty(input$bar.X)&&(length(input$bar.color)>0)) {
        x <- if (.isTRUE(input$bar.reorder)) glue("reorder({input$bar.X},`n()`)") else input$bar.X
        color <- if (input$bar.color %in% c("(aucune)","black")) ""
          else 
            if (input$bar.color.control==1) {
               p <- if (.inOrNULL(input$bar.color.position,1)) "" else ", position='dodge'"
               glue(", fill=~{input$bar.color}{p}")
            }
          else glue(", fill='{input$bar.color}'")
        r <- c(
          if (.isTRUE(input$bar.breaks)) "scale_y_continuous(breaks=function (x) pretty(x, 5))",
          if (.isTRUE(input$bar.coordflip)) "coord_flip()"
        )
        .IGoR$command2(
          if (.isTRUE(input$bar.reorder)) paste0(glue("group_by({input$bar.X})"),NL,"mutate(n())",NL),
          glue("gf_bar( ~ {x}{color})"),
          if (length(r)>0) paste0(NL,glue("gf_refine({paste(r,collapse=', ')})")),
 		      .IGoR$gTitleCmd(input,"bar",X=TRUE,
 		        if (.isNE(input$bar.Y.label,"count")) glue("y={shQuote(input$bar.Y.label)}")
 		      ),
          .IGoR$gSaveCmd(input,"bar")
        )
      }
  ) )
  
}

