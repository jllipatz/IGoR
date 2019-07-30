### 14/06/2019 1.01.1: Ajout du libellé de variable

.IGoR$page$histogram$ui <- function() 
  .IGoR$gUI("histogram", "Histogramme d'une variable quantitative",
    p("La fonction", code("gf_histogram"), "du package", strong("ggformula"), "permet de représenter la",
      span("distribution d'une variable quantitative", style='color:blue'), " sous forme de densité.", br(),
      "La page permet de superposer la courbe représentant une estimation non paramétrique de la loi de probabibilité sous-jacente."
  ) )


.IGoR$page$histogram$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"histogram")

  output$histogram.save.control <- renderUI(if (.isNotEmpty(input$histogram.Y)) .IGoR$save.ui("histogram"))

  output$histogram.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("histogram.X", label=.IGoR$NUMVAR1,
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
            column(width=6, uiOutput("histogram.X.label"))
          ),
          uiOutput("histogram.save.control")
        ),
        column(width=6,
          box(width='100%',
		        fluidRow(
			        column(width=6, radioButtons("histogram.bins.control","Découpage en tranches...",
										   c("de nombre donné"=1,"de taille fixe"=2))),
              column(width=6, uiOutput("histogram.bins"))
			      ),
			      fluidRow(
			        column(width=6, checkboxInput("histogram.kde","Superposer l'estimation non paramétrique de densité",TRUE)),
			        column(width=6, uiOutput("histogram.kde.bwm"))
			      ),
			      hr(),
			      tags$b("Ordonnées"),
			      fluidRow(
			        column(width=6),
			        tags$head(
			          tags$style(type="text/css", "#histogram_Y_label label{ display: table-cell; text-align: center; vertical-align: middle; } 
			                                       #histogram_Y_label .form-group { display: table-row;}")
			        ),
			        tags$div(id = "histogram_Y_label", textInput("histogram.Y.label","Titre :","density"))
  )   ) ) ) )
  
  .IGoR$gVarLabelUI(input,output,"histogram","X")
  
  output$histogram.bins <- renderUI(
    if (length(input$histogram.bins.control)>0)
      numericInput("histogram.bins","",if (input$histogram.bins.control==1) 25))
	  
  output$histogram.kde.bwm <- renderUI(
    if (.isTRUE(input$histogram.kde))
	  numericInput("histogram.kde.bwm","Multiplicateur de fenêtre d'estimation",1)
  )

  output$histogram.command2 <- renderUI(
    .IGoR$textarea("histogram", "gf_dhistogram(~x)", 3,
      if (.isNotEmpty(input$histogram.X)) {
        bins <- if (input$histogram.bins.control==1) "bins" else  "binwidth"  
        bins <- if ((length(input$histogram.bins)==0)
                  ||((input$histogram.bins.control==1)&&(input$histogram.bins==25))
                  ||((input$histogram.bins.control==2)&&is.na(input$histogram.bins))) ""
                else glue(", {bins}={input$histogram.bins}") 
        .IGoR$command2(
          glue("gf_dhistogram( ~ {input$histogram.X}{bins})"),
          if (input$histogram.kde) {
			      bwm <- if (.isEQ(input$histogram.kde.bwm,1)) "" else glue(", adjust={input$histogram.kde.bwm}")
			      paste0(" %>%\n   ",glue("gf_dens( ~ {input$histogram.X}{bwm})"))
			     },
 		       .IGoR$gTitleCmd(input,"histogram",X=TRUE,
 		         if (.isNE(input$histogram.Y.label,"density")) glue("y={shQuote(input$histogram.Y.label)}")),
		       .IGoR$gSaveCmd(input,"histogram")
		    )
      }
  ) )

}
