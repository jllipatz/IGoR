
### 14/06/2019 1.01.1: Ajout du libellé de variable
### 06/08/2019 1.04.0: dropdown buttons
### 11/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$histogram$ui <- function() .IGoR$ui(page="histogram", graphics=TRUE)


.IGoR$page$histogram$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"histogram")

  output$histogram.save.control <- renderUI(if (.isNotEmpty(input$histogram.X)) .IGoR$save.ui("histogram"))

  output$histogram.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            column(width=6, selectizeInput("histogram.X", .IGoR$s1(.IGoR$Z$any$var.quan), choices=.numeric(input))),
            column(width=6, uiOutput("histogram.X.label"))
        ) ),
        column(width=6, uiOutput("histogram.save.control"))
  )  )
  
  output$histogram.dropdown <- renderUI(
    .IGoR$dropdownButton(page="histogram",
      fluidRow(
			  column(width=6, radioButtons("histogram.bins.type",.IGoR$s2(.IGoR$Z$histogram$bins.type),.IGoR$Znames("histogram","bins.type",c("bins","binwidth")))),
        column(width=6, uiOutput("histogram.bins"))
			),
			fluidRow(
			  column(width=6, checkboxInput("histogram.kde",.IGoR$s4(.IGoR$Z$histogram$kde),FALSE)),
			  column(width=6, uiOutput("histogram.kde.bwm"))
			),
			.IGoR$hr(),
			tags$b(.IGoR$Z$any$y),
			fluidRow(
			  column(width=6), .IGoR$label.ui("histogram","Y","density"))
  )   )
  
  .IGoR$gVarLabelUI(input,output,"histogram","X")
  
  output$histogram.bins <- renderUI(
    if (length(input$histogram.bins.type)>0)
      numericInput("histogram.bins","",if (input$histogram.bins.type=="bins") 25))
	  
  output$histogram.kde.bwm <- renderUI(
    if (.isTRUE(input$histogram.kde))
	    numericInput("histogram.kde.bwm",.IGoR$s2(.IGoR$Z$histogram$kde.bwm),1)
  )

  output$histogram.command2 <- renderUI(
    .IGoR$textarea("histogram", "gf_dhistogram(~x)", 3,
      if (.isNotEmpty(input$histogram.X)) {
        bins <- if ((length(input$histogram.bins)==0)
                  ||(length(input$histogram.bins.type)==0)
                  ||((input$histogram.bins.type=="bins")&&(input$histogram.bins==25))
                  ||((input$histogram.bins.type=="binwidth")&&is.na(input$histogram.bins))) ""
                else glue(", {input$histogram.bins.type}={input$histogram.bins}") 
        .IGoR$command2(
          glue("gf_dhistogram( ~ {input$histogram.X}{bins})"),
          if (.isTRUE(input$histogram.kde)) {
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
