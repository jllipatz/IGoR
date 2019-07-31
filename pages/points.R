
### 27/07/2019 1.03.0: Ajout du paramétrage des graduations horizontales

.IGoR$page$points$ui <- function()
  .IGoR$gUI("points","Nuage de points",
    p("La fonction", code("gf_points"), "du package ", strong("ggformula"), "produit des graphiques de",
      span("nuages de points", style="color:blue"), " construits sur deux variables quantitatives. Elle est complétée par la fonction ", code("gf_line"),
      "qui en reliant les points entre eux dessine des", span("courbes", style="color:blue"), ".", br(),
      "La taille des points peut être conditionnée par une autre variable quantitative. ",
      "La couleur des points peut également être dépendante d'une autre variable qualitative. ",
      "Dans le cas de tracé de courbes, un tel paramétrage permet de superposer les courbes obtenues pour chacune des modalités de la variable supplémentaire, ",
      "celle ci agissant comme dans un", code("group_by"), "de", strong("dplyr"), "."
  ) )


.IGoR$page$points$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"points")
  
  output$points.save.control <- renderUI(if (.isNotEmpty(input$points.Y)&&.isNotEmpty(input$points.X)) .IGoR$save.ui("points"))

  output$points.color <- renderUI(
    if (.IGoR$test$meta&&(length(input$points.color.control)>0))
      selectizeInput("points.color",
                     if  (input$points.color.control==1) .IGoR$QALVAR else "", 
                     choices=
                       if (input$points.color.control==1)
                         c("(aucune)",.columns(input$main.data,c("factor","character")))
                     else .IGoR$COLORS
      ))

  output$points.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("points.X", label="Variable quantitative en abscisse (*)",
                                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("points.X.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("points.Y", label=.IGoR$NUMVARY1, 
                                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("points.Y.label"))
          ) ),
          uiOutput("points.save.control")
        ),
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, radioButtons("points.type","",
                                          c("Points seuls"=1,
                                            "Relier les points"=2,
                                            "Regression linéaire"=3))),
              column(width=6, uiOutput("points.scale"))
            ),
            hr(),
            fluidRow(
              column(width=6, radioButtons("points.size.control", "Taille des points",
                                          c("En fonction de la variable..."=1,
                                            "Uniforme..."=2))),
              column(width=6, uiOutput("points.size"))
              ),
            fluidRow(
              column(width=6, radioButtons("points.color.control", "Couleur des points",
                                           c("En fonction de la variable..."=1,
                                             "Uniforme..."=2))),
              column(width=6, uiOutput("points.color"))
            ),
            uiOutput("points.shape")          
        ))
  ))
  
  .IGoR$gVarLabelUI(input,output,"points","X")
  
  .IGoR$gVarLabelUI(input,output,"points","Y")
  
  output$points.scale <- renderUI(
    if (.IGoR$test$meta)
      numericInput("points.scale","Intervalle des graduations",NA)
  )
  
  output$points.size <- renderUI(
    if (.IGoR$test$meta&&(length(input$points.size.control)>0))
      if (input$points.size.control==1)
        selectizeInput("points.size.column", .IGoR$NUMERIC, 
                       choices=c("(aucune)",.columns(input$main.data,"numeric")))
      else
        sliderInput("points.size.value", "", 1,10,1)
  )
  
  output$points.shape <- renderUI(
    if (.IGoR$test$meta&&(isFALSE(input$points.line)))
      selectizeInput("points.shape", "Forme des points en fonction de la variable qualitative", 
                     choices=c("(aucune)",.columns(input$main.data,c("factor","character"))))
  )

  output$points.command2 <- renderUI(
    .IGoR$textarea("points", "gf_point(y~x)", 4,
      if ((length(input$points.type)>0)&&(.isNotEmpty(input$points.Y)&&.isNotEmpty(input$points.X))) {
      color <- if (.inOrNULL(input$points.color,c("(aucune)","black"))) ""
               else 
                 if (input$points.color.control==1)
                    glue(", color=~{input$points.color}")
               else glue(", color='{input$points.color}'")
      shape <- if (.inOrNULL(input$points.shape,"(aucune)")||(input$points.type==2)) ""
               else glue(", shape=~{input$points.shape}")
      size <- if ((length(input$points.size.column)==0)&&(length(input$points.size.value)==0)) ""
              else 
                if (input$points.size.control==1)
                  if (input$points.size.column=="(aucune)") ""
                  else glue(", size=~{input$points.size.column}")
                else
                  if (input$points.size.value==1) ""
                  else glue(", size={input$points.size.value}")
      type <- if (input$points.type==2) "line" else "point"
      .IGoR$command2(
        if (input$points.type==3)
          paste0(glue("gf_lm({input$points.Y} ~ {input$points.X}, interval='confidence')"),NL),
        glue ("gf_{type}({input$points.Y} ~ {input$points.X}{color}{shape}{size})"),
        if (.isNotNA(input$points.scale)) {
          scale <- round(input$points.scale)
          x <- get(input$main.data,envir=.GlobalEnv)[[input$points.X]]
          x1 <- (min(x,na.rm=TRUE) %/% scale)*scale
          x2 <- max(x,na.rm=TRUE)
          paste0(NL,glue("gf_refine(scale_x_continuous(breaks=seq({x1},{x2},{scale})))")) 
        },
 		    .IGoR$gTitleCmd(input,"points",X=TRUE,Y=TRUE),
		    .IGoR$gSaveCmd(input,"points")
      )}
  ) )

}