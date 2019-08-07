
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
    ),
    dropdown=TRUE
  )


.IGoR$page$points$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"points")
  
  output$points.save.control <- renderUI(if (.isNotEmpty(input$points.Y)&&.isNotEmpty(input$points.X)) .IGoR$save.ui("points"))

  output$points.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("points.X", label=.IGoR$s1(.IGoR$NUMVARX1),
                                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("points.X.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("points.Y", label=.IGoR$s1(.IGoR$NUMVARY1), 
                                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))),
              column(width=6, uiOutput("points.Y.label"))
        ) ) ),
        column(width=6,
          box(width='100%',
            radioButtons("points.type",NULL,
                         c("Points seuls"=1, "Relier les points"=2, "Regression linéaire"=3))),
           uiOutput("points.save.control")
  )   ) )
        
  output$points.dropdown <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      .IGoR$dropdownButton(page="points",
        fluidRow(
          column(width=6, radioButtons("points.size.type", .IGoR$s2("Taille des points"),
                                       c("En fonction de la variable..."=1, "Uniforme..."=2))),
          column(width=6, uiOutput("points.size"))
        ),
        fluidRow(
          column(width=6, radioButtons("points.color.type", .IGoR$s2("Couleur des points"),
                                       c("En fonction de la variable..."=1, "Uniforme..."=2))),
          column(width=6, uiOutput("points.color"))
        ),
        uiOutput("points.shape"),
        .IGoR$hr(),
        strong("Abscisses"),
        fluidRow(
          column(width=6, uiOutput("points.scale"))
        )
  )   )
  
  .IGoR$gVarLabelUI(input,output,"points","X")
  
  .IGoR$gVarLabelUI(input,output,"points","Y")
  
  output$points.scale <- renderUI(
    if (.IGoR$test$meta)
      numericInput("points.scale",.IGoR$s3("Intervalle des graduations"),NA)
  )
  
  output$points.size <- renderUI(
    if (.IGoR$test$meta&&(length(input$points.size.type)>0))
      if (input$points.size.type==1)
        selectizeInput("points.size.column", .IGoR$s3(.IGoR$NUMVAR1), 
                       choices=c("(aucune)",.columns(input$main.data,"numeric")))
      else
        sliderInput("points.size.value", "", 1,10,1)
  )
  
  output$points.shape <- renderUI(
    if (.IGoR$test$meta&&.isNE(input$points.type,2))
      selectizeInput("points.shape", .IGoR$s3("Forme des points en fonction de la variable qualitative"), 
                     choices=c("(aucune)",.columns(input$main.data,c("factor","character"))))
  )
  
  output$points.color <- renderUI(
    if (.IGoR$test$meta&&(length(input$points.color.type)>0))
      if (input$points.color.type==1)
           selectizeInput("points.color.column", .IGoR$s3(.IGoR$QALVAR1),
                          choices=c("(aucune)",.columns(input$main.data,c("factor","character"))))
      else selectizeInput("points.color.value", "", choices=.IGoR$COLORS)
  )
  
  output$points.command2 <- renderUI(
    .IGoR$textarea("points", "gf_point(y~x)", 4,
      if ((length(input$points.type)>0)
        &&(.isNotEmpty(input$points.Y)&&.isNotEmpty(input$points.X))) {
      color <- if (length(input$points.color.type)==0) ""
               else 
                 if (input$points.color.type==1)
                   if (.inOrNULL(input$points.color.column,"(aucune)")) ""
                   else glue(", color=~{input$points.color.column}")
                 else
                   if (!.isNE(input$points.color.value,'black')) "" 
                   else glue(", color='{input$points.color.value}'")
      size <- if (length(input$points.size.type)==0) ""
              else 
                if (input$points.size.type==1)
                  if (.inOrNULL(input$points.size.column,"(aucune)")) ""
                  else glue(", size=~{input$points.size.column}")
                else
                  if (!.isNE(input$points.size.value,1)) ""
                  else glue(", size={input$points.size.value}")
      shape <- if (.inOrNULL(input$points.shape,"(aucune)")||(input$points.type==2)) ""
               else glue(", shape=~{input$points.shape}")
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
