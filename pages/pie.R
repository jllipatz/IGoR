### 14/06/2019 1.01.1: Ajout d'une pondération optionnelle

.IGoR$page$pie$ui <- function()
  .IGoR$gUI("pie","Diagramme en camembert",
    p("Il n'existe pas de fonction", strong("ggformula"), "ou", strong("ggplot2"), "pour représenter la",
      span("répartition d'une variable qualitative", style='color:blue'), "sous forme de camembert.",
      "La représentation présentée ici exploite les possibilités d'accès aux fonctions de", strong("ggplot2"),
      "en partant d'un graphique en barres proportionnelles aux effectifs et le transforme par changement du système de coorodonnées."
  ) )


.IGoR$page$pie$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"pie")
  
  output$pie.save.control <- renderUI(if (.isNotEmpty(input$pie.X)) .IGoR$save.ui("pie"))
  
  output$pie.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("pie.Y", label=.IGoR$QALVAR1,
                                             choices=c(.IGoR$QALCOLV,.columns(input$main.data,c("factor","character","integer"))))),
              column(width=6, uiOutput("pie.Y.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("pie.W", label=.IGoR$WEIGHT,
                                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer"))))),
              column(width=6, selectizeInput("pie.X", label=.IGoR$GROUP,
                           choices=c(.IGoR$QALCOLV,.columns(input$main.data,c("factor","character","integer")))))
          )),               
          uiOutput("pie.save.control")
        )
      ))
  
  .IGoR$gVarLabelUI(input,output,"pie","Y")
  
  output$pie.command2 <- renderUI(
    .IGoR$textarea("pie", "gf_bar(~1, fill=~x) + coord_polar('y')", 4,
      if (.isNotEmpty(input$pie.Y)) {
        x <- if (.isNotEmpty(input$pie.X)) glue("| {input$pie.X}, position=position_fill()") else ""
        w <- if (.isNotEmpty(input$pie.W)) input$pie.W else ""
        f <- if (.isNotEmpty(input$pie.W)) "col" else "bar"
        .IGoR$command2(
          glue("gf_{f}({w} ~ 1 {x}, fill=~{input$pie.Y})"),NL,
          "gf_refine(coord_polar('y'))",NL,
          "gf_theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title.y=element_blank())",
          .IGoR$gTitleCmd(input,"pie",
            c("y=''",
              if (.isNE(input$pie.Y.label,input$pie.Y)) glue("fill={shQuote(input$pie.Y.label)}"))
          ),
          .IGoR$gSaveCmd(input,"pie")
        )
      }
  ) )
  
}
