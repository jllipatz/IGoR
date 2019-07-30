
.IGoR$page$slice$ui <- function()
  div(id = "bloc_slice",
    fluidRow(
      column(width=4, 
        img(src="images/slice.png", height = "46px"),
        h3(span("Sélectionner une plage d'observations", style="color: blue"))
      ),
      column(width=8, 
        p("Les fonctions ", code("head"), " et ", code("tail"), " construisent une nouvelle table limitée aux premières ou aux dernières observations.",
          "Elle sont complétées par la fonction ", code("slice"), " du package ", strong("dplyr"), " qui permet d'extraire une plage d'observations en milieu de table, ",
          "ou, au contraire de conserver uniquement les observations qui n'appartiennent pas à la plage spécifiée.",br(),
          "L'usage de ce type de sélection d'observations est généralement lié à un besoin d'extraire un jeu de test limité."
    ) ) ),
    fluidRow(
      column(width=8,
        box(width='100%',
          fluidRow(
            column(width=6, numericInput("slice.top","Numéro de la première observation",1)),
            column(width=6, uiOutput("slice.end"))
          ),
          checkboxInput("slice.drop","Inverser la sélection",FALSE)
      )),
      column(width=4, .IGoR$loadBox("slice","slice.out"))
    ),
    .IGoR$commandBox("slice")
  )


.IGoR$page$slice$sv <- function(input, output, session) {
  
  .IGoR$aaServer(input,output,"slice")
  
  output$slice.end <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      numericInput("slice.end","Numéro de la dernière observation",nrow(get(input$main.data,envir=.GlobalEnv)))
  )
  
  output$slice.command2 <- renderUI(
    .IGoR$textarea("slice", "slice(range)", 3, 
      if (length(input$slice.end)>0) {
        n <- nrow(get(input$main.data,envir=.GlobalEnv))
        .IGoR$command2(
          if (is.na(input$slice.end)||is.na(input$slice.top)) ""
          else glue(
               if (input$slice.top>1)
                 if (input$slice.end<n)
                   if (input$slice.top!=input$slice.end) 
                     if (input$slice.drop)
                          "slice(-({input$slice.top}:{input$slice.end}))"
                     else "slice({input$slice.top}:{input$slice.end})"
                   else if (input$slice.drop)
                          "slice(-{input$slice.top})"
                     else "slice({input$slice.top})"
                 else
                   if (input$slice.drop)
                        "head({input$slice.top-1})"
                   else "tail({n-input$slice.top+1})"
               else     
                 if (input$slice.drop)
                      "tail({n-input$slice.end})"
                 else "head({input$slice.end})"
          )    )
      } )
  )

}
        