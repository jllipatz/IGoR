
.IGoR$page$contents$ui <- function()
  div(id = "bloc_contents", 
    fluidRow(
      column(width=4, 
        img(src="images/table.png", height = "46px"),
        h3(span("Variables de la table courante", style="color: blue"))
      ),
      column(width=8, 
        p("En R, une", strong("table de données")," est une structure rectangulaire constituée de :", br(),
          "- ", strong("lignes"), ": une pour chaque entité observée,", br(),
          "- ", strong("colonnes"), ": une pour chaque variable mesurée sur les observations.",br(),
          "Chaque colonne est caractérisée par son ", em("nom"), ", son type de données (", em("classe"),
          ") et peut porter des informations complémentaires.",
          "La classe d'une variable conditionne les usages qui pourront être faits de ses données.",
          "Le ", em("label"), "est utilisé pour enrichir les graphiques."
      ) )
    ),
    tableOutput("contents")
  )


.IGoR$page$contents$sv <- function(input, output, session) {

  output$contents <- renderTable(
    if ((length(input$main.data)>0)&&.IGoR$test$meta) {
      df <- get(input$main.data,envir=.GlobalEnv)
      f <- Vectorize(function(nom) attr(df[[nom]],"label") %>% ifelse(is.null(.),NA,.))
      data.frame(Nom=colnames(df),
                 Classe=unlist(Map(class,df), use.names=FALSE),
                 Label=f(colnames(df))
      )
    }
  )

  
}

