### 03/06/2019         Correction de l'affichage des modalités pour les chaînes de caractères
### 12/07/2019 1.02.0: Ajout d'une option de passage par summarise
### 09/08/2019 1.04.2: Externalisation des libellés en français
### 30/01/2020 1.06.0: Rétablissement de la prise en compte du tri des modalités

.IGoR$page$distinct$ui <- function()
  .IGoR$ui(page="distinct",
    fluidRow(
      column(width=6,
        box(width='100%',
          uiOutput("distinct.group")
      ) ),
      column(width=6,
        .IGoR$load.ui("distinct"),
        box(width='100%',
          fluidRow(
            column(width=6, radioButtons("distinct.type","", .IGoR$Znames("distinct","type",c("list","count")))),
            column(width=6, uiOutput("distinct.more"))
  ) ) ) ) )


.IGoR$page$distinct$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"distinct")
  
  output$distinct.more <- renderUI(
    if (.isEQ(input$distinct.type,"count")) 
      textInput("distinct.name",.IGoR$s2(.IGoR$Z$distinct$count.var),"n")
    else
    if (length(input$distinct.group)==1)
      checkboxInput("distinct.sort",.IGoR$s5(.IGoR$Z$distinct$sort),TRUE))
  
  output$distinct.group <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      selectizeInput("distinct.group", label=.IGoR$s1(.IGoR$Z$any$vars),
                     multiple = TRUE, options = list(placeholder = .IGoR$any$all),
                     choices = .columns(input$main.data)
    ))

  output$distinct.command2 <- renderUI(
   .IGoR$textarea("distinct", "distinct(columns)", 4,
      if (length(input$distinct.type)>0)
        .IGoR$command2(
          if (input$distinct.type=="count")
            if (.isNE(input$distinct.name,"n"))
              paste0(
                .IGoR$group_by(input,"distinct"),
                glue("summarise({input$distinct.name}=n())"),
                .IGoR$ungroup(input,"distinct",1)
              )
            else glue("count({.collapse(input$distinct.group)})")
          else glue("distinct({.collapse(input$distinct.group)})"),
          if ((input$distinct.type=="list")&&(length(input$distinct.group)==1)&&.isTRUE(input$distinct.sort))
            paste0(NL,glue("arrange({.collapse(input$distinct.group)})"))
  ) )   )
  
  observeEvent(input$distinct.command2,
    .IGoR$try(input,output,"distinct",
      .fn=function(x) 
        if (input$distinct.type=="count")
          sprintf(.IGoR$Z$distinct$msg.result,nrow(x))
        else
          if (ncol(x)==1)
               sprintf(.IGoR$Z$distinct$msg.values,nrow(x),
                       (if (is.character(x[[1]])||is.factor(x[[1]])) .collapse1 else .collapse)(x[[1]]))
          else sprintf(.IGoR$Z$distinct$msg.count,nrow(x)),
      .subset=glue("select({.collapse(input$distinct.group)})")
  ))

}