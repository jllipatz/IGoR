### 2020/04/04
###   IGoR.json, init.R updated

.IGoR$page$line$ui <- function() .IGoR$ui(page="line", graphics=TRUE)

.IGoR$page$line$sv <- function(input, output, session) {

  .IGoR$gServer(input,output,"line")

  output$line.save.control <- renderUI(if (.isNotEmpty(input$line.Y)&&.isNotEmpty(input$line.X)) .IGoR$save.ui("line"))

  output$line.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("line.X", .IGoR$s1(.IGoR$Z$any$var.quan.x), .numeric(input))),
              column(width=6, uiOutput("line.X.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("line.Y", .IGoR$s1(.IGoR$Z$any$var.quan.y), .numeric(input))),
              column(width=6, uiOutput("line.Y.label"))
            ),
            fluidRow(
              column(width=6, selectizeInput("line.group", .IGoR$s1(.IGoR$Z$line$group), .discrete(input))),
              column(width=6, uiOutput("line.group.label"))
        ) ) ),
        column(width=6,
          box(width='100%',
            column(width=6, checkboxInput("line.loess",.IGoR$s2(.IGoR$Z$line$loess),FALSE)),
            column(width=6, uiOutput("line.loess"))
          ),
          uiOutput("line.save.control")
  )   ) )

  .IGoR$gVarLabelUI(input,output,"line","X")

  .IGoR$gVarLabelUI(input,output,"line","Y")

  .IGoR$gVarLabelUI(input,output,"line","group")

  output$line.loess <- renderUI(
    if (.isTRUE(input$line.loess)) sliderInput("line.loess.span", "",.5,2,.75,step=.25)
  )

  output$line.command2 <- renderUI(
    .IGoR$textarea("line", "gf_line(y ~ x)", 6,
      if (.isNotEmpty(input$line.Y)&&.isNotEmpty(input$line.X)) {
        color <- if (.isNotEmpty(input$line.group)) glue(", color=~{input$line.group}") else ""
        pronoun <- if (.isNotEmpty(input$line.group)) "data" else ""
        if (.isTRUE(input$line.loess)) {
          span <- if (.inOrNULL(input$line.loess.span,.75)) ""
                  else glue(", span={input$line.loess.span}")
          x <- glue(".{pronoun}${input$line.X}")
          df <- get(input$main.data,envir=.GlobalEnv)
          if (is.Date(df[[input$line.X]])) x <- glue("as.numeric({x})")
        }
        .IGoR$command2(
          if (.isFALSE(input$line.loess))
               glue ("gf_line({input$line.Y} ~ {input$line.X}{color})")
          else paste0(
                 .IGoR$group_by(input,"line"),
                 glue("mutate(..y=loess(.{pronoun}${input$line.Y} ~ {x}{span})$fitted)"),
                 .IGoR$ungroup(input,"line"),NL,
                 glue("gf_line(..y ~ {input$line.X}{color}, linetype = 'dashed')"),NL,
                 glue("gf_point({input$line.Y} ~ {input$line.X}{color})")
               ),
          .IGoR$gTitleCmd(input,"line",X=TRUE,Y=TRUE,
            c(.IGoR$gLabel.arg(input,"line","group","color")
          ) ),
          .IGoR$gSaveCmd(input,"line")
        )}
    ) )

}



