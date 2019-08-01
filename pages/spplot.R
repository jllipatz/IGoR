
.IGoR$page$spplot$ui <- function()
  .IGoR$gUI("spplot","Expérimentations cartographiques",
    p("La fonction", code("spplot"), "du package", strong("sp"), "produit des cartographie thématiques simples.", br(),
      "Les fonds de carte nécessaires peuvent être restaurés à partir du fichier", em("data/geo_GADMxxxx.RData"), ".",br(), 
      em("NOTE : Les fonctionnalités présentées ici ne le sont qu'à titre démonstratif.")
) )


.IGoR$page$spplot$sv <- function(input, output, session) {
  
  .IGoR$gServer(input,output,"spplot")

  list.sp <- function () {
    v <- unlist(Map(function(x) if (is(get(x,envir=.GlobalEnv),"SpatialPolygonsDataFrame")) x,ls(envir=.GlobalEnv)))
    if (!is.null(v)) v[order(unlist(Map(function(x) nrow(get(x,envir=.GlobalEnv)@data), v)))]
  }
  
  list.sp.column.ids <- function (.sp, .zone) {
    sp <- get(.sp,envir=.GlobalEnv)
    v <- sort(unique(sp@data[[.zone]]))
    names(v) <- v
    v
  }
  
  list.sp.columns <- function (.sp,.id) {
    sp <- get(.sp,envir=.GlobalEnv)
    c <- names(sp)
    c <- c[map_lgl(c, function(x) is.character(sp@data[[x]]))]
    c <- if (.id) 
         c[map_lgl(c, function(x) length(unique(sp@data[[x]]))==nrow(sp@data))]
    else c[map_lgl(c, function(x) length(unique(sp@data[[x]]))>1)]
    names(c) <- Map(function (x) {
                      i <- list.sp.column.ids(.sp,x)
                      if (length(i)==1)
                           paste0(x," (",i[1],")")
                      else paste0(x," (",.collapse1(i[1:2]),if (length(i)>2) ",...",")")},
                    c)
    c
  }

  output$spplot.control<- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=8,
          box(width='100%',
            column(width=6, selectizeInput("spplot.Z", label=.IGoR$NUMVAR1,
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer"))))),
            column(width=6, selectizeInput("spplot.zone", "Identifiant de zone",
                                           choices=c(.IGoR$CHRCOLV,.columns(input$main.data,"character"))))
          ),
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("spplot.sp", "Fond  de carte",
                                             choices=c("<SpatialPolygonsDataFrame>"="",list.sp()))),
              column(width=6, uiOutput("spplot.sp.zone"))
            ),
            fluidRow(
              column(width=6, uiOutput("spplot.sp.zone2")),
              column(width=6, uiOutput("spplot.sp.zone2.ids")) 
            ),
            fluidRow(
             column(width=6, selectizeInput("spplot.sp2","Superposer le fond de carte",
                                            choices=c("<SpatialPolygonsDataFrame>"="",list.sp())))
          ) )
        ),
        column(width=4,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("spplot.fill","Couleur des zones sans donnée",
                                             choices=c("<aucune>"="<none>",.IGoR$COLORS), selected='black'))
            ),
            fluidRow(
              column(width=6, selectizeInput("spplot.fill.start","Couleur de départ", choices=.IGoR$COLORS, selected='yellow')),
              column(width=6, selectizeInput("spplot.fill.end",  "Couleur darrivée",  choices=.IGoR$COLORS, selected='red'))
  )   ) ) ) )
  
  output$spplot.sp.zone <- renderUI(
    if (.isNotEmpty(input$spplot.sp))
      selectizeInput("spplot.sp.zone","Identifiant de zone",
                     choices=c("<colonne>"="",list.sp.columns(input$spplot.sp,TRUE)))
  )
  
  output$spplot.sp.zone2 <- renderUI(
    if (.isNotEmpty(input$spplot.sp))
      selectizeInput("spplot.sp.zone2","Restreindre aux zones...",
                     choices=c("<zonage>"="",list.sp.columns(input$spplot.sp,FALSE)))
              
  )
  
  output$spplot.sp.zone2.ids <- renderUI(
    if (.isNotEmpty(input$spplot.sp)&&.isNotEmpty(input$spplot.sp.zone2)) {
      l <- list.sp.column.ids(input$spplot.sp,input$spplot.sp.zone2)
      selectizeInput("spplot.sp.zone2.ids","...de codes", 
                     multiple = TRUE, options=list(placeholder = glue("<codes ({length(l)})>")), choices = l)
    }
  )
  
  output$spplot.command2 <- renderUI(
    .IGoR$textarea("spplot", "spplot(...)", 8,
      if (.isNotEmpty(input$spplot.Z)&&.isNotEmpty(input$spplot.zone)
        &&.isNotEmpty(input$spplot.sp)&&.isNotEmpty(input$spplot.sp.zone)) {
        s <- glue("subset({input$spplot.sp},{input$spplot.sp.zone2} %in% {.collapse2(input$spplot.sp.zone2.ids)})")
        df <- get(input$main.data,envir=.GlobalEnv)
        sp <- get(input$spplot.sp,envir=.GlobalEnv)
        col <- ", col=NA"
        add <- if (.isNotEmpty(input$spplot.sp2))
                 if (input$spplot.sp2==input$spplot.sp) col <- ""
                 else paste0(" +\n       ",glue("layer(sp.polygons({input$spplot.sp2}))"))
               else ""
        if (.isNotEmpty(input$spplot.sp.zone2)&&.isNotEmpty(input$spplot.sp.zone2.ids)) sp <- eval(parse(text=s),envir=.GlobalEnv)
        if (length(unique(df[[input$spplot.zone]]))!=nrow(df))
          {output$spplot.comment <- renderText("ERREUR : Il y a plusieurs observations pour une même zone!"); NULL}
        else
        if (length(intersect(df[[input$spplot.zone]],sp@data[[input$spplot.sp.zone]]))==0)
          {output$spplot.comment <- renderText("ERREUR : Aucune observation n'a d'écho dans le fond de carte!"); NULL}
        else
          .IGoR$command2(
            "{",'\n     ',
            paste0("tmp.sp <- ",
              if (.isNotEmpty(input$spplot.sp.zone2)&&.isNotEmpty(input$spplot.sp.zone2.ids)) s else input$spplot.sp,
              '\n     '
            ),
            glue("idx <- match(tmp.sp@data${input$spplot.sp.zone}, .${input$spplot.zone})"),'\n     ',
            glue("tmp.sp@data$tmp <- .${input$spplot.Z}[idx]"),'\n     ',
            "tmp.sp %>%\n       ",
            glue("spplot('tmp', col.regions=colorRampPalette(c('{input$spplot.fill.start}','{input$spplot.fill.end}'))(100){col})"),
            if (.isNE(input$spplot.fill,"<none>"))
              paste0(' +\n     ',glue("  layer_(sp.polygons({input$spplot.sp}, fill='{input$spplot.fill}', col=NA))")),
            add,
            '\n   }'
          )
      }
  ) ) 

}

