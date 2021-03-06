
### 01/08/2019 1.03.2
### 06/08/2019 1.03.3: Suppression de la palette manuelle
### 06/08/2019 1.04.0: dropdown buttons
### 11/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$spplot$ui <- function() .IGoR$ui(page="spplot", graphics=TRUE, subtitle=FALSE)


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
        column(width=4,
          box(width='100%',
            selectizeInput("spplot.Z", .IGoR$s1(.IGoR$Z$any$var.quan),.numeric(input)),
            selectizeInput("spplot.zone", .IGoR$s1(.IGoR$Z$spplot$id), choices=c(.IGoR$CHRCOLV,.columns(input$main.data,"character")))
        ) ),
        column(width=8,
          box(width='100%',
            fluidRow(
              column(width=6,
                selectizeInput("spplot.sp", .IGoR$s1(.IGoR$Z$spplot$map), choices=c("<SpatialPolygonsDataFrame>"="",list.sp())),
                uiOutput("spplot.sp.zone")
              ),
              column(width=6,
                uiOutput("spplot.sp.zone2"),
                uiOutput("spplot.sp.zone2.ids")
  )   ) ) ) ) )
  
  output$spplot.sp.zone <- renderUI(
    if (.isNotEmpty(input$spplot.sp))
      selectizeInput("spplot.sp.zone", .IGoR$s1(.IGoR$Z$spplot$id), choices=c(.IGoR$COLV,list.sp.columns(input$spplot.sp,TRUE)))
  )
  
  output$spplot.sp.zone2 <- renderUI(
    if (.isNotEmpty(input$spplot.sp))
      selectizeInput("spplot.sp.zone2",.IGoR$s3(.IGoR$Z$spplot$where),
                     choices=c('' %>% {names(.)<- .IGoR$Z$spplot$zone;.},list.sp.columns(input$spplot.sp,FALSE)))
              
  )
  
  output$spplot.sp.zone2.ids <- renderUI(
    if (.isNotEmpty(input$spplot.sp)&&.isNotEmpty(input$spplot.sp.zone2)) {
      l <- list.sp.column.ids(input$spplot.sp,input$spplot.sp.zone2)
      selectizeInput("spplot.sp.zone2.ids",.IGoR$s3(.IGoR$Z$spplot$codes), 
                     multiple = TRUE, options=list(placeholder = glue("<({length(l)})>")), choices = l)
    }
  )
  
  output$spplot.dropdown <- renderUI(
    .IGoR$dropdownButton(page="spplot",
      fluidRow(
        column(width=6, selectizeInput("spplot.sp2",.IGoR$s3(.IGoR$Z$spplot$map2), choices=c("<SpatialPolygonsDataFrame>"="",list.sp())))
      ),
      .IGoR$hr(),
      fluidRow(
        column(width=6, checkboxInput("spplot.labels",.IGoR$s4(.IGoR$Z$spplot$labels),FALSE),
                        checkboxInput("spplot.colors",.IGoR$s4(.IGoR$Z$spplot$palette), FALSE)),
        column(width=6, selectizeInput("spplot.fill",.IGoR$s2(.IGoR$Z$spplot$fill.none), choices=c(.IGoR$Z$any$NONE,.IGoR$COLORS), selected='black'))
      ),
      uiOutput("spplot.colors")
  ) )

  output$spplot.colors <- renderUI(
    if (.isTRUE(input$spplot.colors))
      fluidRow(
        column(width=6, selectizeInput("spplot.fill.start",.IGoR$s2(.IGoR$Z$spplot$fill.start), choices=.IGoR$COLORS, selected='yellow')),
        column(width=6, selectizeInput("spplot.fill.end",  .IGoR$s2(.IGoR$Z$spplot$fill.end),   choices=.IGoR$COLORS, selected='red'))
      )   )
  
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
        main <- if (!.isNotEmpty(input$spplot.title)) ""
                else paste0(", main=",shQuote(input$spplot.title))
        sub <- if (!.isNotEmpty(input$spplot.source)) ""
               else paste0(", sub=",shQuote(paste0("Source : ",input$spplot.source)))
        labels <- if (!.isTRUE(input$spplot.labels)) ""
                  else paste0(',\n         ',glue("sp.layout=make.labels(tmp.sp,'{input$spplot.sp.zone}')"))
        colors <- if (!.isTRUE(input$spplot.colors)) ""
                  else paste0(',\n         ',glue("col.regions=colorRampPalette(c('{input$spplot.fill.start}','{input$spplot.fill.end}'))(100)"))
        if (.isNotEmpty(input$spplot.sp.zone2)&&.isNotEmpty(input$spplot.sp.zone2.ids)) sp <- eval(parse(text=s),envir=.GlobalEnv)
        if (length(unique(df[[input$spplot.zone]]))!=nrow(df))
          {output$spplot.comment <- renderText(.IGoR$Z$spplot$msg.duplicated); NULL}
        else
        if (length(intersect(df[[input$spplot.zone]],sp@data[[input$spplot.sp.zone]]))==0)
          {output$spplot.comment <- renderText(.IGoR$Z$ssplot$msg.notfound); NULL}
        else
          .IGoR$command2(
            "{",'\n     ',
            if (.isTRUE(input$spplot.labels))
              paste0(
                glue("make.labels <- function(x,y) do.call(list, list('sp.text', coordinates(x), x@data[[y]]))"),'\n     '
              ),
            paste0("tmp.sp <- ",
              if (.isNotEmpty(input$spplot.sp.zone2)&&.isNotEmpty(input$spplot.sp.zone2.ids)) s else input$spplot.sp,
              '\n     '
            ),
            glue("idx <- match(tmp.sp@data${input$spplot.sp.zone}, .${input$spplot.zone})"),'\n     ',
            glue("tmp.sp@data$tmp <- .${input$spplot.Z}[idx]"),'\n     ',
            "tmp.sp %>%\n       ",
            glue("spplot('tmp'{col}{main}{sub}{colors}{labels})"),
            if (.isNotEmpty(input$spplot.fill))
              paste0(' +\n     ',glue("  layer_(sp.polygons({input$spplot.sp}, fill='{input$spplot.fill}', col=NA))")),
            add,
            '\n   }'
          )
      }
  ) ) 

}

