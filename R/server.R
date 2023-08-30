#' this is called by search_gwascat, also symlinked to inst/app2 for shinyapps usage
#' @param input formal element for shiny server component
#' @param output formal element for shiny server component
#' @param session formal element for shiny server component
#' @note Not exported.
server <- function(input, output, session) {
  data("efo_tc", package = "gwasCatSearch")
  data("efo_df", package = "gwasCatSearch")
  ntab <- reactive({
   input$submit
   isolate({
    sout <- corpustools::search_features(tc = efo_tc, query = input$query)
    validate(need(nrow(sout$hits) > 0, "no hits, please try a different query"))
    tab <- gwasCatSearch::hits2DT(sout, efo_df, efo_tc)
    tab
    })
  })
  output$hits <- DT::renderDataTable(
    ntab(),
    escape = FALSE, rownames=FALSE
    #   ntab(), escape=FALSE,
    #                options = list(dom = 'Bfrtip',
    #               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )


  process_annotated <- reactive({
    tab <- ntab()
    rn = rownames(tab)
    sels = input$hits_rows_selected
    if (!is.null(sels)) rn = rn[sels]
    u <- unique(rn)
    last <- gwasCatSearch::resources_annotated_with_term(u,
      include_subclasses = isTRUE("include subclasses" %in% input$inclsub),
      direct_subclasses_only = isTRUE("direct subclss only" %in% input$inclsub)
    )
    dups <- which(duplicated(last$STUDY.ACCESSION))
    if (length(dups) > 0) last <- last[-dups, ]
    shinyjs::hide("notif")
    last$PUBMEDID <- sprintf(
      "<A href='https://pubmed.ncbi.nlm.nih.gov/%s/'>%s</A>", last$PUBMEDID,
      last$PUBMEDID
    )
    last$MAPPED_TRAIT_URI <- sprintf("<A href='%s'>%s</A>", last$MAPPED_TRAIT_URI, last$MAPPED_TRAIT_URI)
    last$accstr = last$STUDY.ACCESSION
    last$STUDY.ACCESSION <- sprintf(
      "<A href='https://www.ebi.ac.uk/gwas/studies/%s' target='_blank'>%s</A>",
      last$STUDY.ACCESSION, last$STUDY.ACCESSION
    )
    last
  })
  output$resources <- DT::renderDataTable({
    last <- process_annotated()
    DT::datatable(last, escape = FALSE)
  })
  output$ontoviz <- renderPlot({
    validate(need(input$graphicson == TRUE, "must enable graphics on sidebar"))
    if (!exists("efo")) efo <<- ontoProc::getOnto("efoOnto")
    validate(need(!is.null(input$gbuttons), "Waiting for gbutton UI"))
    last <- process_annotated()
    validate(need(length(input$gbuttons)>1, "only one term present, nothing to plot"))
    ontoProc::onto_plot2(efo, input$gbuttons)
  })
  grab_resources = reactive({
#     if (!exists("gwascat_2023_06_24")) data("gwascat_2023_06_24", package="gwasCatSearch")
     data(gwc_gr)
#     dat = as.data.frame(gwascat_2023_06_24)
     dat = as.data.frame(gwc_gr)  # fixes names, so STUDY.ACCESSION
     last <- process_annotated()
     acc = unique(last$accstr[input$resources_rows_selected])
     validate(need(length(input$resources_rows_selected)>0, "no studies selected"))
     dat = dat[which(dat$STUDY.ACCESSION %in% acc),]
     validate(need(nrow(dat)>0, "no values found in SNPS"))
     dat
     })
     
  output$snps <- DT::renderDataTable({
     fields2use = c("SNPS", "REGION", "CHR_ID", "CHR_POS", "MAPPED_GENE", "CONTEXT",
            "OR.or.BETA", "INITIAL.SAMPLE.SIZE", "REPLICATION.SAMPLE.SIZE")
     lk = input$resources_rows_selected  # watch
     dat = grab_resources()
     #dat[,fields2use]
     dat
   })
  output$snpviz <- plotly::renderPlotly({
#     if (!exists("gwascat_2023_06_24")) data("gwascat_2023_06_24", package="gwasCatSearch")
     data(gwc_gr)
     snpind = input$snps_rows_selected
     validate(need(length(snpind)==1, "please select only one SNP for viz"))
     dat = grab_resources()
     kp = dat[snpind,]
     kp$CHR_ID = kp$seqnames
     kp$CHR_POS = kp$start
     gwasCatSearch::view_variant_context(chr=kp$CHR_ID, pos=kp$CHR_POS, radius=5e5, focal_rec=kp, gwdat=gwc_gr)
     })
  output$snptab <- DT::renderDataTable({
#     if (!exists("gwascat_2023_06_24")) data("gwascat_2023_06_24", package="gwasCatSearch")
     snpind = input$snps_rows_selected
     validate(need(length(snpind)==1, "please select only one SNP for viz"))
     dat = grab_resources()
     kp = dat[snpind,]
     kp$CHR_ID = kp$seqnames
     kp$CHR_POS = kp$start
     #gwasCatSearch::get_variant_context(chr=kp$CHR_ID, pos=kp$CHR_POS, radius=5e5, focal_rec=kp, gwdat=gwc_gr)
     gwasCatSearch::get_variant_context(chr=kp$seqnames, pos=kp$start, radius=5e5, focal_rec=kp, gwdat=gwc_gr)
     })
    
  output$showbuttons <- renderUI({
    validate(need(input$graphicson == TRUE, "must enable graphics on sidebar"))
    if (!exists("efo")) {
      if (file.exists("efo.rda")) {
        load("efo.rda")
      } else {
        efo <<- ontoProc::getOnto("efoOnto")
      }
    }
    last <- process_annotated()
    u <- unique(last$MAPPED_TRAIT_CURIE) # used to eliminate dups, before commas
#
# August 30 -- discovered that MAPPED_TRAIT_CURIE can be comma delimited
#
    u <- unique(unlist(strsplit(u, ",")))
#
# FIXME -- must allow setting of max num tags at UI
#
    su <- u
    if (length(u) >= 15) su <- u[seq_len(15)]
#
# there can be NAs in efo$name[u]
#
    en = efo$name[u]
    dr = which(is.na(en))
    if (length(dr)>0) {
       u = u[-dr]
       su = su[-dr]
       }
    names(u) <- efo$name[u]
    names(su) <- efo$name[su]

    checkboxGroupInput("gbuttons", "ontoterms to show", u, selected = su, inline = TRUE)
  })
  output$dlres <- downloadHandler(
    filename = function() {
      "resources.csv"
    },
    content = function(fname) {
      write.csv(process_annotated(), fname)
    }
  )

  observe({
    if (input$stopBtn > 0) {
      isolate({
        stopApp(returnValue = 0)
      })
    }
  })
  output$packdesc = renderPrint({
    packageDescription("gwasCatSearch")
    })
  output$sessinf = renderPrint({
    sessionInfo()
    })

#
# potential approach for multiple input boxes
#  ids <<- NULL
#
#  observeEvent(input$addInput,{
#    print(ids)
#    if (is.null(ids)){
#      ids <<- 1
#    }else{
#      ids <<- c(ids, max(ids)+1)
#    }
#    output$newInps <- renderUI({
#      tagList(
#        lapply(1:length(ids),function(i){
#          textInput(paste0("txtInput",ids[i]), sprintf("query #%d",ids[i]+1))
#        })
#      )
#    })
#  })

}
