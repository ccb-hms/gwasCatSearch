#' produce an ontology_index instance from GWASCatalogSearchDB sqlite
#' using only a vector of EFO curies
#' @importFrom ontologyIndex ontology_index
#' @importFrom dplyr tbl
#' @param con SQLite connection via RSQLite/DBI dbConnect
#' @note This is used in the server for the `search_gwascat()` app. 
#' @examples
#' tags = c("DOID:10113", "DOID:7551", 
#"      "EFO:0000278", "EFO:0000342", "EFO:0000405", 
#'      "EFO:0000549", "EFO:0000584", "EFO:0000638", 
#'      "EFO:0000650", "EFO:0000694")
#' oo = make_oi(tags) 
#' oo
#' if (requireNamespace("ontoProc"))
#'    ontoProc::onto_plot2(oo, names(oo$name))
#' @export
make_oi = function (tags) 
{
    tags = unique(tags)
    anc = EFOancestors(tags)
    cands = c(unique(anc), tags)
    pars = EFOparents(cands)
    up = unname(pars)
    np = names(pars)
    sp = split(up, np)
    labs = EFOlabels(names(sp))
    bad = setdiff(names(sp), names(labs))
    if (length(bad) > 0) {
        drop = match(bad, names(sp))
        sp = sp[-drop]
        sp = sp[names(labs)]
    }
    ontology_index(name = labs, parents = sp)
}


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
#    if (!exists("efo")) efo <<- ontoProc::getOnto("efoOnto")
    validate(need(!is.null(input$gbuttons), "Waiting for gbutton UI"))
    last <- process_annotated()
    validate(need(length(input$gbuttons)>1, "only one term present, nothing to plot"))
    efo = make_oi(input$gbuttons)
    ontoProc::onto_plot2(efo, input$gbuttons)
  })
  grab_resources = reactive({
     con = gwasCatSearch_dbconn()
     dat = dbGetQuery(con, "select * from gwascatalog_associations")
     last <- process_annotated()
     acc = unique(last$accstr[input$resources_rows_selected])
     validate(need(length(input$resources_rows_selected)>0, "no studies selected"))
     dat = dat[which(dat$STUDY.ACCESSION %in% acc),]
     validate(need(nrow(dat)>0, "no values found in SNPS"))
     dat$seqnames = dat$`CHR_ID`
     dat$start = as.numeric(dat$`CHR_POS`)
     dat
     })
     
  output$snps <- DT::renderDataTable({
#     fields2use = c("SNPS", "REGION", "CHR_ID", "CHR_POS", "MAPPED_GENE", "CONTEXT",
#            "OR.or.BETA", "INITIAL.SAMPLE.SIZE", "REPLICATION.SAMPLE.SIZE")
     lk = input$resources_rows_selected  # watch
     dat = grab_resources()
     dat
   })
  output$snpviz <- plotly::renderPlotly({
     dat = grab_resources()
     d4manh = ggmanh::manhattan_data_preprocess(dat, chr.colname="seqnames",
            pos.colname="start", pval.colname="P-VALUE", chr.order=c(1:22, "X", "Y"))
     gwasCatSearch::simple_ggmanh(d4manh, y.label="-log10 p", label.colname = "MAPPED_TRAIT")
     })
  output$snptab <- DT::renderDataTable({
     snpind = input$snps_rows_selected
     dat = grab_resources()
     kp = dat
     kp$CHR_ID = kp$seqnames
     kp$CHR_POS = kp$start
     kp
     })
    
  output$showbuttons <- renderUI({
    validate(need(input$graphicson == TRUE, "must enable graphics on sidebar"))
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
    efo = make_oi(u)
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

}
