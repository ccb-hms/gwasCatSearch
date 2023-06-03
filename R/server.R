#' this is called by search_gwascat, also symlinked to inst/app2 for shinyapps usage
#' @note Not exported.
server = function(input, output, session) {
 data("efo_tc", package="gwasCatSearch")
 data("efo_df", package="gwasCatSearch")
 ntab = reactive({
   sout = corpustools::search_features(tc = efo_tc, query=input$query)
   validate(need(nrow(sout$hits)>0, "no hits, please try a different query"))
   tab = gwasCatSearch::hits2DT(sout, efo_df, efo_tc)
   tab
   })
 output$hits = DT::renderDataTable({
   DT::datatable(ntab(), escape=FALSE)
   })
 
 process_annotated = reactive({
   tab = ntab()
   u = unique(rownames(tab))
#   nu = length(u)
#   ans = vector("list", nu)
#   withProgress(message = "collecting resources", value = 0, {
#     for (i in seq_len(nu)) {
#      ans[[i]] = gwasCatSearch::resources_annotated_with_term(u) #[i])
#      incProgress(1/nu)
#      }
#     })
#   last = do.call(rbind, ans)
   last = gwasCatSearch::resources_annotated_with_term(u, include_subclasses=input$inclsub,
     direct_subclasses_only = input$direct_only) 
   dups = which(duplicated(last$STUDY.ACCESSION))
   if (length(dups)>0) last = last[-dups,]
   shinyjs::hide("notif")
   last$PUBMEDID = sprintf("<A href='https://pubmed.ncbi.nlm.nih.gov/%s/'>%s</A>", last$PUBMEDID,
      last$PUBMEDID)
   last$MAPPED_TRAIT_URI = sprintf("<A href='%s'>%s</A>", last$MAPPED_TRAIT_URI, last$MAPPED_TRAIT_URI)
   last$STUDY.ACCESSION = sprintf("<A href='https://www.ebi.ac.uk/gwas/studies/%s' target='_blank'>%s</A>", 
     last$STUDY.ACCESSION, last$STUDY.ACCESSION)
   last
   })
 output$resources = DT::renderDataTable({
   last = process_annotated()
   DT::datatable(last, escape=FALSE)
   })
 output$ontoviz = renderPlot({
   validate(need(input$graphicson == TRUE, "must enable graphics on sidebar"))
   if (!exists("efo")) efo <<- ontoProc::getOnto("efoOnto")
   validate(need(!is.null(input$gbuttons), "Waiting for gbutton UI"))
   last = process_annotated()
#   u = unique(last$MAPPED_TRAIT_CURIE)
#   kp = input$gbuttons
#   su = u
#   if (length(u)>= 15) su = u[seq_len(15)]
   ontoProc::onto_plot2(efo, input$gbuttons)
})
 output$showbuttons = renderUI({
   validate(need(input$graphicson == TRUE, "must enable graphics on sidebar"))
   if (!exists("efo")) {
      if (file.exists("efo.rda")) load("efo.rda")
      else efo <<- ontoProc::getOnto("efoOnto")
      }
   last = process_annotated()
   u = unique(last$MAPPED_TRAIT_CURIE)
   su = u
   if (length(u)>= 15) su = u[seq_len(15)]
   names(u) = efo$name[u]
   names(su) = efo$name[su]

   checkboxGroupInput("gbuttons", "ontoterms to show", u,selected=su, inline=TRUE)
   })
  
 observe({
            if(input$stopBtn > 0)
               isolate({
                 stopApp(returnValue=0)
                      })
           })

}

