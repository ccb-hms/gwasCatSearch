
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
 
 output$resources = DT::renderDataTable({
   tab = ntab()
   u = unique(rownames(tab))
   nu = length(u)
   ans = vector("list", nu)
   withProgress(message = "collecting resources", value = 0, {
     for (i in seq_len(nu)) {
      ans[[i]] = gwasCatSearch::resources_annotated_with_term(u[i])
      incProgress(1/nu)
      }
     })
   last = do.call(rbind, ans)
   dups = which(duplicated(last$STUDY.ACCESSION))
   if (length(dups)>0) last = last[-dups,]
   shinyjs::hide("notif")
   last$PUBMEDID = sprintf("<A href='https://pubmed.ncbi.nlm.nih.gov/%s/'>%s</A>", last$PUBMEDID,
      last$PUBMEDID)
   last$MAPPED_TRAIT_URI = sprintf("<A href='%s'>%s</A>", last$MAPPED_TRAIT_URI, last$MAPPED_TRAIT_URI)
   last$STUDY.ACCESSION = sprintf("<A href='https://www.ebi.ac.uk/gwas/studies/%s' target='_blank'>%s</A>", 
     last$STUDY.ACCESSION, last$STUDY.ACCESSION)
   DT::datatable(last, escape=FALSE)
   })
 observe({
            if(input$stopBtn > 0)
               isolate({
                 stopApp(returnValue=0)
                      })
           })

}

