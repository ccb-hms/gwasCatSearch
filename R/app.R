
server = function(input, output, session) {
 data("efo_tc", package="gwasCatSearch")
 data("efo_df", package="gwasCatSearch")
 ntab = reactive({
   sout = corpustools::search_features(tc = efo_tc, query=input$query)
   validate(need(nrow(sout$hits)>0, "no hits, please try a different query"))
   tab = hits2DT(sout, efo_df, efo_tc)
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
      ans[[i]] = resources_annotated_with_term(u[i])
      incProgress(1/nu)
      }
     })
   last = do.call(rbind, ans)
   dups = which(duplicated(last$STUDY.ACCESSION))
   if (length(dups)>0) last = last[-dups,]
   shinyjs::hide("notif")
   last$PUBMEDID = sprintf("<A href='https://pubmed.ncbi.nlm.nih.gov/%s/'>%s</A>", last$PUBMEDID,
      last$PUBMEDID)
   DT::datatable(last, escape=FALSE)
   })
 observe({
            if(input$stopBtn > 0)
               isolate({
                 stopApp(returnValue=0)
                      })
           })

}

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText(sprintf("gwasCatSearch v. %s",
              packageVersion("gwasCatSearch"))),
   helpText("Enter free text, * permitted"),
   textInput("query", "query", value="vasculitis", placeholder="vasculitis",
     width="200px"), 
   helpText("Be sure to refresh hits tab before viewing resources."),
   actionButton("stopBtn", "stop app"),
   width=2
   ),
  mainPanel(
   tabsetPanel(
    tabPanel("hits", DT::dataTableOutput("hits")),
    tabPanel("resources", tags$div(id="notif", helpText("can be slow; progress bar in lower right")), DT::dataTableOutput("resources")),  # want to remove notif with hide() in server but not working
    tabPanel("about", helpText("This experimental app is based on a tokenization of phenotype descriptions
from the EBI/NHGRI GWAS catalog, data obtained in March 2023.  The text2term mapper was applied,
a corpus was derived using corpustools, and the corpus can be interrogated with regular expression
and phrase logic.

https://computationalbiomed.hms.harvard.edu/tools-and-technologies/tools-tech-details/text2term-ontology-mapping/"))
   )
  )
 )
)

search_gwascat = function() {
 shiny::runApp(list(ui=ui, server=server))
}
