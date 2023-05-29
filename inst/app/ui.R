library(gwasCatSearch)

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
