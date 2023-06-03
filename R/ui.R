#' this is for search_gwascat, and symlinked to inst/app2
#' @note Not exported.
#' @rawNamespace import(shiny, except=c(renderDataTable, dataTableOutput))
uif = function() shiny::fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText(sprintf("gwasCatSearch v. %s",
              packageVersion("gwasCatSearch"))),
   helpText("Enter free text, * permitted"),
   textInput("query", "query", value="vasculitis", placeholder="vasculitis",
     width="200px"), 
   helpText("Be sure to refresh hits tab before viewing resources."),
   checkboxInput("graphicson", "Enable graphics", FALSE),
   helpText("graphics startup involves retrieving an ontology, can take 20 sec or so"),
   actionButton("stopBtn", "stop app"),
   width=2
   ),
  mainPanel(
   tabsetPanel(
    tabPanel("hits", DT::dataTableOutput("hits")),
    tabPanel("resources", checkboxInput("inclsub", "include subclasses", TRUE),
                          checkboxInput("direct_only", "direct subclss only", FALSE), DT::dataTableOutput("resources")), 
    tabPanel("graph", plotOutput("ontoviz"), helpText(" "), uiOutput("showbuttons")),
    tabPanel("about", helpText("This experimental app is based on a tokenization of phenotype descriptions
from the EBI/NHGRI GWAS catalog, data obtained in March 2023.  The text2term mapper was applied,
a corpus was derived using corpustools, and the corpus can be interrogated with regular expression
and phrase logic.

https://computationalbiomed.hms.harvard.edu/tools-and-technologies/tools-tech-details/text2term-ontology-mapping/"))
   )
  )
 )
)

ui = uif()