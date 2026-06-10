#' this is for search_gwascat, and symlinked to inst/app2
#' @note Not exported.
#' @rawNamespace import(shiny, except=c(renderDataTable, dataTableOutput))
uif <- function() {
  shiny::fluidPage(
    sidebarLayout(
      sidebarPanel(
        helpText(sprintf(
          "gwasCatSearch v. %s",
          packageVersion("gwasCatSearch")
        )),
        helpText("Enter free text, * permitted; use AND or OR for multiple terms"),
        textAreaInput("query", "query phrase",
          value = "(glomerul* AND filtration) OR urate OR (chronic AND kidney AND disease)", 
          placeholder = "(glomerul* AND filtration) OR urate OR (chronic AND kidney AND disease)",
          height = "350px"
        ),
        actionButton("submit", "Submit"),
#        actionButton("addInput","Add another query"),
#        uiOutput("newInps"),
        helpText("Be sure to refresh hits tab before viewing resources."),
        checkboxInput("graphicson", "Enable graphics", TRUE),
        helpText("graphics startup may involve retrieving an ontology, can take 20 sec or so"),
        actionButton("stopBtn", "stop app"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("hits", 
             helpText("Select terms to limit resource retrieval by clicking in the middle of the associated row"),
             DT::dataTableOutput("hits")),
          tabPanel(
            "resources", fluidRow(
              column(8, checkboxGroupInput(
                inputId = "inclsub",
                label = "resourceOpts",
                choices = c("include subclasses", "direct subclss only"),
                selected = c("include subclasses"), inline = TRUE
              )),
              column(2, downloadButton("dlres", "Download table"))
            ),
            DT::dataTableOutput("resources")
          ),
          #    tabPanel("resources", checkboxInput("inclsub", "include subclasses", TRUE),
          #                          checkboxInput("direct_only", "direct subclss only", FALSE), DT::dataTableOutput("resources")),
          tabPanel("graph", plotOutput("ontoviz"), helpText(" "), uiOutput("showbuttons")),
          tabPanel("snps", helpText("First SNP retrieval can take 20 seconds"), DT::dataTableOutput("snps")),
          tabPanel("manh", plotly::plotlyOutput("snpviz"), 
            helpText("All SNPs in plot:"), DT::dataTableOutput("snptab")),
          tabPanel("about", uiOutput("aboutinfo"), helpText(" "), verbatimTextOutput("packdesc"), verbatimTextOutput("sessinf"))
        )
      )
    )
  )
}

ui <- uif()
