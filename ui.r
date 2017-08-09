ui = tagList(
   #  shinythemes::themeSelector(), # theme selector
  navbarPage(
    theme = "readable",  # <--- To use a theme, uncomment this
    "gene expression",
    # tabPanel("EBI atlas", 
    #          DT::dataTableOutput('EBI')),
    # tabPanel("NCBI GEO Data", 
    #          DT::dataTableOutput('Ncbi')),
    tabPanel("TCGA",
             sidebarPanel(
               tags$head(
                 tags$style(type="text/css", "select { max-width: 140px; }"),
                 tags$style(type="text/css", ".span4 { max-width: 190px; }"),
                 tags$style(type="text/css", ".well { max-width: 180px; }")
               ),
               selectInput("myGene", "Select a gene:",
                           choices = c("GFRAL", "GDF15", "INHBA", "IL6", "IL8", "CXCL10", "CCL4", "VEGF")),
               selectInput("tumorType", "Select tumor type :",
                           choices = c("BLCA", "BRCA", "CESC", "COAD", "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LIHC",
                                       "LUAD", "LUSC", "PRAD", "READ", "STAD", "THCA", "UCEC"))
             ),
             mainPanel(
               column(12,
               tabsetPanel(
                 tabPanel("Individual Tumors Boxplot",
                          plotOutput("TCGA_tumor_box"),
                          tableOutput("table")
                 ),
                 tabPanel("Individual Tumors Violin Plot",
                          fluidRow(plotOutput("TCGA_tumor"),
                          tableOutput("table2")
                          )#fluidRow
                          ),
                 tabPanel("TCGA sample guide",
                          tableOutput("sample_guide")
                          )
               )#,style='width: 1200px; height: 400px'#tabsetPanel
               )#column,
               ,width = 10)#mainPanel
    ),#tabPanel TCGA
    tabPanel("TCGA whole dataset plots",
             tags$head(
               tags$style(type="text/css", "select { max-width: 140px; }"),
               tags$style(type="text/css", ".span4 { max-width: 190px; }"),
               tags$style(type="text/css", ".well { max-width: 180px; }")
             ),
             selectInput("myGene2", "Select a gene:",
                         choices = c("GFRAL", "GDF15", "INHBA", "IL6", "IL8", "CXCL10",
                           "CCL4")),
             downloadButton('DownloadImage')
    )#TCGA whole dataset plots
  )
)