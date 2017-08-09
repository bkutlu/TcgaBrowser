# load data
myDataSet <- readRDS('data/tcgaDataGoi.rds')
tumorSampleInv <- read.delim(file = 'data/tcga_cancer_sample_guide.txt', sep = "\t", header = T)
ebiDat <- readRDS('data/ebiDatAnn.rds')

ebiDat$Experiment.Accession <- sapply(ebiDat$Experiment.Accession, function (x){
  toString(tags$a(href=paste0("http://www.ebi.ac.uk/arrayexpress/experiments/", x), x))
  
})

NcbiDat <- readRDS('data/ncbiDat.rds')
tcgaTumTypes <- readRDS('data/tcgaTumTypes.rds')


library(ggplot2)
library(dplyr)

# function to format the y axis
scaleFUN <- function(x) sprintf("%.1f", x)

plotTumor <- function (myData = myData, ploTitre = "", plotType = "boxplot"){
  
  tumPlot <-  ggplot(myData, aes(x = factor(tumor), fill = factor(sample), color = factor(sample), y = tpm)) +
    geom_point(position=position_jitterdodge(dodge.width=0.9)) 
  if (plotType == "boxplot"){
    tumPlot <- tumPlot + geom_boxplot(fill="transparent", 
                position = position_dodge(width=0.9), lwd = 1.2) 
  } else {
    tumPlot <- tumPlot + geom_violin(fill="transparent",outlier.colour = NA, 
                position = position_dodge(width=0.9), lwd = 1.2) 
    }#if
    
  tumPlot <- tumPlot +
    scale_y_continuous(trans = "log2", labels = scaleFUN) +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=305, hjust=0)) +
    labs(title = ploTitre) +
    theme(text = element_text(size=30),
          axis.text.x = element_text(angle=305, hjust=0, size = rel(0.5)),
          legend.text = element_text(size = rel(0.6)),
          legend.title = element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(size = rel(1.2)))
  
  print(tumPlot)

  }# plotTumor

server = function(input, output) {
  
  tumTitle <- reactive({
    tcgaTumTypes %>% filter(Cohort == input$tumorType) %>% .[["Disease.Name"]]
  })
  
  
  output$EBI <- DT::renderDataTable(
    ebiDat <- DT::datatable(ebiDat, escape = FALSE)
  )
  
  output$Ncbi <- DT::renderDataTable(
    NcbiDat <- DT::datatable(NcbiDat, escape = FALSE)
  )
  
  output$TCGA_tumor <- renderPlot({
    myDataSet <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene)
     plotTumor(myDataSet, ploTitre = tumTitle(), plotType = "violin")
  })
  
  # 
  output$TCGA_tumor_box <- renderPlot({
    myDataSet <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene)
    plotTumor(myDataSet, ploTitre = tumTitle(), plotType = "boxplot")
  })
  
  output$table <- renderTable({
    controlDat <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene, sample == "control")$tpm
    tumorDat <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene, sample == "cancer")$tpm
   myT <-  rbind(c(mean(controlDat, na.rm = T), 
                   sd(controlDat, na.rm = T)), 
          c(mean(tumorDat, na.rm = T), 
            sd(tumorDat, na.rm = T))
            )
  
    dimnames(myT) <- list(c("control", "cancer"),c("mean tpm","SD"))
    myT
  }, rownames = TRUE)
  
  
  output$table2 <- renderTable({
    controlDat <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene, sample == "control")$tpm
    tumorDat <- filter(myDataSet, tumor == input$tumorType , goi == input$myGene, sample == "cancer")$tpm
    myT <-  rbind(c(mean(controlDat, na.rm = T), 
                    sd(controlDat, na.rm = T)), 
                  c(mean(tumorDat, na.rm = T), 
                    sd(tumorDat, na.rm = T))
    )
    
    dimnames(myT) <- list(c("control", "cancer"),c("mean tpm","SD"))
    myT
  }, rownames = TRUE)
  
  # table of samples  
  output$sample_guide <- renderTable({
    tumorSampleInv
  })

    
    
output$DownloadImage = downloadHandler(
    filename = function() { paste(input$myGene2, '_all_samples.pdf', sep='') },
    content = function(file) {
    pdf(file, width = 30, height = 10)
     myDataSet <- filter(myDataSet, goi == input$myGene2)
     plotTumor(myDataSet, ploTitre = input$myGene2)
     dev.off()
    })
  
}
