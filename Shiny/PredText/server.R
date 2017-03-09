library(shiny)
library(tm)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    PredTextModel2 <- reactive({
        txt <- input$txtIn
        Nwords <- input$numWords
        txt_pp <- removePunctuation(txt)
        txt_pp <- tolower(txt_pp)
        txt_pp <- removeNumbers(txt_pp)
        txt_pp <- stripWhitespace(txt_pp)
        wordList <- unlist(strsplit(txt_pp, " "))
        last2words <- paste(tail(wordList, 2), collapse=" ")
        tgrams <- subset(trigramPM, grepl(paste("^", last2words, " ", sep=""),
                                          rownames(trigramPM), perl=TRUE))
        
        if(nrow(tgrams)<Nwords) {
            lastword <- tail(wordList, 1)
            bgrams <- subset(bigramPM, grepl(paste("^", lastword, " ", sep=""),
                                             rownames(bigramPM), perl=TRUE))
            if(nrow(bgrams)<(Nwords-nrow(tgrams))) {
                predWords <- c(getPredWords(tgrams, nrow(tgrams)), 
                               getPredWords(bgrams, nrow(bgrams)), 
                               rownames(wordFreq)[1:(Nwords-nrow(tgrams)-nrow(bgrams))])
            }
            else {predWords <- c(getPredWords(tgrams, nrow(tgrams)), 
                                 getPredWords(bgrams, 
                                              min((Nwords-nrow(tgrams)), nrow(bgrams))))}
        }
        else {predWords <- getPredWords(tgrams, Nwords)}
        
        predWords
    })
    
    getPredWords <- function(ngrams, N) {
        if (N==0) {return(NULL)}
        PredWords <- ngrams[order(-ngrams$Prob), , drop=FALSE]
        PredWords <- rownames(PredWords)[1:N]
        PredWords <- sapply(PredWords, function(x) tail(unlist(strsplit(x, " ")),1))
        as.character(PredWords)
    }
    
    output$txtOut <- renderTable({
        PredTextModel2()
    }, colnames=FALSE)
  })