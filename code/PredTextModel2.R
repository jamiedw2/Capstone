library(tm)
#This predictive text model is based on modifies Kneser-Ney smoothed ngram probabilities.
#It takes the last two words of a phrase and returns the last words of the three most probable
#trigrams in the list. If there are no matching trigrams, it does the same from the bigram
#list, then the word list.
#This model didn't do any better on the quizzes than the first model, but some of the
#suggestions were more sensible.
PredTextModel2 <- function(txt, Nwords){
    txt_pp <- removePunctuation(txt)
    txt_pp <- tolower(txt_pp)
    txt_pp <- removeNumbers(txt_pp)
    txt_pp <- stripWhitespace(txt_pp)
    wordList <- unlist(strsplit(txt_pp, " "))
    last2words <- paste(tail(wordList, 2), collapse=" ")
    tgrams <- subset(trigramPM, grepl(paste("^", last2words, " ", sep=""),
                                      rownames(trigramPM), perl=TRUE))
    
    predWords <- getPredWords(tgrams, min(Nwords, nrow(tgrams)))
    if(length(predWords)<Nwords) {
        lastword <- tail(wordList, 1)
        bgrams <- subset(bigramPM, grepl(paste("^", lastword, " ", sep=""),
                                          rownames(bigramPM), perl=TRUE))
        rptWords <- sapply(rownames(bgrams), 
                           function(x) {tail(unlist(strsplit(x, " ")),1) %in% predWords})
        bgrams <- subset(bgrams,!rptWords)
        predWords <- c(predWords, getPredWords(bgrams, min((Nwords-nrow(tgrams)),
                                                           nrow(bgrams))))
        if(length(predWords)<Nwords) {
            wF <- subset(wordFreq, !(rownames(wordFreq) %in% predWords))
            predWords <- c(predWords, rownames(wF)[1:(Nwords-nrow(tgrams)-nrow(bgrams))])
        }
    }
    predWords
}

getPredWords <- function(ngrams, N) {
    if (N==0) {return(NULL)}
    PredWords <- ngrams[order(-ngrams$Prob), , drop=FALSE]
    PredWords <- rownames(PredWords)[1:N]
    PredWords <- sapply(PredWords, function(x) tail(unlist(strsplit(x, " ")),1))
    as.character(PredWords)
}