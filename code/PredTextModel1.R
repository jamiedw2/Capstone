library(tm)
library(SnowballC)
#This is my first attempt at a predictive text model based on a sample of the corpus.
#This function simply takes the last two words in a given phrase and looks for them in the 
#trigramFreq list, returning the last word in the trigram as the prediction.
#If it can't find it in the trigram list, it looks for the last word in the bigram list.
#Failing that, it returns the most likely word from the wordFreq list, which is "the".
#This model was correct for 3/10 questions in Quiz 2.
PredTextModel1 <- function(txt){
    txt_pp <- removePunctuation(txt)
    txt_pp <- tolower(txt_pp)
    txt_pp <- removeNumbers(txt_pp)
#   txt_pp <- wordStem(txt_pp)  Comment out, because this seems to cause problems
    txt_pp <- stripWhitespace(txt_pp)
    wordList <- unlist(strsplit(txt_pp, " "))
    last2words <- paste(tail(wordList, 2), collapse=" ")
    tgpnts <- grep(paste("^", last2words, " ", sep=""), rownames(trigramFreq))
    
    if(length(tgpnts)==0) {
        lastword <- tail(wordList, 1)
        bgpnts <- grep(paste("^", lastword, sep=""), rownames(bigramFreq))
        if(length(bgpnts)==0) {
            predWord <- rownames(wordFreq)[1]
        }
        else {predWord <- unlist(strsplit(rownames(bigramFreq)[min(bgpnts)], " "))[3]}
    }
    else {predWord <- unlist(strsplit(rownames(trigramFreq)[min(tgpnts)], " "))[3]}
    
    predWord
    
}