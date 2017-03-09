#The KNmodSmooth(trigram) function applies the modified Kneser-Ney smoothing algorithm to
#the trigram model. It returns the probability of a trigram given the first two words.
#The algorith is described in full by Chen & Goodman (1999) Computer Speech and Language
#13, 359–394.

library(parallel)

tgSmooth <- function() {
    #Apply the modified KN smoothing across the trigram frequency table
    trigramFreq2 <- trigramFreq
    results <- lapply(1:1000, KNmodSmooth)
    trigramPM$bgProb <- sapply(results, function(x) x[1])
    trigramPM$tgProb <- sapply(results, function(x) x[2])
    trigramPM
}

tgSmoothPar <- function() {
    #Apply the modified KN smoothing across the trigram frequency table
    #using parallel processing
    trigramPM <- trigramFreq
    
    cl <- makeCluster(2)
    clusterExport(cl, c("KNmodSmooth", "trigramFreq", "bigramFreq", "Dmatrix", "BGuniPM3",
                        "trigramProb", "bigramProb", "bgRows", "KNmodGamma"))
    results <- parLapply(cl, 1:nrow(trigramFreq), KNmodSmooth)
    stopCluster(cl)
    
    trigramPM$bgProb <- sapply(results, function(x) x[1])
    trigramPM$tgProb <- sapply(results, function(x) x[2])
    trigramPM
}

KNmodPrep <- function() {
    Dmatrix <- matrix(nrow=2, ncol=3, dimnames=list(c("bigramFreq", "trigramFreq"),
                                                    c("c.1", "c.2", "c.3")))
    for (i in 1:3) {Dmatrix[1,i] <- KNmodD(i, bigramFreq)}
    for (i in 1:3) {Dmatrix[2,i] <- KNmodD(i, trigramFreq)}
    Dmatrix
    
    #BGuniPM <- unigramProbMat(wordFreq)
    #results <- unigramProbMat2Par(wordFreq)
    #bgRows <- sapply(results, function(x) x[2:length(x)]) #
}

KNmodD <- function(c, ngramFreq) {
    #This function calculates the discount, D, given the frequency of the n-gram
    #in the appropriate lookup table.
    if (c > 3) {c <- 3}
    n1 <- sum(ngramFreq$Freq==1)
    n2 <- sum(ngramFreq$Freq==2)
    nc <- sum(ngramFreq$Freq==c)
    nc1 <- sum(ngramFreq$Freq==(c+1))
    Y <- n1 / (n1 + 2 * n2)
    c - ((c+1) * Y * nc1 / nc)
}

KNmodSmooth <- function(i) {
    #This function breaks the trigram down into constituent words
    #and passes them to trigramProb(w1, w2, w3)
    trigram <- rownames(trigramFreq)[i]
    tgList <- unlist(strsplit(trigram, " "))
    w1 <- tgList[1]
    w2 <- tgList[2]
    w3 <- tgList[3]
    trigramProb(w1, w2, w3, i)
}

unigramProb <- function(w3) {
    #This function calculates the unigram probability
    #w3Freq <- wordFreq$Freq[grep(paste("^", w3, "$", sep=""), rownames(wordFreq))]
    #w3Freq / sum(wordFreq$Freq)
    
    N1plus_dotw3 <- sum(bigramFreq$Freq[grep(paste(" ", w3, "$", sep=""), rownames(bigramFreq),
                                             perl=TRUE)])
    N1plus_dotw3 / nrow(bigramFreq)
}

bigramProb <- function(w2, w3) {
    #This function calculates the smoothed bigram probability
    bigram <- paste(w2, w3, sep=" ")
    bgCounts <- bigramFreq[bigram, 1]
    whFreq <- bigramFreq$Freq[bgRows[[BGuniPM3[bigram, 4]]]]
    c <- ifelse(bgCounts > 3, 3, bgCounts)
    
    ((bgCounts - Dmatrix["bigramFreq", c]) / BGuniPM3[bigram, 3]) + 
        (KNmodGamma(bigramFreq, whFreq) * BGuniPM3[bigram, 2]) 
}

trigramProb <- function(w1, w2, w3, i) {
    #This function calculates the smoothed trigram probability
    tgCounts <- trigramFreq$Freq[i]
    whFreq <- trigramFreq$Freq[grep(paste("^", w1, " ", w2, " ", sep=""),
                                    rownames(trigramFreq), perl=TRUE)]
    c <- ifelse(tgCounts > 3, 3, tgCounts)
    bgProb <- bigramProb(w2, w3)
    
    tgProb <- ((tgCounts - Dmatrix["trigramFreq", c]) / sum(whFreq)) +
        (KNmodGamma(trigramFreq, whFreq) * bgProb)
    
    c(bgProb, tgProb)
}

KNmodGamma <- function(ngramFreq, whFreq) {
    #This function calculates the scaling factor given the words and the
    #appropriate n-gram lookup table
    ngramStr <- deparse(substitute(ngramFreq))
    D1 <- Dmatrix[ngramStr, 1]
    D2 <- Dmatrix[ngramStr, 2]
    D3 <- Dmatrix[ngramStr, 3]
    
    N1_wdot <- sum(whFreq == 1)
    N2_wdot <- sum(whFreq == 2)
    N3plus_wdot <- sum(whFreq >= 3)
    
    ((D1 * N1_wdot) + (D2 * N2_wdot) + (D3 * N3plus_wdot)) / sum(whFreq)
}

#Need to combine the unigramProbMat functions for neatness
unigramProbMat <- function(wordFreq) {
    #Generates the unigram probability matrix based on bigram frequencies, but using word
    #frequencies as reference
    BGuniPM <- bigramFreq
    BGuniPM$N1plus_dotw3 <- 0
    for (i in 1:nrow(wordFreq)) {
        w1 <- rownames(wordFreq)[i]
        bgRows <- grep(paste(" ", w1, "$", sep=""), rownames(bigramFreq), perl=TRUE)
        N1plus_dotw3 <- sum(bigramFreq$Freq[bgRows]) / nrow(bigramFreq)
        BGuniPM$N1plus_dotw3[bgRows] <- N1plus_dotw3
    }
    BGuniPM
}

unigramProbMat2 <- function(wordFreq) {
    #Generates the summed frequency matrix for first word in bigram list, using word
    #frequencies as reference
    BGuniPM2 <- BGuniPM
    BGuniPM2$N1plus_w2dot <- 0
    for (i in 1:nrow(wordFreq)) {
        w1 <- rownames(wordFreq)[i]
        bgRows <- grep(paste("^", w1, " ", sep=""), rownames(bigramFreq), perl=TRUE)
        N1plus_w2dot <- sum(bigramFreq$Freq[bgRows])
        BGuniPM2$N1plus_w2dot[bgRows] <- N1plus_w2dot
    }
    BGuniPM2
}

unigramProbMat2Par <- function(wordFreq) {
    #Performs the same function as unigramProbMat2, but with parallel processing
    cl <- makeCluster(2)
    clusterExport(cl, "bigramFreq")
    results <- parLapply(cl, rownames(wordFreq), ugProb)
    stopCluster(cl)
    results
}

ugProb <- function(w1) {
    bgRows <- grep(paste("^", w1, " ", sep=""), rownames(bigramFreq), perl=TRUE)
    N1plus_w2dot <- sum(bigramFreq$Freq[bgRows])
    c(N1plus_w2dot, bgRows)
}

bgRowsMat <- function(bgRows, wordFreq) {
    BGuniPM3 <- BGuniPM2
    BGuniPM3$bgRows <- 0
    for (i in 1:nrow(wordFreq)) {
        w2 <- rownames(wordFreq)[i]
        wF_ind <- grep(paste("^", w2, "$", sep=""), rownames(wordFreq), perl=TRUE)
        r_ls <- bgRows[[wF_ind]]
        BGuniPM3$bgRows[r_ls] <- wF_ind
    }
    BGuniPM3
}

bgSmoothPar <- function() {
    #Apply the modified KN smoothing across the bigram frequency table
    #using parallel processing
    bigramPM <- bigramFreq
    
    cl <- makeCluster(2)
    clusterExport(cl, c("bigramFreq", "Dmatrix", "BGuniPM3", "bgRows",
                        "bigramProb2", "KNmodGamma"))
    results2 <- parLapply(cl, 1:nrow(bigramFreq), bigramProb2)
    stopCluster(cl)
    
    bigramPM$bgProb <- as.numeric(results2)
    bigramPM
}

bigramProb2 <- function(i) {
    #This function calculates the smoothed bigram probability
    bigram <- rownames(bigramFreq)[i]
    bgCounts <- bigramFreq[bigram, 1]
    whFreq <- bigramFreq$Freq[bgRows[[BGuniPM3[bigram, 4]]]]
    c <- ifelse(bgCounts > 3, 3, bgCounts)
    
    ((bgCounts - Dmatrix["bigramFreq", c]) / BGuniPM3[bigram, 3]) + 
        (KNmodGamma(bigramFreq, whFreq) * BGuniPM3[bigram, 2]) 
}