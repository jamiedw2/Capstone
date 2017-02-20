

KNsmooth_mod <- function(ngramFreq) {
    n1 <- sum(ngramFreq$Freq==1)
    n2 <- sum(ngramFreq$Freq==2)
    n3 <- sum(ngramFreq$Freq==3)
    n4 <- sum(ngramFreq$Freq==4)
    
    Y <- n1 / (n1 + 2 * n2)
    
    D1 <- 1 - (2 * Y * n2 / n1)
    D2 <- 2 - (3 * Y * n3 / n2)
    D3 <- 3 - (4 * Y * n4 / n3)
    
}