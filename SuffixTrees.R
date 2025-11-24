rm(list=ls())
library(Biostrings)

## TASK 1
SuffixArray <- function(text){
  s <- as.character(text)
  n <- nchar(s)
  
  SA <- substring(s, first = seq_len(n), last = n)
  SA <- order(SA)
  return(SA)
}


text <- DNAString('CTAATAATG')
SA <- SuffixArray(text)
SA


## TASK 2
InverseSuffixArray <- function(SA){
  ISA <- integer(length(SA))
  for (i in seq_along(SA)) {
    ISA[SA[i]] <- i
  }
  return(ISA)
}

ISA <- InverseSuffixArray(SA)
ISA

## TASK 3
LCPArray <- function(text, SA, ISA){
  m <- length(text)
  string <- xscat(as.character(text), "$")
  LCP <- integer(length(SA))
  LCP[1] <- -1
  LCP[m + 1] <- -1
  l <- 0
  for (i in 1:m){
    j <- ISA[i]
    if (j > 1){
      k <- SA[j - 1]
      while (text[k + l] == text[i + l]){
        l <- l + 1}
      LCP[j] <- l
      l <- max(l - 1, 0)}}
  return(LCP)
}

LCP <- LCPArray(text, SA, ISA)
LCP

## TASK 4
BinarySearchSA <- function(pattern, text, SA){
  minIndex <- 1
  maxIndex <- length(SA)
  while (minIndex < maxIndex){
    midlIndex <- floor((minIndex + maxIndex) / 2)
    if (pattern <= subseq(text, start = SA[midlIndex])){
       maxIndex <- midlIndex
    }
    else{
      minIndex <- midlIndex + 1
    }
   }
  First <- minIndex
  maxIndex <- length(SA)
  while(maxIndex > minIndex){
    midlIndex <- ceiling((minIndex + maxIndex) / 2)
    if (subseq(text, start = SA[midlIndex]) <= pattern){
       minIndex <- midlIndex + 1
    } 
    else{
       maxIndex <- midlIndex
     }
  }
  Last <- maxIndex - 1
  if (Last < First){
    return('Pattern does not appear in text')
  }
   else{
    return(c(First, Last))
   }
}


pattern <- DNAString('AAT')
BinarySearchSA(pattern, text, SA)

 