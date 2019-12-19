# This script turns the 20newsgroup dataset into a Term Document Matrix

library(tm)

# readNewsgroups - reads all text files from all subdirectories of the given path,
#                  performs some preprocessing (remove non-letters, stopwords,
#                  rare words, small documents) and returns a term-document-matrix
# Input:
#   path - String, giving the path where the files lay
#   minWordFreq - integer, words have to occur at least this many times throughout
#                 the corpus to be included in the TDM
#   minDocSize - integer, documents have to include at least this many words
#                to be included in the TDM
#   showProgress - logical, defines if progress updates should be shown in console
# Output:
#   list with 2 elements:
#     tdm - a matrix where each row is a word and each column a document.
#           Gives relative frequencies of words per document
#     vocab - character vector, giving the words in the order of the TDM rows
readNewsgroups <- function(path = "./20news-bydate-train", minWordFreq = 50L, 
                           minDocSize = 50L, showProgress = TRUE){
  # readFile - reads a text file and removes signatures, newsgroup names and
  #            special characters. Returns a character with the words of the text
  #            (can have length 0)
  readFile <- function(path){
    text <- readLines(path)
    
    # Throw out the first lines giving contextual information
    # This can be found with lines that have a "... writes:" at the end and a
    # blank line before
    contextEnd <- which(grepl(" writes:[[:blank:]]*[>]*$", text[-1]) & text[-length(text)] == "") + 1
    if(length(contextEnd) == 1){
      text <- text[-seq(contextEnd)]
    }
    
    # Try to detect signatures and throw them out (optional)
    signatureStart <- which(text[-1] == "--" & text[-length(text)] == "")
    if(length(signatureStart) == 1 && signatureStart > 1){
      text <- text[seq(signatureStart - 1)]
    }
    
    # Remove all occurences of the newsgroup name:
    text <- gsub("comp\\.graphics|comp\\.os\\.ms-windows\\.misc|
                 comp\\.sys\\.ibm\\.pc\\.hardware|comp\\.sys\\.max\\.hardware|
                 comp\\.windows\\.x|rec\\.autos|rec\\.motorcycles|
                 rec\\.sport\\.baseball|rec\\.sport\\.hockey|
                 sci\\.crypt|sci\\.electronics|sci\\.med|sci\\.space|
                 misc\\.forsale|talk\\.politics\\.misc|talk\\.politics\\.guns|
                 talk\\.politics\\.mideast|talk\\.religion\\.misc|
                 alt\\.atheism|soc\\.religion\\.christian", "", text)
    
    # Remove everything that is not a character
    text <- gsub("['`´]", "", text) # for words like "that's" -> "thats"
    text <- gsub("[[:punct:]]", " ", text)
    text <- gsub("[^[[:alpha:]]]", "", text)
    text <- tolower(text)
    
    # extract words:
    words <- unlist(unname(sapply(text, strsplit, "[[:blank:]]")))
    
    # remove small words and stopwords
    words <- words[words != ""]
    words <- words[nchar(words) > 1]
    words <- words[!words %in% stopwords()]
    
    return(words)
  }
  
  # read the texts
  if(showProgress) cat("Reading files...\n")
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  texts <- lapply(files, readFile)
  names(texts) <- files
  
  # Find the vocabulary
  if(showProgress) cat("Finding vocabulary...\n")
  vocab <- unlist(texts)
  vocab <- table(vocab)
  vocabToUse <- names(vocab[vocab >= minWordFreq])
  
  # Remove rare words and small docs
  if(showProgress) cat("Removing rare words and small documents...\n")
  for(j in seq(length(texts))){
    texts[[j]] <- texts[[j]][texts[[j]] %in% vocabToUse]
  }
  texts <- texts[sapply(texts, length) >= minDocSize]
  
  # create TDM
  if(showProgress) cat("Creating TDM...\n")
  tdm <- matrix(0, nrow = length(vocabToUse), ncol = length(texts))
  rownames(tdm) <- vocabToUse
  colnames(tdm) <- names(texts)
  for(j in seq(length(texts))){
    words <- table(texts[[j]])
    tdm[match(names(words), vocabToUse), j] <- words / sum(words)
  }
  
  return(list(tdm = tdm, 
              vocab = vocabToUse))
}

news <- readNewsgroups()
save(news, file = "news.RData")
