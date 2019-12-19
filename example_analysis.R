# Analysis of the DAM Model on the 20newsgroups dataset

# Dependencies
load("news.RData")
source("DAM.R")


# Run the topic model:
damResult <- dam(news$tdm, alpha = 0.2, delta = 0.25, catchwordMinFreq = 0.005,
                 r = 6, algorithm = "kmeans")
rownames(damResult$B) <- rownames(news$tdm)
names(damResult$dominantTopic) <- colnames(news$tdm)
  
  
# Distribution of Topics:
plot(table(damResult$dominantTopic)/ length(damResult$dominantTopic))
# Does not look degenerated, which is good


# Let's find out the semantic meaning of the topics:
# Most frequent words in each topic:
apply(damResult$B, 2, function(x){
  rownames(damResult$B)[order(x, decreasing = TRUE)[1:10]]
})
#       [,1]      [,2]       [,3]           [,4]   [,5]           [,6]    
# [1,]  "edu"     "edu"      "edu"          "edu"  "ac"           "edu"   
# [2,]  "one"     "one"      "center"       "com"  "uk"           "one"   
# [3,]  "can"     "can"      "will"         "one"  "edu"          "can"   
# [4,]  "people"  "dont"     "subject"      "dod"  "can"          "god"   
# [5,]  "god"     "people"   "lines"        "can"  "one"          "people"
# [6,]  "dont"    "baseball" "one"          "dont" "subject"      "will"  
# [7,]  "subject" "god"      "can"          "like" "lines"        "dont"  
# [8,]  "cwru"    "think"    "organization" "just" "organization" "think" 
# [9,]  "com"     "will"     "nasa"         "god"  "will"         "com"   
# [10,] "lines"   "com"      "like"         "will" "university"   "just"  
#
# still many fragments of mail addresses and so on. Could be improved by a better
# data preprocessing.

# The catchwords for each topic:
rownames(damResult$B)[damResult$pseudoCatchword]
#"cwru"     "baseball" "center"   "dod"      "ac"       "accept"  

# Words that occur way more frequently in this than in other topics:
for(l in 1:6){
  otherTops <- apply(damResult$B[, -l], 1, max)
  dist <- damResult$B[, l] - otherTops
  cat(paste(rownames(damResult$B)[order(dist, decreasing = TRUE)[1:20]], sep = ", "), "\n")
}
# edu cwru cleveland freenet ins case western reserve po posting 
# baseball game players games league runs mets hitter year last 
# center nasa space research gov points israeli hamburg policy armenian 
# dod bike com bmw bnr ride dog helmet ysu ama 
# ac uk use university version nz dcs 44 cs using 
# accept god jesus will christians people window church want us 
#
# We may label the topics as:
# Topic 1: Internet
# Topic 2: Sports
# Topic 3: Institutions
# Topic 4: Technology
# Topic 5: University
# Topic 6: Religion


# Representative Document per Topic:
colnames(news$tdm)[sapply(damResult$pseudoPureDocs, "[", 1)]
# [1] "./20news-bydate-train/comp.sys.ibm.pc.hardware/60718"
# [2] "./20news-bydate-train/rec.sport.hockey/53734"        
# [3] "./20news-bydate-train/comp.graphics/38320"           
# [4] "./20news-bydate-train/rec.motorcycles/104945"        
# [5] "./20news-bydate-train/talk.politics.mideast/76307"   
# [6] "./20news-bydate-train/soc.religion.christian/21397"  


# See how the topics are used across the different newsgroups:
# Topic per newsgroup
newsgroup <- gsub("^\\./[^/]+/([^/]+)/[[:digit:]]+$", "\\1", names(damResult$dominantTopic))
topicPerGroup <- tapply(damResult$dominantTopic, newsgroup, function(x) {
  x <- factor(x, levels = 1:6)
  table(x) / length(x)
  })

# Topic per Newsgroup-theme:
area <- sapply(newsgroup, function(x) switch(x, 
               "comp.graphics" = "computer",
               "comp.os.ms-windows.misc" = "computer",
               "comp.sys.ibm.pc.hardware" = "computer",
               "comp.sys.mac.hardware" = "computer",
               "comp.windows.x" = "computer",
               "rec.autos" = "sports",
               "rec.motorcycles" = "sports",
               "rec.sport.baseball" = "sports",
               "rec.sport.hockey" = "sports",
               "sci.crypt" = "science",
               "sci.electronics" = "science",
               "sci.med" = "science",
               "sci.space" = "science",
               "misc.forsale" = "classifieds",
               "talk.politics.misc" = "politics",
               "talk.politics.guns" = "politics",
               "talk.politics.mideast" = "politics",
               "talk.religion.misc" = "religion",
               "alt.atheism" = "religion",
               "soc.religion.christian" = "religion"))
topicPerArea <- tapply(damResult$dominantTopic, area, function(x) {
  x <- factor(x, levels = 1:6)
  table(x) / length(x)
  })
crosstab <- round(table(damResult$dominantTopic, area)[, c(5, 1, 2, 3, 4, 6)] / length(area) * 100, 2)
#   science classifieds computer politics religion sports
# 1    3.56        1.61     6.71     1.28     0.90   3.91
# 2    2.83        0.08     1.50     2.02     1.03   4.86
# 3    5.31        0.89     4.74     5.96     2.56   3.30
# 4    4.70        0.20     4.80     1.28     0.82   4.49
# 5    3.95        1.44     6.47     1.59     2.56   3.00
# 6    1.27        0.05     0.25     3.47     5.78   0.83
# 
# Interesting to see that politics and religion use topic 6 (religion) heavily
# while all other topics dont. Sports (and science) seem to be built by a broad
# mix of topics. Computer and classifieds are similar in their topic distribution,
# which might be because people on the newsgroups dataset mostly inserted computer
# parts.

# See which newsgroups are similar by their used topics:
crosstab <- crosstab / rep(colSums(crosstab), each = 6)
(dist(t(crosstab), method = "manhattan"))
#               science classifieds  computer  politics  religion
# classifieds 0.7338443                                          
# computer    0.3825103   0.3837687                              
# politics    0.6002787   1.0606227 0.9375033                    
# religion    0.7390918   0.9633820 0.8547699 0.5732601          
# sports      0.2747356   0.8439619 0.4631793 0.8038738 0.8977072
# 
# Similar as above, classifieds and computer are similar, maybe also science.
# Plitics and religion are similar. Religion differs highly from all other 
# newsgroups.