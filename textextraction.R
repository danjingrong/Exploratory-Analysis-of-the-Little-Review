 setwd("/Users/danjing/Downloads/project 3")
install.packages("XML")
install.packages("tm")
library(XML)
library(tm)
directory <- "LR"
directory
files.v <- dir(path=directory, pattern = ".*xml")
documents.l <- list()
for(i in 1:length(files.v)){
  document <- xmlTreeParse(file.path(directory, files.v[i]), useInternalNodes = TRUE)
  documents.l[[files.v[i]]] <- getNodeSet(document, "/tei:TEI//tei:div[@type='fiction']", namespaces = c(tei="http://www.tei-c.org/ns/1.0"))
}
#delete issues without fiction
delete<-c(1:10,14,23,27,28,30,31,35,41,47)
withfictions<-documents.l[-delete]

fictions<-vector()
for (i in length(withfictions)){
  fictions.word.v <- paste(sapply(withfictions, xmlValue), collapse = " ")
  fictions[[i]] <- tolower(fictions.word.v)
}

class(fictions)
write.csv(fictions, file="/Users/danjing/Downloads/project 3/fictionsplaintext")
#fictions <- read.csv("/Users/danjing/Downloads/project 3/fictionsplaintext")
fictions.all <- paste(fictions, collapse=" ", sep="\n")

#extracted all fictions! now text mining
fictions.corpus <- Corpus(VectorSource(fictions.all))
fictions.corpus = tm_map(fictions.corpus, removeWords, stopwords("english"))

writeCorpus(fictions.corpus, filenames = "nostopwords.txt")
lrfiction<- scan("nostopwords.txt", what="character", sep="\n")
lrfiction<-paste(lrfiction, collapse = " ")
lrfiction.words<-strsplit(lrfiction,"\\W") #splitintowords
lrfiction.words<-unlist(lrfiction.words) #unlist
lrfiction.words <- lrfiction.words[which(lrfiction.words!="")]
lrfiction.words[1:10]#first 10 words
#frequency of words
lrfiction.freq <- table(lrfiction.words)
length(lrfiction.freq)
lrfiction.freq <- lrfiction.freq[140:28233]
lrfiction.freq.sort <- sort(lrfiction.freq, decreasing = TRUE)
lrfiction.freq.sort[1:20]
plot(lrfiction.freq.sort[1:20])

write.table(lrfiction.freq.sort1, "/Users/danjing/Downloads/project 3/top30df.")

#ggplot
library(ggplot2)
lrfiction.freq.sort1<-as.data.frame(lrfiction.freq.sort)
lrfiction.freq.sort1<-lrfiction.freq.sort1[1:30,]
colnames(lrfiction.freq.sort1)<- c("word","frequency")
p<-ggplot(data=lrfiction.freq.sort1, aes(x=word, y=frequency)) +geom_text(aes(label=frequency), vjust=-0.7, color = "red", position = position_dodge(0.5), size=3.5)+
  geom_bar(stat="identity") + ggtitle("Top 30 Word Frequencies in fictions of the Little Review 1914-1922")
p
ggsave("top30.pdf",plot=last_plot(),width = 1000, height = 800)

ggplot(data = lrfiction.freq.sort1 + aes(x=word, y=frequency))