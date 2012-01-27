require(RCurl)
library(tm)
library(wordcloud)
library(ggplot2)
require(RColorBrewer)

mydata<-getURL("http://api.wefeelfine.org:8080/ShowFeelings?display=txt&returnfields=sentence&limit=1500&country=spain")

myCorpus <- Corpus(VectorSource(mydata))

myCorpus <- tm_map(myCorpus, tolower)

myCorpus <- tm_map(myCorpus, removePunctuation)

myCorpus <- tm_map(myCorpus, removeNumbers)

myStopwords <- c( stopwords('english'),"feeling", "feel","I´m","http","www","href","dont","ive","don","feelings","feel","feels","didnt","cant","ain")

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument)

myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

findFreqTerms(myDtm, lowfreq=10)




m <- as.matrix(myDtm)

v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)

d <- data.frame(word=myNames, freq=v)
pal2 <- brewer.pal(8,"Set2")
pdf("/Users/sciruela/Documents/tagclouds-emotions/emotions-tagclouds-spain.pdf")
wordcloud(d$word, d$freq, min.freq=10, max.words=Inf, random.order=F, rot.per=.3, colors=pal2)
dev.off()

myfreq<-v[1:10]
mywords<-names(v)[1:10]
myletters<-c("A","B","C","D","E","F","G","H","I","J")
df=data.frame(letters=myletters,words=mywords,freq=myfreq)


pdf("/Users/sciruela/Documents/tagclouds-emotions/frecuencies-top-ten-emotions-tagclouds-spain.pdf")
ggplot(data=df)+geom_bar(aes(x=letters,y=freq,colors=letters,fill=letters),binwidth=1)+theme_bw()+scale_x_discrete(breaks=df$letters,labels=df$words)+opts(legend.position = "none")
dev.off()