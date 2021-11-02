library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#
# The next line of code contains a list of words frequently appearing in patents which meaning is unrelated to the nature
# of the inveniton, thus they should be eliminated
#
myStopwords <- c("problem", "solved", "present", "disclosure", "disclose", "discloses", "directed", "means", "invention", "relates", "related", "method", "provided", "embodiment", "embodiments", "system", "can", "device", "solution", "copyright", "function", "apparatus", "based", "device", "provide", "used", "use", "using", "capable", "one", "jpo", "may", "inlcude", "includes", "key", "first", "etc",  "via", "including", "comprises", "comprising", "according", "example", "thus", "like", "also", "without", "number", "lower", "made", "second", "unit", "least", "said", "member", "plurality", "user", "corresponding", "provides", "associated", "include", "methods", "anti", "composition")
#
docs = Corpus(VectorSource(Abstract))
#docs <- VCorpus(VectorSource(Abstract))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# specify your stopwords as a character vector and remove them
docs <- tm_map(docs, removeWords, myStopwords)
#
dtm <- DocumentTermMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)



#frequencies
freq <- colSums(as.matrix(dtm)) 
ord <- order(freq) 
m <- as.matrix(dtm)   
freq <- colSums(as.matrix(dtm)) 
freq.terms <- findFreqTerms(dtm, lowfreq=25)


# Network
library(graph)
library(Rgraphviz)
LabelColor <- "light green"
EdgeColor <- "blue" #alternative "orange"
LabelShape <- "ellipse" #alternative "box"
NetworkKind <- "twopi" #alternative "neato"
CorrThreshold <- 0.15  #Threshold of minumum correlation value
FontSize <- "20"
freq.terms <- findFreqTerms(dtm, lowfreq=120)
(length(freq.terms))
plot(dtm, term = freq.terms, corThreshold = CorrThreshold, weighting = T, list(node = list(label = "foo", fillcolor = LabelColor ,fontsize=FontSize, shape = LabelShape, fixedsize = T, height = "0.5", width ="1"), edge = list(color = EdgeColor), graph = list(rankdir = "LR")), NetworkKind)
#

#Clustering by Term Similarity
library(cluster)
#dtmss <- removeSparseTerms(dtm, 0.15)
dtms <- removeSparseTerms(dtm, 0.95)
d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="average")   # for a different look try substituting: method="ward.D"
plot(fit, hang=-1) 
rect.hclust(fit, k = 30)

#Find relationships between words
findAssocs(dtm, "form", corlimit=0.20)
as.data.frame(findAssocs(dtm, "form", corlimit=0.20))
#
# example
#
air <- data.frame(air = findAssocs(dtm, "air", corlimit=0.20)[[1]][1:20])
formed <- data.frame(formed = findAssocs(dtm, "formed", corlimit=0.20)[[1]][1:20])

