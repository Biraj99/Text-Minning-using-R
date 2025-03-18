

# Read the file
text <- read.csv("clipboard", sep = "\t", header = TRUE)


# Load the data as a corpus

TextDoc<-Corpus(VectorSource(text$Review.Text))





# Get the default stopword list
stopwords()
default_stopwords <- stopwords()
words_to_exclude <- c("not", "doesn't")

# Create a custom stopwords list by removing words to exclude
custom_stopwords <- setdiff(default_stopwords, words_to_exclude)
custom_stopwords

# Update the TextDoc by setting custom stopwords
TextDoc <- tm_map(TextDoc, removeWords, custom_stopwords)
#####################################################################################

#Replacing "/", "@" and "|" with space

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "!")
TextDoc <- tm_map(TextDoc, toSpace, "#")
TextDoc <- tm_map(TextDoc, toSpace, "%")


# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)

# Remove english common stopwords
stopwords()
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))# already done
stopwords()

# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("dresses","dress","top", "not", "wear", "general", "petit", "just", "this", "one", "will", "well", "can", "back", "bit", "shirt","sizes")) #### some example
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
?stemDocument

##########Building the term document matrix#######
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)

dtm_m <- as.matrix(TextDoc_dtm)
class(dtm_m)

# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_v
?names
dtm_d <- data.frame(words = names(dtm_v),freq = dtm_v)
class(dtm_d)

# Display the top 10 most frequent words
head(dtm_d, 20)# helps in printing the top 20 words
tail(dtm_d, 10)

library(dplyr)


# Plot the most frequent words
?barplot ## will give you help 
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$words,
        col = rainbow(20), main ="Top 20 most frequent words",
        ylab = "WORD FREQUENCIES")### las shows legacy axis = 2 vertical

?barplot

#generate word cloudlibrary(wordcloud2)
install.packages("wordcloud2")
library(wordcloud2)
??wordcloud2
wordcloud2(dtm_d,
           size = .4,
           shape = 'Circle',
           rotateRatio = 0.9,
           minSize = .5)

#####################
# Find associations 

findAssocs(TextDoc_dtm, terms = c("like","love", "fit"), corlimit = 0.15)

?findAssocs


# Find associations for words that occur at least 2000 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 2000), corlimit = 0.15)

