######################################################
######################################################
## Script name:  A3: Business Insight Report                      
##                                                                                                              ##  
## Purpose of script: MBAN2 Hult 2021                                           
##                                                                                                              ##
## Author: Abdelmohssin Elyounss                                                  
##                                                                                                             ## 
## Date Created: 12-01-2021                                                             
##                                                                                                            ##  
##                                                                                                            ##   
## Email: melyounssi2020@student.hult.edu                                
##                                                                                                           ##  
## ------------------------------------------------------

################################################################################
############        Importing my 10  dream jobs target     #####################
############                  Text Format                  #####################             
#####################     Using DTM, VCorpus  Frameworks   #####################
################################################################################

############  My 10 dream Block chain Analyst jobs target 

#install.packages("textreadr")
library(textreadr)
#Importing all .txt files from one directory #
setwd("C:\\Users\\elyou\\Desktop\\Blockchain crypto\\Blockchain\\Text")
nm <- list.files(path="C:\\Users\\elyou\\Desktop\\Blockchain crypto\\Blockchain\\Text")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_blockchain <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

library(stringr)  
my_blockchain <-str_replace_all(my_blockchain, "[^[:alnum:]]", " ")

############  My 10 dream Cryoto Analyste  jobs target 


#install.packages("textreadr")
library(textreadr)
#Importing all .txt files from one directory #
setwd("C:\\Users\\elyou\\Desktop\\Blockchain crypto\\Crypto\\Text")
nm <- list.files(path="C:\\Users\\elyou\\Desktop\\Blockchain crypto\\Crypto\\Text")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_crypto <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

library(stringr)  
my_crypto <-str_replace_all(my_crypto, "[^[:alnum:]]", " ")

############ Cleaning and creating  the Block chain corpus 

######## Source and create the corpus Block chain 

library(tm)

my_blockchain_v <- VectorSource(my_blockchain)

my_blockchain_corp <- VCorpus(VectorSource(my_blockchain))


##### Cleaning the corpus Block chain 

#install.packages("textcat")
library(textcat)
library(subspace)
#### Clean the corpus with **tm_map**
 clean_corpus <- function(corpus) {
  # Add more stopwords
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "crypto", "blockchain",'will'))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Remove Numbers
  #corpus <- tm_map(corpus,  removeNumbers)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Remove special characters 
  #corpus <- tm_map(corpus, subSpace, "\\|%&*#+_><")
  #corpus <- tm_map(corpus, toSpace, "???") 
  return(corpus)
 }
 
 
my_blockchain_corp_cleaned   <- clean_corpus(my_blockchain_corp)

my_blockchain_corp_cleaned

content(my_blockchain_corp_cleaned[[1]])
 
######## cleaning and creating  the Crypto corpus 

######## Source and create the corpus Crypto 
 
 library(tm)

my_crypto_v <- VectorSource(my_crypto) 


my_crypto_corp <- VCorpus(VectorSource(my_crypto))
 
##### Cleaning the corpus Crypto 
 
 my_crypto_corp_cleaned   <- clean_corpus(my_crypto_corp)
 
 
 content(my_crypto_corp_cleaned[[1]])
 
 ###### Creating DTM Block chain 
 
 
 # Create the document-term matrix from the corpus
 my_blockchain_dtm <- DocumentTermMatrix(my_blockchain_corp_cleaned)
 
 
 # Print out my_blockchain_dtm data
 my_blockchain_dtm
 
 # Convert my_blockchain_dtm  to a matrix
 my_blockchain_m <- as.matrix(my_blockchain_dtm)
 
 ###### Creating DTM Crypto 
 
 # Create the document-term matrix from the corpus
 my_crypto_dtm <- DocumentTermMatrix(my_crypto_corp_cleaned)
 
 # Print out my_crypto_dtm data
 
 my_crypto_dtm
 
 # Convert my_blockchain_dtm  to a matrix
 my_crypto_m <- as.matrix(my_crypto_dtm)
 
 ### Top 20 frequent term with tm 
 
 # Calculate the row sums of blockchain 
 term_frequency_bc <- colSums(my_blockchain_m)
 
 # Sort term_frequency in decreasing order
 term_frequency_bc <- sort(term_frequency_bc, decreasing = TRUE)
 
 # View the top 10 most common words
 term_frequency_bc[1:20]
 
 # Plot a barchart of the 20 most common words Block chain 
 barplot(term_frequency_bc[1:20], col = "green", las = 2)
 
 
 ### Plot a word cloud of block chain 
 #install.packages("wordcloud")
 library(wordcloud)
 
 
 wordcloud(names(term_frequency_bc),term_frequency_bc,min.freq = 1,
           max.words=500, random.order=FALSE, rot.per=0.40, 
           colors=brewer.pal(8, "Dark2"))
 
 # Calculate the row sums of Crypto 
 term_frequency_cr <- colSums(my_crypto_m)
 
 # Sort term_frequency in decreasing order Crypto 
 term_frequency_cr <- sort(term_frequency_cr, decreasing = TRUE)
 
 # View the top 30 most common words Crypto 
 term_frequency_cr[1:30]
 
 # Plot a bar chart of the 30 most common words Crypto 
 barplot(term_frequency_cr[1:30], col = "green", las = 2)
 
###### Cloud word Crypto 
 
 library(wordcloud)
 
 #Plot a word cloud of 
 wordcloud(names(term_frequency_cr),term_frequency_cr,min.freq = 1,
           max.words=100, random.order=FALSE, rot.per=0.40, 
           colors=brewer.pal(8, "Dark2"))

 ################################################################################
 ############        Importing my 10  dream jobs target     #####################
 ############                  PDF  Format                  #####################             
 #####################     Using Bi-Gram IF-DTF  Frameworks #####################
 ################################################################################
 
 ############  My 10 dream Block chain Analyst jobs target ##################### 
 
 #install.packages("pdftools")
 library(pdftools) 
 setwd("C:/Users/elyou/Desktop/Blockchain crypto/Blockchain/pdf")
 nmb <- list.files(path="C:/Users/elyou/Desktop/Blockchain crypto/Blockchain/pdf")
 my_pdf_text <- do.call(rbind, lapply(nmb, function(x) pdf_text(x)))
 
 ##### Transforming the pdf  to text
 #install.packages("textreadr") 
 library(textreadr)
 
 pdf <- read_document(file=nmb ) # Vector,list of strings
 pdf_together <- paste(pdf, collapse = " ") # Concatenated vector, one string
 pdf_text <- do.call(rbind, lapply(nmb, function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
 blockchain <- data.frame(pdf_text)
 colnames(blockchain) <- "text"
 
 ############      Analizing Blockchain dream jobs  - PDF            ###########
 
 
 ###################################################
 ################# Bi-Gram  ########################
 ###################################################
 
 
 
 library(dplyr)
 library(tidytext)
 library(tidyr)
 
 blockchain_bigrams <- blockchain %>%
   unnest_tokens(bigram, text, token = "ngrams", n=2)
 
 blockchain_bigrams #We want to see the bigrams (words that appear together, "pairs")
 
 blockchain_bi_fqr <- blockchain_bigrams %>%
   count(bigram, sort = TRUE) #this has many stop words, need to remove them 
 
 #to remove stop words from the bigram data, we need to use the separate function:
 library(tidyr)
 bigrams_separated_blockchain <- blockchain_bigrams %>%
   separate(bigram, c("word1", "word2"), sep = " ")
 
 bigrams_filtered_blockchain <- bigrams_separated_blockchain %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word)
 
 #creating the new bigram, "no-stop-words":
 bigram_counts_blockchain <- bigrams_filtered_blockchain %>%
   count(word1, word2, sort = TRUE)
 #want to see the new bigrams
 bigram_counts_blockchain
 
 # creating a ggraph to see the connections between the different words
 
 library(igraph)
 
 bigram_graph_blockchain <- bigram_counts_blockchain %>%
   filter(n>2) %>%
   graph_from_data_frame()
 
 bigram_graph_blockchain
 
 library(ggraph)
 
 ggraph(bigram_graph_blockchain, layout = "fr")+
   geom_edge_link()+
   geom_node_point()+
   geom_node_text(aes(label=name), vjust =1, hjust =1)
 
 ###################################################
 ################# TF_IDF Block chain ##############
 ###################################################
 
 #we're grouping by the company 
 
 company <- c("AE Studio","Alumni Venture", "coibase","cryptocom","deloitte",
              "Goodwin","Lamda School","moodys","Motion Recuitument", "rsf" )
 
 blockchain_comp <- cbind(company,blockchain)
 # Tokenization 
 
 blockchain_token <- blockchain_comp %>%
   unnest_tokens(word, text) %>%
   count(company, word, sort=TRUE) %>%
   ungroup()
 
 total_words <- blockchain_token %>%
   group_by(company) %>%
   summarise(total=sum(n))
 
 blockchain_words <- left_join(blockchain_token, total_words)%>%
   filter(company %in% c("coinbase", "moodys", "deloitte"))
 
 print(blockchain_words)
 
 company_words <- blockchain_words %>%
   bind_tf_idf(word, company, n)
 # we get all the zeors because we are looking at stop words 
 company_words 
 
 company_words %>%
   arrange(desc(tf_idf))
 
 # looking at the graphical apprach:
 
 company_words %>%
   arrange(desc(tf_idf)) %>%
   mutate(word=factor(word, levels=rev(unique(word)))) %>%
   group_by(company) %>%
   top_n(15) %>%
   ungroup %>%
   ggplot(aes(word, tf_idf, fill=company))+
   geom_col(show.legend=FALSE)+
   labs(x=NULL, y="tf-idf")+
   facet_wrap(~company, ncol=2, scales="free")+
   coord_flip()
 
 ############      Analizing Crypto  dream jobs  - PDF            ###########
 
 
 #install.packages("pdftools")
 library(pdftools) 
 setwd("C:/Users/elyou/Desktop/Blockchain crypto/Crypto/pdf")
 nmc <- list.files(path="C:/Users/elyou/Desktop/Blockchain crypto/Crypto/pdf")
 my_pdf_text <- do.call(rbind, lapply(nmc, function(x) pdf_text(x)))
 
 ##### Transforming the pdf  to text
 #install.packages("textreadr") 
 library(textreadr)
 
 pdf <- read_document(file=nmc ) # Vector,list of strings
 pdf_together <- paste(pdf, collapse = " ") # Concatenated vector, one string
 pdf_text <- do.call(rbind, lapply(nmc, function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
 crypto <- data.frame(pdf_text)
 colnames(crypto) <- "text"
 
 ################## Analizing Object Crypto - PDF ##############################
 
 ###################################################
 ################# Bi-Gram Crypto ## ###############
 ###################################################
 
 
 crypto_bigrams <- crypto %>%
   unnest_tokens(bigram, text, token = "ngrams", n=2)
 
 crypto_bigrams #We want to see the bigrams (words that appear together, "pairs")
 
 crypto_bi_fqr <-  crypto_bigrams %>%
   count(bigram, sort = TRUE) #this has many stop words, need to remove them 
 
 #to remove stop words from the bigram data, we need to use the separate function:
 library(tidyr)
 bigrams_separated_crypto <- crypto_bigrams %>%
   separate(bigram, c("word1", "word2"), sep = " ")
 
 bigrams_filtered_crypto <- bigrams_separated_crypto %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word)
 
 #creating the new bigram, "no-stop-words":
 bigram_counts_crypto <- bigrams_filtered_crypto %>%
   count(word1, word2, sort = TRUE)
 #want to see the new bigrams
 bigram_counts_crypto
 
 # creating a ggraph to see the connections between the different words
 
 library(igraph)
 
 bigram_graph_crypto <- bigram_counts_crypto %>%
   filter(n>2) %>%
   graph_from_data_frame()
 
 bigram_graph_crypto
 
 library(ggraph)
 
 ggraph(bigram_graph_crypto, layout = "fr")+
   geom_edge_link()+
   geom_node_point()+
   geom_node_text(aes(label=name), vjust =1, hjust =1)
 
 
 ################################################################################
 ############        Importing my resume                  #####################
 ############                  PDF  Format                  #####################             
 #####################     Using Bi-Gram                    #####################
 ################################################################################
 
 #install.packages("pdftools")
 library(pdftools) 
 setwd("C:/Users/elyou/Desktop/Blockchain crypto/my_resume/pdf")
 nmr <- list.files(path="C:/Users/elyou/Desktop/Blockchain crypto/my_resume/pdf")
 my_pdf_text <- do.call(rbind, lapply(nmr, function(x) pdf_text(x)))
 
 ##### Transforming the pdf  to text
 #install.packages("textreadr") 
 library(textreadr)
 
 pdf <- read_document(file=nmr ) # Vector,list of strings
 pdf_together <- paste(pdf, collapse = " ") # Concatenated vector, one string
 pdf_text <- do.call(rbind, lapply(nmr, function(x) paste(read_document(file=x), collapse = " "))) # each string for each file
 my_resume <- data.frame(pdf_text)
 colnames(my_resume) <- "text"
 
 
 
 ################## Analizing Object my_resume - PDF ##########################
 
 token_list <- my_resume %>%
   
   unnest_tokens( word , text)#no punctutation, no upper case letters
 
 print(token_list)
 
 
 ########## token frequencies
 
 
 frequencies_tokens <- my_resume %>%
   
   unnest_tokens( word , text) %>%
   count(word, sort = TRUE)
 
 print(frequencies_tokens)
 
 
 ########### stop words 
 
 #stop words are words that are commonly used in English 
 # e.g. is, I, are, you, me, the, of, etc.
 #we will use the anti_join(stop_words) to remove the stop words
 #install.packages("stringr")
 
 library(stringr)
 
 data(stop_words)
 
 frequencies_tokens_nstop <- my_resume %>% 
   
   unnest_tokens( word , text) %>%
   anti_join(stop_words) %>%
   count(word, sort = TRUE)
 
 
 print(frequencies_tokens_nstop) 
 
 
 # View the top 10 most common words
 my_frq <- frequencies_tokens_nstop[1:20,]
 
 my_frq
 
 # Plot a barchart of the 10 most common words
 
 counts <- table(my_frq$word)
 barplot(counts, main="My resume", xlab="words",col = "green", las = 2)
 
 
 
 ###### creating Bi-grame my_resume 
 
 my_resume_bigrams <- my_resume %>%
   unnest_tokens(bigram, text, token = "ngrams", n=2)
 
 #We want to see the bigrams 
 my_resume_bigrams 
 
 #this has many stop words, need to remove them 
 my_resume_bi_fqr <-  my_resume_bigrams %>%
   count(bigram, sort = TRUE) 
 
 #to remove stop words from the bigram data, we need to use the separate function:
 library(tidyr)
 bigrams_separated_my_resume <- my_resume_bigrams %>%
   separate(bigram, c("word1", "word2"), sep = " ")
 
 bigrams_filtered_my_resume <- bigrams_separated_my_resume %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word)
 
 #creating the new bigram, "no-stop-words":
 bigram_counts_my_resume <- bigrams_filtered_my_resume %>%
   count(word1, word2, sort = TRUE)
 #want to see the new bigrams
 bigram_counts_my_resume
 
 # creating a ggraph to see the connections between the different words
 
 library(igraph)
 
 bigram_graph_my_resume <- bigram_counts_my_resume %>%
   filter(n>1) %>%
   graph_from_data_frame()
 
 bigram_graph_my_resume
 
 library(ggraph)
 
 ggraph(bigram_graph_my_resume, layout = "fr")+
   geom_edge_link()+
   geom_node_point()+
   geom_node_text(aes(label=name), vjust =1, hjust =1)
 
 
 ################################################################################
 ############                  PDF  Format                  #####################             
 ##################   Using Sentiment analysises Frameworks  ###################
 ################################################################################

 
 library(tidytext)
 library(janeaustenr)
 library(dplyr)
 library(stringr)
 library(tidyr)
 library(tidytuesdayR)
 
 ############################################################
 ##### Comparing different sentiment libraries on Block chain
 ############################################################
 
 
 get_nrc_sentiment(blockchain, cl = NULL, language = "english", lowercase = TRUE)
 

 
 ##################### Sentiment Analysis for Block chain  #####################
 
 
 # regular sentiment score using get_sentiment() 
 # each method has a different scale
 
 library(tidytext)
 library(textdata)
 library(syuzhet)
 library(bing)
 
 
 # bing method
 bing_vector_blockchain <- get_sentiment(my_blockchain_corp_cleaned, method="bing")
 head(bing_vector_blockchain)
 summary(bing_vector_blockchain)
 
 #affin method
 afinn_vector_blockchain <- get_sentiment(my_blockchain_corp_cleaned, method="afinn")
 head(afinn_vector_blockchain)
 summary(afinn_vector_blockchain)
 
 #nrc method
 
 d_blockchain <- get_nrc_sentiment(as.vector(as.character(my_blockchain_corp_cleaned))) 
 
 # To see top 20 lines of the get_nrc_sentiment dataframe
 head (d_blockchain,20)
 
 #transpose
 td_blockchain <- data.frame(t(d_blockchain))
 # Dimension of the DF
 dim(td_blockchain)
 # Computing the sums across rows for each variable.
 td_new_blockchain <- data.frame(rowSums(td_blockchain[1:10,]))
 #Cleaning
 names(td_new_blockchain)[1] <- "count"
 td_new_blockchain <- cbind("sentiment" = rownames(td_new_blockchain), td_new_blockchain)
 rownames(td_new_blockchain) <- NULL
 td_new2_blockchain <- td_new_blockchain[1:20,]
 #Plot 
 quickplot(sentiment, data=td_new2_blockchain, weight=count, geom="bar",
           fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
 
 
 
 
 ##################### Sentiment Analysis for Crypto  #########################
 
 
 
 # bing method
 bing_vector_crypto <- get_sentiment(my_crypto_corp_cleaned, method="bing")
 head(bing_vector_crypto)
 summary(bing_vector_crypto)
 
 #affin method
 afinn_vector_crypto <- get_sentiment(my_blockchain_corp_cleaned, method="afinn")
 head(afinn_vector_crypto)
 summary(afinn_vector_crypto)
 
 
 #nrc method
 
 d_crypto <- get_nrc_sentiment(as.vector(as.character(my_crypto_corp_cleaned))) 
 
 # To see top 20 lines of the get_nrc_sentiment dataframe
 head (d_crypto,20)
 
 #transpose
 td_crypto <- data.frame(t(d_crypto))
 # Dimension of the DF
 dim(td_crypto)
 # Computing the sums across rows for each variable.
 td_new_crypto <- data.frame(rowSums(td_crypto[1:10,]))
 #Cleaning
 names(td_new_crypto)[1] <- "count"
 td_new_crypto <- cbind("sentiment" = rownames(td_new_crypto), td_new_crypto)
 rownames(td_new_crypto) <- NULL
 td_new2_crypto <- td_new_crypto[1:20,]
 #Plot 
 quickplot(sentiment, data=td_new2_crypto, weight=count, geom="bar",
           fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 