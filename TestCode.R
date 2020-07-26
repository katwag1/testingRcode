#install.packages("stm")
#install.packages("quanteda")
#install.packages("readtext") 
#install.packages("SnowballC")
#install.packages("textreadr")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("tidytext")
#install.packages("tokenizers")
#install.packages("tm")
#install.packages("geometry")
#install.packages("rsvd")
#install.packages("Rtsne")
#install.packages("furrr")
#install.packages("future")
#install.packages("wordcloud")
install.packages("ggplot2",dependencies = TRUE)
if (!require(devtools)) install.packages("devtools")
install.packages("cli")
devtools::install_github("mikajoh/tidystm", dependencies = TRUE)

library(stm)
library(quanteda)
library(readtext)
library(textreadr)
library(stringr)
library(dplyr)
library(tidyverse)
library(data.table)
library(tidytext)
library(tokenizers)
library(tm)
library(geometry)
library(rsvd)
library(Rtsne)
library(furrr)
library(future)
library(tidystm)
library(stringi)


DATA <- '/Users/katwagener/Documents/Dissertation/Practice/SubsetDataTest'
texts <-readtext(DATA)
files <- list.files(DATA, recursive=TRUE) 
metadataraw <- data.table(str_match(files,"([\\w\\s]+)\\/(\\d+)\\/(\\w+)\\/(.+)")[,2:5])
metadataraw <- metadataraw %>% 
  rename(
    newspaper = V1,
    year = V2,
    month = V3, 
    doc_id = V4
  )
common_column_names <- intersect(names(texts), names(metadataraw))
articles_metadata <- merge(texts, metadataraw, by=common_column_names, all.dataframe_1=TRUE)
articles_metadata$text <- sapply(str_split(articles_metadata$text, coll("Body")), "[[", 2)
articles_metadata$text <- sapply(str_split(articles_metadata$text, coll("Load-Date")), "[[", 1)
print(articles_metadata$text[1])
print(Sys.time())

#build corpus
corpus <- corpus(articles_metadata)
summary(corpus, showmeta = TRUE, n = 3)
print(Sys.time())

#determine bigrams
collocations <- corpus %>% 
  tokens() %>% 
  tokens_remove(pattern = stopwords("english"), padding = TRUE) %>% 
  textstat_collocations(
    method = "lambda", 
    size = 2,
    min_count = 2,
    smoothing = 0.5,
    tolower = TRUE) 
print(Sys.time())

toks1<- tokens(corpus, remove_punct = TRUE)
tokslower <- tokens_tolower(toks1)
toks2 <- tokens_remove(toks1, c(stopwords("english"),"like","just","also", "can","us"))
toks3 <- tokens_ngrams(toks2, 2)

unique_bigrams <- str_replace(collocations$collocation, pattern=' ',replacement='_')
toks4 <- tokens_select(toks3, unique_bigrams, selection = 'keep')

#build dfm
dfm <- dfm(corpus, remove = c(stopwords("english"),"like","just","also", "can","us", "get", "mr", "publication-typ"), stem = TRUE, remove_punct=TRUE, remove_numbers = FALSE) 
dfmtoks <- dfm(toks4, remove = c(stopwords("english"),"like","just","also", "can","us", "get", "mr", "publication-typ"))
tokenised_dfm <- cbind(dfm,dfmtoks)
dfmatrix <- dfm_trim(tokenised_dfm, min_termfreq = 10, termfreq_type = "count", max_docfreq = 0.9, docfreq_type = "prop")
topfeatures(dfmatrix, 50)
docvars(dfmatrix)
print(Sys.time())

#STM estimation
newspaper_stm <- stm(documents = dfmatrix,
                     K = 10,
                     init.type = "Spectral")

dfm2stm <- convert(dfmatrix, to = "stm", docvars = docvars(dfmatrix))


labelTopics(newspaper_stm, topics = c(1,3,10), n = 10)

#findthoughts
docs1 <- articles_metadata$text
docs2 <- substr(docs1, start = 1, stop = 200)
thought2 <- findThoughts(newspaper_stm, texts=docs2, topics=c(1,3,10), n=3)
print(thought2)


