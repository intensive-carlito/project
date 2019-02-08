library(data.table)
library(httr)
library(textcat)

translate <- function(str) {
  r <- POST("https://translate.yandex.net/api/v1.5/tr.json/translate",
            body = list(
              key = "trnsl.1.1.20181214T151349Z.e323c6a0eeb6c59d.6ed57788f95d2a05d4269fddb847986f8769b990",
              text = str,
              lang = "en",
              format = "plain"
            ),
            encode = "form",
            verbose())
  print(content(r)$lang)
  print(content(r)$text)
  c(content(r)$lang, content(r)$text)
}

comments <- fread("./input/reviews.csv")
to_translate <- comments[,.N, by=.(comments)][order(lang)]
to_translate[, lang:=textcat(comments)]
to_translate[lang=="estonian", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="finnish", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="breton", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="english", comment_in_english:=comments, by=comments]
to_translate[is.na(comment_in_englishlang), comment_in_english:=translate(comments)[2], by=comments]
ordered_to_translate <- to_translate[order(lang)]
unique(ordered_to_translate[lang!="english", .(lang)])
esperanto <-ordered_to_translate[lang == 'esperanto']
estonian <- ordered_to_translate[lang == 'estonian']
finnish <- ordered_to_translate[lang == 'finnish']
french <- ordered_to_translate[lang == 'french']
fwrite(x = french, "french.csv")

# Une requête permettant d'avoir du bon et du moins bon
french_tmp <- french[comments %like% "nul"]
french_tmp <- french_tmp[,comment_in_english:=NA]
french_tmp <- french_tmp[,.(comments, N, lang)]

## Tentative de text mining sur du français

# On commence par remplir le corpus
library(tm)
documents <- Corpus(VectorSource(french_tmp$comments))
documents[1]$content

library(stringi)
library(stringr)

# On commence par retirer les accents du texte
accent <- function(x) stri_trans_general(x, "Latin-ASCII")
documents <- tm_map(documents, content_transformer(accent))

# On ne garde que les caractères de l'alphabet et les chiffres.
documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z0-9]", replacement = " ")

# On passe tout en minuscules
documents <- tm_map(documents, content_transformer(tolower))

# On recrée les stopwords en leur enlevant les accents.
stopwords_fr <- sapply(stopwords("french"),accent)
stopwords_fr

# On nettoie notre document en lui enlevant tous ces stopwords
documents <- tm_map(documents, removeWords, stopwords_fr)

# Les textes sont sujet aux fautes d'orthographe. On racinise pour essayer d'uniformiser les textes.
library('SnowballC')
documents_nonstem <- documents
documents <- tm_map(documents, stemDocument, "french")

# Les mots sont séparés par plein d'espaces, on fait du nettoyage
documents <- tm_map(documents, stripWhitespace)

# On vectorise les documents
dtm <- DocumentTermMatrix(documents)
dim(dtm)

# Il est possible d'ignorer les mots apparaissant moins d'un certain seuil avec
# minfreq <- findFreqTerms(dtm, 10) Ici on ne garde que les mots apparaissant au moins 10 fois.

# Là, c'est l'application de la réduction, mais pour le moment, je ne la fais pas...
# dtm <- DocumentTermMatrix(documents, control=list(dictionary = minfreq, weighting=weightTfIdf))

dtm_m = as.matrix(dtm)
counts <- rowSums(dtm_m)
hist(counts,breaks=50)

library(wordcloud2)

wordsFreq <- sort(colSums(dtm_m),decreasing = TRUE)
wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
wordcloud2(data = wordsFreq[1:50,],minSize = 1, size = 3)

library(skmeans)
tdm <- TermDocumentMatrix(documents)
dim(tdm)

start_time=Sys.time()
sk <- skmeans(x=tdm, 6) # on a choisi de voir ce qui ressortait pour 6 classes (pour voir si on retrouve les 6 thèmes utilisés pour classer les articles)
## proportion par classe:
print(paste("skmeans tourne en ",round(difftime(Sys.time(),start_time,units="secs"),1),"secondes"))
table(sk$cluster)/sum(table(sk$cluster))

for (i in 1:6){
  print(paste('cluster ',i,sep=' '))
  print(names(sk$cluster)[sk$cluster==i][1:50])
  print('---------------')
}

dtmss <- removeSparseTerms(dtm, 0.99)   
sk <- skmeans(x=dtmss, 6)
## On regarde la répartition dans les clusters
table(sk$cluster)/sum(table(sk$cluster))

dtm_m <- as.matrix(dtm)
for (i in 1:6){
  m <- dtm_m[sk$cluster==i,]
  wordsFreq <- sort(colSums(m),decreasing = TRUE)
  wordsFreq <- data.frame(word = names(wordsFreq), Freq = as.vector(wordsFreq)) # mise au format pour wordcloud2
  wordcloud2(data = wordsFreq[1:500,],minSize = 1, size = 3)
}

library(cluster)  
library(proxy)
d <- dist(dtm_m, method="cosine")   #attention quand dist vient de proxy, elle veut une matrice en argument
fit <- hclust(d=d, method="ward.D")   
fit  

plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red")


###############################
# text2vec

library(text2vec)
wiki = readLines("~/Projets/exos-R/text-mining/formation tm SC-PC/text8", n = 1, warn = F)
tokens = space_tokenizer(wiki)

# creation d'un vocabulaire de 1-grammes
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
# on supprime les n-grammes rares
vocab = prune_vocabulary(vocab, term_count_min = 5L)
vectorizer = vocab_vectorizer(vocab)
# co-occurrence avec un contexte de taille 5
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

# Il est temps de factoriser la matrice TCM avec l'algorithme GloVe
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 50, convergence_tol = 0.01)
save(wv_main,glove,file="GloVe.RData")
dim(wv_main)

wv_context = glove$components
dim(wv_context)

#On ajoute les deux matrices qui sont transposées l'une de l'autre
# A creuser mais ça donne de meilleurs résultats

word_vectors = wv_main + t(wv_context)

berlin = word_vectors["paris", , drop = FALSE] - 
  word_vectors["france", , drop = FALSE] + 
  word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)
