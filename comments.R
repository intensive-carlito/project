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
to_translate <- comments[,.N, by=.(comments)]
to_translate[, lang:=textcat(comments)]
to_translate[lang=="estonian", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="finnish", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="breton", comment_in_english:=translate(comments)[2], by=comments]
to_translate[lang=="english", comment_in_english:=comments, by=comments]
to_translate[is.na(comment_in_englishlang), comment_in_english:=translate(comments)[2], by=comments]
