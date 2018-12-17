library(data.table)
library(httr)

translate <- function() {
  comments <- fread("./input/reviews.csv")
  r <- POST("https://translate.yandex.net/api/v1.5/tr.json/translate",
            body = list(
              key = "trnsl.1.1.20181214T151349Z.e323c6a0eeb6c59d.6ed57788f95d2a05d4269fddb847986f8769b990",
              text = comments[id == 295840159,comments],
              lang = "en",
              format = "plain"
            ),
            encode = "form",
            verbose())
  print(content(r)$lang)
  print(content(r)$text)
}