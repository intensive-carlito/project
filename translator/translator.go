package main

import (
    "bufio"
    "encoding/csv"
    "fmt"
    "io"
    "log"
    "os"
    "net/http"
    "io/ioutil"
    "net/url"
    "strconv"
)

//curl -XPOST "https://translate.yandex.net/api/v1.5/tr.json/translate?key=trnsl.1.1.20181214T151349Z.e323c6a0eeb6c59d.6ed57788f95d2a05d4269fddb847986f8769b990&text=ce%20chat%20est%20mignon&lang=fr-en&format=plain"

func translate2(str string) string {
    return str + " translation"
}

func translate(str string) string {
    translatorUrl := "https://translate.yandex.net/api/v1.5/tr.json/translate"
    fmt.Println("URL:>", translatorUrl)

    resp, err := http.PostForm(translatorUrl, url.Values{
        "key": {"trnsl.1.1.20181214T151349Z.e323c6a0eeb6c59d.6ed57788f95d2a05d4269fddb847986f8769b990"},
        "text": { str },
        "lang": { "en" },
        "format": {"plain"} })

    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)

    if nil != err {
        log.Fatal("errorination happened reading the body", err)
    }

    return string(body[:])
}

func main() {
    infile, err := os.Open("../to_translate.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer infile.Close()

    outfile, err := os.Create("../translations.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer outfile.Close()

    reader := csv.NewReader(bufio.NewReader(infile))
    writer := csv.NewWriter(outfile)
    defer writer.Flush()

    index := 0
    for {
        if index++; index % 100 == 0 {
            fmt.Println("Row " + strconv.Itoa(index) + " is translated")
        }

        row, err := reader.Read()
        if err == io.EOF {
            break
        } else if err != nil {
            log.Fatal(err)
        }
        comment := row[0]
        num := row[1]
        lang := row[2]
        var comment_in_english string
        if lang != "english" {
            comment_in_english = translate2(comment)
        } else {
            comment_in_english = comment
        }
        err = writer.Write([]string{ comment, num, lang, comment_in_english })
        if err != nil {
            log.Fatal(err)
        }
    }
}
