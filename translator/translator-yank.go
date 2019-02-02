package main

import (
    "bufio"
    "encoding/csv"
    "encoding/json"
    "fmt"
    "io"
    "log"
    "os"
    "net/http"
    "io/ioutil"
    "net/url"
    "strconv"
    "strings"
)

type Row struct {
    Code int
    Lang string
    Text []string
    Message string
}

//curl -XPOST "https://translate.yandex.net/api/v1.5/tr.json/translate?key=trnsl.1.1.20181214T151349Z.e323c6a0eeb6c59d.6ed57788f95d2a05d4269fddb847986f8769b990&text=ce%20chat%20est%20mignon&lang=fr-en&format=plain"

func translate2(str string) Row {
    var retval Row
    ret := []byte(`{"code":200,"lang":"fr-en","text":["Hello world"]}`)
    err := json.Unmarshal(ret, &retval)
    if err != nil {
        log.Fatal(err);
    }
    return retval
}

func translate(str string) Row {
    translatorUrl := "https://translate.yandex.net/api/v1.5/tr.json/translate"

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

    var retval Row
    err = json.Unmarshal(body, &retval)
    if err != nil {
        log.Fatal(err);
    }

    if retval.Code != 200 {
        log.Fatal("It is the end for today... : " + retval.Message)
    }

    return retval
}

func main1() {
    ret := translate("Bonjour les amis")
    fmt.Println(ret.Text)
}

func main() {
    infile, err := os.Open("../french6.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer infile.Close()

    outfile, err := os.Create("../translations-french6.csv")
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
        fmt.Print(".")

        row, err := reader.Read()
        if err == io.EOF {
            break
        } else if err != nil {
            log.Fatal(err)
        }
        comment := row[0]
        num := row[1]
        lang := row[2]
        var comment_in_english Row
        var ll, cmt string
        if lang != "english" {
            comment_in_english = translate(comment)
            cmt = strings.Join(comment_in_english.Text, " - ")
            ll = comment_in_english.Lang
        } else {
            cmt = comment
            ll = "en-en"
        }
        err = writer.Write([]string{ comment, num, lang, ll, cmt })
        if err != nil {
            log.Fatal(err)
        }
    }
}
