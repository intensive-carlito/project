package main

import (
    "bufio"
    "encoding/csv"
    "encoding/json"
    "fmt"
    "io"
    "log"
    "os"
    "time"
    "net/http"
    "io/ioutil"
    "net/url"
    "regexp"
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

func translate_google(str string) Row {
    time.Sleep(2 * time.Second)
    translatorUrl := "https://translate.googleapis.com/translate_a/single"

    resp, err := http.Get(translatorUrl + "?client=gtx&sl=auto&tl=en&dt=t&q=" + url.QueryEscape(str))
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
    bytes, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    content := string(bytes)
    r := regexp.MustCompile(`\[\[\["(.*)","(.*)",.*,.*,.*\]\],.*,"(.*)",.*`)
    match := r.FindStringSubmatch(content)
    if len(match) == 0 {
        panic("No more result for now...")
    }

    retval := Row {
        200,
        match[3] + "-en",
        []string{ match[1] },
        "" }

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
    infile, err := os.Open("../to_translate2.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer infile.Close()

    outfile, err := os.Create("../translations3.csv")
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
