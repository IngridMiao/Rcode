#http://biostat.tmu.edu.tw/enews/ep_download/21rb.pdf
install.packages(c("tidyverse", "rvest", "stringr", "jiebaR", "tmcn"))
library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
ptt.url <- "https://www.ptt.cc"
gossiping.url <- str_c(ptt.url, "/bbs/Gossiping")
gossiping.url

gossiping.session <- html_session(url = gossiping.url)
gossiping.session

gossiping.form <- gossiping.session %>%
  html_node("form") %>%
  html_form()
gossiping.form

gossiping <- session_submit(
  x = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)
gossiping 

page.latest <- gossiping %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("index[0-9]{2,}\\.html") %>%
  str_extract("[0-9]+") %>%
  as.numeric()

links.article <- NULL
page.length <- 10
for (page.index in page.latest:(page.latest - page.length)) {
  link <- str_c(gossiping.url, "/index", page.index, ".html")
  print(link)
  links.article <- c(
    links.article,
    gossiping %>%
      session_jump_to(link) %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html")
  )
}


  links.article <- unique(links.article)


push.table <- tibble() # 建⽴推⽂儲存空間
article.table <- tibble() # 建⽴⽂章儲存空間
for (temp.link in links.article) {
  
  article.url <- str_c(ptt.url, temp.link) # ⽂章網址
  temp.html <- gossiping %>% jump_to(article.url) # 連結⾄⽂章網址
  article.header <- temp.html %>%
    html_nodes("span.article-meta-value") %>% # 開頭部分元素
    html_text()
  article.author <- article.header[1] %>% str_extract("^[A-z0-9_]+") # 作者
  article.title <- article.header[3] # 標題
  article.datetime <- article.header[4] # 時間
  article.content <- temp.html %>%
    html_nodes( # 內⽂部分
      xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
    ) %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")
  article.table <- article.table %>% # 合併⽂章資料
    bind_rows(
      tibble(
        datetime = article.datetime,
        title = article.title,
        author = article.author,
        content = article.content,
        url = article.url
      )
    )
  
  article.push <- temp.html %>% html_nodes("div.push") # 擷取推⽂
  push.table.tag <- article.push %>% html_nodes("span.push-tag") %>% html_text(trim =
                                                                                 TRUE) # 推⽂種類
  push.table.author <- article.push %>% html_nodes("span.push-userid") %>% html_text(trim
                                                                                     = TRUE) # 作者
  push.table.content <- article.push %>% html_nodes("span.push-content") %>%
    html_text(trim = TRUE) %>% str_sub(3) # 推⽂內容
  push.table.datetime <- article.push %>% html_nodes("span.push-ipdatetime") %>%
    html_text(trim = TRUE) # 推文時間
  
  push.table <- push.table %>% # 合併推⽂資料
    bind_rows(
      tibble(
        tag = push.table.tag,
        author = push.table.author,
        content = push.table.content,
        datetime = push.table.datetime,
        url = article.url
      )
    )
  
}


jieba.worker <- worker()


article.date <- article.table %>%  ##change here
  group_by(datetime) %>% # 以每⽇做分組
  do((function(input) {
    freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻
      filter(
        !(char %in% toTrad(stopwordsCN())), # 過濾 stopword
        !str_detect(char, "[A-z0-9]"), # 過濾英⽂數字
        nchar(char) > 1 # 過濾單個字
      ) %>%
      arrange(desc(freq)) %>% # 以詞頻排序
      slice(1:100) %>% # 取前 100
      return
  })(.)) %>%
  ungroup
article.date.words <- freq(article.date$char) %>%
  rename(freq.all = freq) ->temp

wordcloud2(filter(temp, freq.all > 5),size=0.5)





##network analysis
temp<-list()
naut=length(article.table$author)
for(i in 1:naut){
temp[[i]] = which(push.table$url==article.table$url[i])}
df<-list()
for(i in 1:naut){
n = length(temp[[i]])
df[[i]]= cbind(rep(article.table$author[i],n), push.table$author[temp[[i]]])}


dd=do.call(rbind,df)

d=as.data.frame(dd)


#依照推文和被推者分組，計算每個pair的數量
edge2 <- group_by(d[1:100,],V1,V2)
edge2 <- summarise(edge2,weight=n())
#from 表示推文者 in 表示被推文的
edge2 <- data.frame(from = edge2$V2, to = edge2$V1,weight = edge2$weight)

library("igraph")

g <- graph.data.frame(edge2,directed = TRUE)


plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=5)
plot(g, vertex.size=3)

