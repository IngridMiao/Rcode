# https://www.tidytextmining.com/
text<-c(
"There ain't no gold in this river
That I been washin' my hands in forever
I know there is hope in these waters
But I can't bring myself to swim
When I am drownin' in the silence
Baby, let me in
  
Go easy on me, baby
I was still a child
Didn't get the chance to feel the world around me
I had no time to choose what I chose to do
So go easy on me

There ain't no room for our things to change
When we are both so deeply stuck in our ways
You can't deny how hard I've tried
I changed who I was to put you both first
But now I give up
  
Go easy on mе, baby
I was still a child
Didn't get the chance to
Feel thе world around me
Had no time to choose what I chose to do
So go easy on me

I had good intentions
And the highest hopes
But I know right now
It probably doesn't even show
  
Go easy on me, baby
I was still a child
I didn't get the chance to
Feel the world around me
I had no time to choose what I chose to do
So go easy on me")

#turn it into a tidy text dataset, we first need to put it into a data frame.
library(dplyr)
text_df <- tibble(line = 1:39, text = text)
text_df

# break the text into individual tokens
install.packages("tidytext")
library(tidytext)

text_df %>%
  unnest_tokens(word, text) -> text_df2  #line words

text_df2 %>%
  count(word, sort = TRUE) 


library(ggplot2)

text_df2 %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


text_df2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


text_df2 %>%
  count(word, sort = TRUE) %>%
filter(n > 3) ->newd

library(wordcloud2)
wordcloud2(newd)
wordcloud2(newd, size = 1,shape = 'star')
wordcloud2(newd, color = "random-light", backgroundColor = "grey")



