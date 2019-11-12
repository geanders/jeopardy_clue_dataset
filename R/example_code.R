library(tidytext)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(stringr)
library(tidyr)
library(forcats)

season_35 <- read_tsv("data/season35.tsv")

season_35 %>% 
  select(air_date) %>% 
  distinct()

s35_e1 <- season_35 %>% 
  filter(air_date == ymd("2018-09-10"))

nrow(s35_e1)

s35_e1 %>% 
  select(category) %>% 
  distinct()

s35_e1 %>% 
  filter(category == "LITERATURE") %>% 
  select( answer, question)

library(DT)
s35_e1 %>% 
  filter(category == "BACK TO BASICS") %>% 
  select(value, answer, question) %>% 
  DT::datatable()

season_35 %>% 
  filter(daily_double == "yes") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() + 
  facet_wrap(~ round, nrow = 2)

s35_e1 %>% 
  filter(round %in% c(1, 2)) %>% 
  group_by(round, category) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>% 
  mutate(category = str_wrap(category, width = 10)) %>% 
  ggplot(aes(x = category, y = n, fill = value, color = daily_double)) + 
  geom_tile() + 
  scale_y_reverse(name = "") + 
  scale_x_discrete(name = "", position = "top") + 
  scale_fill_viridis(labels = scales::dollar) + 
  scale_color_manual(values = c("white", "red")) + 
  facet_wrap(~ round, nrow = 2, scales = "free_x") 



system("say 'hello world'")

system("say 'This word for \"basic\" is also a 3rd grade type of school'")

system(paste("say '", s35_e1$answer[1], "'"))
system(paste("say '", s35_e1$question[1], "'"))



answer_words <- s35_e1 %>% 
  select(value, category, answer, round) %>% 
  unnest_tokens(answer_words, answer)

answer_words %>% 
  group_by(value, category, round) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(category, round) %>% 
  summarize(average_words = mean(n)) %>% 
  arrange(desc(average_words)) %>% 
  ggplot(aes(x = fct_reorder(str_wrap(category, width = 25), average_words), 
             y = average_words,
             fill = factor(round))) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(x = "", 
       y = "Average number of words in answer",
       fill = "round")

answer_words %>% 
  anti_join(get_stopwords(), by = c("answer_words" = "word"))

