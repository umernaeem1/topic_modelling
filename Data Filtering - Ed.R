library(tidyverse)
library(tm)

setwd("C:/Users/lenovo/Dropbox/ECARO RTA of COVID response (team folder)/11. Phase 2/Report_Education/TW Import Data/Without UNICEF")

df <-
  list.files(pattern = ".xlsx",
             full.names = TRUE) %>%
  map_df(~readxl::read_xlsx(.)) %>%
  filter(!is.na(content))

#https://stackoverflow.com/questions/15748190/emoticons-in-twitter-sentiment-analysis-in-r
# https://stackoverflow.com/questions/2098368/concatenate-a-vector-of-strings-character
df1 <- df %>%
  mutate(content_clean = emo::ji_replace_all(content,"")) %>%
  mutate(hashtags = str_extract_all(content, "#\\S+"),
         content_clean = str_remove_all(content_clean, "#\\S+")) %>%
  rowwise() %>%
  mutate(hashtags_c = stringi::stri_paste(hashtags,collapse = ' '))

writexl::write_xlsx(df1, "ED_Without_UNICEF_hash_emoji.xlsx")
#write.csv(df1, "SP_FULL.csv", row.names = F)
