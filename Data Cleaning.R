library(tm)
library(tidyverse)

setwd("C:/Users/unaeem/Dropbox (OPML)/ECARO RTA of COVID response (team folder)/11. Phase 2/Text Analysis")
df <- readxl::read_xlsx("EDU_FULL_without_hash_emoji_EN.xlsx")

corpus <- Corpus(VectorSource(df$translated_content))

cleaner <- function(TextDoc) {
  # Convert the text to lower case
  TextDoc <- tm_map(TextDoc, content_transformer(tolower))
  # https://gist.github.com/CateGitau/05e6ff80b2a3aaa58236067811cee44e
  # https://stackoverflow.com/questions/25352448/remove-urls-from-string
  
  #Replacing "/", "@" and "|" with space
  toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
  TextDoc <- tm_map(TextDoc, toSpace, "\\b+rt") ## Remove RT
  TextDoc <- tm_map(TextDoc, toSpace, "@\\S+") ## Remove Mentions
  TextDoc <- tm_map(TextDoc, toSpace, "@.\\S+") ## Remove Mentions with space
  #TextDoc <- tm_map(TextDoc, toSpace, "#\\S+") ## Remove Hashtags
  #TextDoc <- tm_map(TextDoc, toSpace, " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") ## Remove URLs
  #TextDoc <- tm_map(TextDoc, toSpace, "[^\x01-\x7F]") ## Remove emoji and shapes
  TextDoc <- tm_map(TextDoc, toSpace, "http\\S+\\s*") ## Remove URLs
  TextDoc <- tm_map(TextDoc, content_transformer(function (x , pattern) gsub(pattern, "#", x)), "\\# ") ## Remove URLs
  TextDoc <- tm_map(TextDoc, toSpace, "[^[:alnum:]\\#s]") ## Remove punctuation
  TextDoc <- tm_map(TextDoc, content_transformer(gsub), pattern = "schools", replacement = "school")
  TextDoc <- tm_map(TextDoc, content_transformer(gsub), pattern = "children", replacement = "child")
  TextDoc <- tm_map(TextDoc, content_transformer(gsub), pattern = "#covid", replacement = "covid")
  TextDoc <- tm_map(TextDoc, content_transformer(gsub), pattern = "coronavirus", replacement = "covid")  
  TextDoc <- tm_map(TextDoc, content_transformer(gsub), pattern = "pandemic", replacement = "covid")  

  # Remove numbers
  TextDoc <- tm_map(TextDoc, removeNumbers)
  
  # Creating a list of words to remove
  #list <- c("unicef", "education", "student", "school")
  mywords <- (c(stopwords("english"), "get","like","just", "can", "etc", "unicef", "re", "turkey", "azerbaijan")) # can add more
  # Remove english common stopwords
  TextDoc <- tm_map(TextDoc, removeWords, mywords)
  
  # Remove punctuations
  #TextDoc <- tm_map(TextDoc, removePunctuation)
  #TextDoc <- tm_map(TextDoc, toSpace, "[[:punct:] ]+")
  
  # Eliminate extra white spaces
  TextDoc <- tm_map(TextDoc, stripWhitespace)
  
  # Text stemming - which reduces words to their root form
  #TextDoc <- tm_map(TextDoc, stemDocument)
  
  TextDoc <- tm_map(TextDoc, toSpace, " *\\b[[:alpha:]]{1}\\b *") ## Removing single characters
  TextDoc <- tm_map(TextDoc, toSpace, "\\# ") ## Remove Hashtags with space
  
  # Removing single length characters (a, b etc)
  TextDoc <- tm_map(TextDoc, toSpace, "^[[:space:]]*") ## Remove leading whitespaces
  TextDoc <- tm_map(TextDoc, toSpace, "[[:space:]]*$") ## Remove trailing whitespaces
  
  
  # Converting to dataframe
  dataframe <- data.frame(text=sapply(TextDoc, identity), 
                          stringsAsFactors=F)
  
  return(dataframe)
}

cleaned_data <- cleaner(corpus)

df$cleaned <- cleaned_data$text

df$source_type <- gsub("SOCIALMEDIA,SOCIALMEDIA_","", df$source_type)
#write.csv(df, "C:/Users/unaeem/Dropbox (OPML)/ECARO RTA of COVID response (team folder)/11. Phase 2/Report_Education/TW Import Data/English_translated_cleaned.csv", row.names = F)

# raw_content = df$translated_content,

##### Making Upload Ready File
upload <- data.frame(url = df$url,
                     published = df$published,
                     content = df$cleaned,
                     source = df$source_type,
                     country_code = df$extra_source_attributes.world_data.country_code,
                     author_name = df$extra_author_attributes.name,
                     author_country = df$extra_source_attributes.world_data.country_code,
                     raw_content = df$translated_content)

upload <- upload %>%
  mutate(country_name = case_when(
    country_code == "az" ~ "Azerbaijan",
    country_code == "ba" ~ "Bosnia and Herzegovina",
    country_code == "rs" ~ "Serbia",
    country_code == "tr" ~ "Turkey"
  ))

#upload %>%
#  rowwise() %>%
#  mutate(length = str_length(gsub("[[:blank:]]", "", content)))

writexl::write_xlsx(upload,
                    "Data_Processed_without_hash_emoji.xlsx")
