# Concat month-end Billboard Hot 100 data
library(dplyr)
library(readr)
hitsong <- list.files(path="billboard_data/", full.names = TRUE) %>% 
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows

str(hitsong)

# Remove duplicated rows since songs can feature on the chart for weeks
hitsong <- hitsong[!duplicated(hitsong$title),]

# Remove useless columns - isNew, last, peak, week
hitsong<- subset(hitsong, select = c(title, artist))
head(hitsong)

# Label hit or not - clearly all of has the attribute hit -1
hitsong$hit <-  rep(1, nrow(hitsong))
head(hitsong)

# Split artist into multiple columns in case it is a collab song because we want to merge on primary artist
## "Featuring", "X", "x", and "&" mean multi-artist
library(tidyr)
hitsong_clean <- hitsong %>%
  separate(artist, into = c("artist", "artist_sub"), 
           sep = "\\s*Featuring\\s*|\\s*X\\s*|\\s*x\\s*|\\s*&\\s*")

head(hitsong_clean)
# Output a csv file
#write.csv(hit_songs, 'hit_songs_clean_2000_2021.csv', row.names = FALSE)

# Concat song release data - the data does not include headers
song_release <- list.files(path="song_release/", full.names = TRUE) %>% 
  lapply(read_csv, col_names = FALSE, show_col_types = FALSE) %>% 
  bind_rows

colnames(song_release) <- c("title", "artist")
head(song_release)

# Lable all the songs by merging with the Billboard data
total <- merge(song_release, hitsong_clean, by = c("title", "artist"), all.x = TRUE)
head(total)

# Drop artist_sub
total <- subset(total, select = -c(artist_sub))
head(total)

# Label non-hit songs hit -0
total$hit[is.na(total$hit)] <- 0
head(total)

# Output to a csv file
#write.csv(total, 'total_songs_2000_2021.csv', row.names = FALSE)

# frequency table of hit and non-hit
table(total$hit)
