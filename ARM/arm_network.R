#install.packages('tokenizers')
library(tokenizers)
#install.packages('arules')
library(arules)
#install.packages('arulesViz')
library(arulesViz)
#install.packages('networkD3')
library(networkD3)
#install.packages('igraph')
library(igraph)
#install.packages('visNetwork')
library(visNetwork)
library(tidyverse)
library(dplyr)
library(stringr)
#install.packages('stopwords')
library(stopwords)

setwd('/Users/kristallqiu/Desktop/501/portfolio/ARM')

######## Transaction Data Preparation ########
lyrics <- read.csv('/Users/kristallqiu/Desktop/501/portfolio/lyrics.csv')[!lyrics$lyrics=='',]
TopArtists <- c('Justin Bieber', 'Ed Sheeran', 'The Weeknd', 'Dua Lipa')
TopLyrics <- lyrics[grepl(TopArtists[1], lyrics$artists),'lyrics']
for (artist in TopArtists[2:length(TopArtists)]){
  TopLyrics <- c(TopLyrics,
                     lyrics[grepl(artist, lyrics$artists), 'lyrics'])
}

# text cleaning
TopLyrics <- gsub('\\n', ' ', TopLyrics)
TopLyrics <- gsub('[0-9]+', '', TopLyrics)

# create the file
trans <- file('TopLyricsFile.csv')

tokens<-tokenizers::tokenize_words(
  TopLyrics[1], stopwords = stopwords::stopwords('en'), 
  lowercase = T,  strip_punct = T, strip_numeric = T,
  simplify = T)

# write tokens
cat(unlist(tokens), '\n', file=trans, sep=',')
close(trans)

# append the remaining lists of tokens
trans <- file('TopLyricsFile.csv', open = 'a')
for(i in 2:length(TopLyrics)){
  tokens<-tokenizers::tokenize_words(
    TopLyrics[i], stopwords = stopwords::stopwords('en'), 
    lowercase = T,  strip_punct = T, strip_numeric = T,
    simplify = T)
  
  cat(unlist(tokens), '\n', file=trans, sep=',')
}
close(trans)

# read lyrics transactions as a dataframe
LyricsDF <- read.csv('TopLyricsFile.csv', 
                    header = FALSE, sep = ',') %>%
  mutate_all(as.character)
head(LyricsDF)

# clean with grepl - every row in each column
MyDF<-NULL
for (i in 1:ncol(LyricsDF)){
  MyList=c() 
  MyList=c(MyList,(nchar(LyricsDF[[i]])<4 | nchar(LyricsDF[[i]])>11))
  MyDF<-cbind(MyDF,MyList) 
}
LyricsDF[MyDF] <- ''
head(LyricsDF)

# save the dataframe using the write table command 
write.table(LyricsDF, file = 'UpdatedTopLyricsFile.csv', col.names = FALSE, 
            row.names = FALSE, sep = ',')

######## ARM ######## 
# read transactions
lyrics_trans <- read.transactions('UpdatedTopLyricsFile.csv', sep =',', 
                                format('basket'),  rm.duplicates = TRUE)

# itemsets length = 2
rule2 <- arules::apriori(lyrics_trans, parameter = list(support=.06,
                                                       conf=0.5,
                                                       minlen=2,
                                                       maxlen=2))

write.csv(DATAFRAME(rule2), 'arules2.csv')
write.csv(DATAFRAME(head(sort(rule2, by='support', decreasing=TRUE),15)), 'arules2_top15_sup.csv')
write.csv(DATAFRAME(head(sort(rule2, by='confidence', decreasing=TRUE),15)), 'arules2_top15_conf.csv')
write.csv(DATAFRAME(head(sort(rule2, by='lift', decreasing=TRUE),15)), 'arules2_top15_lift.csv')

# itemsets length > 2
rule3 <- arules::apriori(lyrics_trans, parameter = list(support=.06,
                                                       conf=0.5,
                                                       minlen=3))

write.csv(DATAFRAME(rule3), 'arules3.csv')
write.csv(DATAFRAME(head(sort(rule3, by='support', decreasing=TRUE),15)), 'arules3_top15_sup.csv')
write.csv(DATAFRAME(head(sort(rule3, by='confidence', decreasing=TRUE),15)), 'arules3_top15_conf.csv')
write.csv(DATAFRAME(head(sort(rule3, by='lift', decreasing=TRUE),15)), 'arules3_top15_lift.csv')

rules_all <- arules::apriori(lyrics_trans, parameter = list(support=.06,
                                                        conf=0.85,
                                                        minlen=2))

write.csv(DATAFRAME(rules_all), 'arules_all.csv')
write.csv(DATAFRAME(head(sort(rules_all, by='support', decreasing=TRUE),15)), 'arules_all_top15_sup.csv')
write.csv(DATAFRAME(head(sort(rules_all, by='confidence', decreasing=TRUE),15)), 'arules_all_top15_conf.csv')
write.csv(DATAFRAME(head(sort(rules_all, by='lift', decreasing=TRUE),15)), 'arules_all_top15_lift.csv')

######## Visualizations ########
# igraph
vis_all = plot(head(sort(rules_all, by='support', decreasing = TRUE), 35), 
               method = 'graph', control = list(verbose = FALSE),
                   measure = 'lift', shading = 'confidence', engine = 'htmlwidget')
htmlwidgets::saveWidget(vis_all, 'igraph_all_rules.html', selfcontained = TRUE)

## Build Nodes & Edges ##
build_nodes_n_edges <- function(rule, measure='support', n_rules=100){
  # convert rules to dataframe
  rules_df <- DATAFRAME(head(sort(rule, by=measure, decreasing=TRUE), n_rules))
  # remove {}
  rules_df$LHS<-as.character(rules_df$LHS)
  rules_df$RHS<-as.character(rules_df$RHS)
  rules_df[] <- lapply(rules_df, gsub, pattern='[{]', replacement='')
  rules_df[] <- lapply(rules_df, gsub, pattern='[}]', replacement='')
  
  # build nodes
  lhs <- rules_df %>%
    distinct(LHS) %>%
    rename(label = LHS)
  rhs <- rules_df %>%
    distinct(RHS) %>%
    rename(label = RHS)
  nodes <- full_join(lhs, rhs, by = 'label') %>%
    rowid_to_column('id')
  
  # build edges
  per_route <- rules_df %>%  
    group_by(LHS, RHS) %>%
    summarise(weight = support) %>% 
    ungroup()
  
  edges <- per_route %>% 
    left_join(nodes, by = c('LHS' = 'label')) %>% 
    rename(from = id)
  
  edges <- edges %>% 
    left_join(nodes, by = c('RHS' = 'label')) %>% 
    rename(to = id)
  
  edges <- select(edges, from, to, weight)
  return (list(nodes=nodes, edges=edges))
}

# length = 2
## networkD3
nw2 <- build_nodes_n_edges(rule2, n_rules = 120)
nodes_d3 <- mutate(nw2$nodes, id = id - 1)
edges_d3 <- mutate(nw2$edges, from = from - 1, to = to - 1)
vis2 <- forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = 'from',
                               Target = 'to', NodeID = 'label', Group = 'id',
                               Value = 'weight', arrows = T,
                               colourScale = JS('d3.scaleOrdinal(d3.schemeCategory10);'),
                               opacity = 0.9,
                               fontSize = 16,
                               height = 583,
                               width = 750,
                               linkDistance = networkD3::JS('function(d) { return d.value*650; }'),
                               linkWidth = networkD3::JS('function(d) { return d.value*5; }'),
                               zoom = TRUE)
saveNetwork(vis2, 'networkD3.html', selfcontained = TRUE)

## sankey
vis2_sankey <- sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = 'from', Target = 'to', 
              NodeID = 'label', Value = 'weight', fontSize = 16)
htmlwidgets::saveWidget(vis2_sankey, 'Sankey2.html', selfcontained = TRUE)

# length = 3
## sankey
nw3 <- build_nodes_n_edges(rule3, n_rules = 50)
nodes_d3 <- mutate(nw3$nodes, id = id - 1)
edges_d3 <- mutate(nw3$edges, from = from - 1, to = to - 1)
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = 'from', Target = 'to', 
                             NodeID = 'label', Value = 'weight', fontSize = 16)
htmlwidgets::saveWidget(vis2_sankey, 'Sankey3.html', selfcontained = TRUE)

## visNetwork
edges <- nw3$edges
nodes <- nw3$nodes
edges <- mutate(edges, as.numeric(weight)/5 + 1)
vis3_visNetwork <- visNetwork(nw3$nodes, edges) %>% 
  visIgraphLayout(layout = 'layout_with_fr') %>% 
  visEdges(arrows = 'middle')
visSave(vis3_visNetwork, 'visNetwork.html', selfcontained = TRUE, background = 'white')