library(tidyverse)
library(tidytext)
library(textclean)
library(igraph)
library(ggraph)
library(rgexf)

df1 <- read.csv("D:/KMMI/covid19_tweet.csv")
df1 = df1 %>%
  select(screen_name,text)
df1 <- dplyr::filter(df1,stringr::str_detect(text,"vaksin"))

## menagmbil hastag pada suatu text
regex <- "#[a-zA-Z0-9]{1,}"
hastag <- str_extract_all(string = df1$text,
                             pattern = regex, 
                             simplify = TRUE)

hastag <- data.frame(hastag)
hastag <- hastag %>%
  unite("hastags", sep = " ")

df1 = bind_cols(df1, hastag)
df1$hastags = replace_white(df1$hastags)
df1$hastags = str_trim(string = df1$hastags, side = "both")
glimpse(df1)

# membuat data network
## tokenisasi target
df2 = df1 %>%
  unnest_tokens(hastag, hastags, token = "words",
                to_lower = FALSE, drop = FALSE)
df3 = df2 %>%
  select("sumber" = screen_name, "target" = hastag)

## Perkecil data
df3 = df3 %>%
  group_by(sumber) %>%
  count(target) %>%
  filter(n >= 5)
class(df3)
glimpse(df3)

?graph_from_data_frame
## membuat data network
df_net = graph_from_data_frame(d = df3, directed = FALSE)

write_graph(graph = df_net, file = "D:/KMMI/tes_net.graphml",
            format = "graphml")

nodes_df <- data.frame(ID = c(1:vcount(df_net)), NAME = V(df_net)$name)

edges_df <- as.data.frame(get.edges(df_net, c(1:ecount(df_net))))

write.gexf(nodes = nodes_df, edges = edges_df,
           defaultedgetype = "directed",
           output = "D:/KMMI/sample_net.gexf")

# visualisasi
plot(df_net)

ec <- eigen_centrality(graph = df_net,
                       directed = FALSE,
                       weights = NA)$vector

?plot
plot(df_net, vertex.size = ec*10, layout = layout_with_fr)

## Layout network
coords <- layout_(df_net, as_star(), normalize())
plot(df_net, vertex.size = ec*10, layout = coords)

plot(df_net, vertex.size = ec*10, layout = layout_with_dh)

## Visualisasi ggraph
df_net %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_link() +
  theme_graph()

### geom_node_point adl nodenya, geom_edge_link adl edgenya

# membuat cluster
wc <- cluster_walktrap(graph = df_net)
col <- wc$membership

df_net %>%
  ggraph(layout = "kk") +
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = ec, color = as.factor(col)),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = FALSE) +
  scale_fill_manual() + theme_graph()

user_kelompok_0 %>%
  arrange(desc(betweenesscentrality)) %>%
  head(n = 10) %>%
  select(Label, betweenesscentrality)
