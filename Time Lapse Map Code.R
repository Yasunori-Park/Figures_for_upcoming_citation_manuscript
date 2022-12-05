library(ggraph)
library(igraph)
library(readr)
library(snahelper)
library(oaqc)

#Palette
SeqError_palette <- c("Green", "Grey", "Red", "Black")
Edge_palette <- c("Red", "#C5B233", "#4DFF4D", "#33CCD0", "#81B0FF",
                  "#F783E9")
########################################################################

#2015
#2022
#Read in Edges + Nodes & convert all int to str
Node_Year <- read.csv("2022_Nodes.csv", header = T, as.is = T)
Node_Year$Year <- as.factor(Node_Year$Year)
Node_Year$PMID <- as.character(Node_Year$PMID)

Edge_Year <- read.csv("2022_Edges.csv", header = T, as.is = F)
Edge_Year$Generation <- as.character(Edge_Year$Generation)

#df for citation network from Edges + Nodes
Network_Year <- graph_from_data_frame(
  d = Edge_Year, vertices = Node_Year, directed = T)


#Reuse this code to generate a citation map for each year
#Export pdf as 10:8 inches ratio. 
ggraph(Network_Year, layout = "kk") +
  geom_node_point(aes(fill = Error, 
                      colour = Error),
                  shape = 21,
                  size = 2,
                  alpha = 0.7)+
  geom_edge_link(aes(edge_color = Generation),
                 start_cap = circle(1.5, 'mm'),
                 end_cap = circle(1.5, 'mm'),
                 arrow = arrow(length =
                                 unit(2,
                                      'mm'),
                               ends = "last",
                               angle = 30,
                               type = 'closed'), 
                 show.legend = T, 
                 width = 0.5, 
                 position = "identity",
                 edge_alpha = 0.8)+
  #geom_node_text(aes(label = Label), size = 2.3, repel = F)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 7.0), 
        legend.text = element_text(size = 7.0),
        panel.background = element_blank())+
  scale_fill_manual(values = SeqError_palette)+
  scale_colour_manual(values = SeqError_palette)+
  scale_edge_colour_manual(values = Edge_palette) + 
  labs(fill = "Publication status", 
       colour = "Publication status")+
  guides(shape = guide_legend(override.aes = list(size = 3.0)),
         color = guide_legend(override.aes = list(size = 3.0)),
         edge_colour = guide_legend(override.aes = list(size = 6.0)))

#BIG
ggraph(Network_Year, layout = "kk") +
  geom_node_point(aes(fill = Error, 
                      colour = Error),
                  shape = 21,
                  size = 13,
                  alpha = 0.7)+
  geom_edge_link(aes(edge_color = Generation),
                 start_cap = circle(15, 'mm'),
                 end_cap = circle(15, 'mm'),
                 arrow = arrow(length =
                                 unit(6,
                                      'mm'),
                               ends = "last",
                               angle = 30,
                               type = 'closed'), 
                 show.legend = F, 
                 width = 0.5, 
                 position = "identity",
                 edge_alpha = 0.8)+
  #geom_node_text(aes(label = Label), size = 23, repel = F)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 70.0), 
        legend.text = element_text(size = 70.0),
        panel.background = element_blank())+
  scale_fill_manual(values = SeqError_palette)+
  scale_colour_manual(values = SeqError_palette)+
  scale_edge_colour_manual(values = Edge_palette) + 
  labs(fill = "Publication status", 
       colour = "Publication status")+
  guides(shape = guide_legend(override.aes = list(size = 30.0)),
         color = guide_legend(override.aes = list(size = 30.0)),
         edge_colour = guide_legend(override.aes = list(size = 60.0)))

