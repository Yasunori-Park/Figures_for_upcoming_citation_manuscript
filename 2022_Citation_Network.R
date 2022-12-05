library(ggraph)
library(igraph)
library(readr)
library(snahelper)
library(oaqc)

#Read in Edges + Nodes & convert all int to str
Drug_Node <- read.csv("2022_Citation_Nodes.csv", header = T, as.is = T)
Drug_Node$Year <- as.factor(Drug_Node$Year)
Drug_Node$PMID <- as.character(Drug_Node$PMID)
Drug_Node$Shape <- as.character(Drug_Node$Shape)

Drug_Edge <- read.csv("2022_Citation_Edges.csv", header = T, as.is = F)
Drug_Edge$Generation <- as.character(Drug_Edge$Generation)

View(Drug_Edge)

#df for citation network from Edges + Nodes
Drug_Citation_Net <- graph_from_data_frame(d = Drug_Edge, vertices = Drug_Node, directed = T)
#View(Drug_Citation_Net)

#Values for scale fill/colour
SeqError_palette <- c("Green", "Red", "Red")

#Standard citation map
#As per Guillaume's comment, tried to sort shapes by year. 
#This is possible but it is quite messy. Best course of action will be doing a
#Time lapse of the citation graph year by year. 
ggraph(Drug_Citation_Net, layout = "kk") +
  geom_node_point(aes(fill = Error, 
                      colour = Error,
                      shape = Shape),
                  size = 2,
                  alpha = 0.5)+
  geom_edge_link(aes(edge_color = Generation),
                 start_cap = circle(3, 'mm'),
                 end_cap = circle(1, 'mm'),
                 arrow = arrow(length =
                                 unit(1.5,
                                      'mm'),
                               ends = "last",
                               angle = 30,
                               type = 'closed'), 
                 show.legend = T, 
                 width = 0.5, 
                 position = "identity",
                 edge_alpha = 0.9)+
  geom_node_text(aes(label = Label), size = 3, repel = F)+
  theme_grey()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = SeqError_palette)+
  scale_colour_manual(values = SeqError_palette)+
  labs(fill = "Publication has \nSequence Error?", 
       colour = "Publication has \nSequence Error?")

  