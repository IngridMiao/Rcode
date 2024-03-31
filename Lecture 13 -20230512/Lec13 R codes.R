####################
#    Lecture 13    #
####################
# Network Analysis #
####################
install.packages("statnet", dependencies = TRUE)
library(statnet)
statnet::update_statnet()

#Creat a network object with sociometrix data format
netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
net1 <- as.network(x = netmat1, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input ; 相鄰矩陣
)
network.vertex.names(net1) <- LETTERS[1:5] #c("A","B",...)
class(net1)
summary(net1)

network.size(net1) #check num. of nodes
gden(net1) #density
gtrans(net1) #Cluster Coefficient, range from 0 to 1 ,measure the transitivity

gplot(net1)
gplot(net1, displaylabels =TRUE)
gplot(net1,gmode="graph",mode="circle", displaylabels =TRUE)
gplot(net1,gmode="graph",mode="mds", displaylabels =TRUE)

install.packages("GGally")
library(GGally)
ggnet2(net1,node.size = 6, node.color = "tomato", edge.size = 0.5, edge.color = "black",
           label = TRUE, label.size = 5, label.color = "white")

# https://briatte.github.io/ggnet/

#Creat network object with edge list format
netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2    <- network(netmat2, matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
summary(net2)
gplot(net2, displaylabels =TRUE)

#### another way ######
library(tidyverse)
edge_list = tibble(from = c(1,1,2,2,3,5), to=c(2,3,3,4,2,3), weight = c(1,3,4,3,1,2))
node_list = tibble(id =1:5, label = LETTERS[1:5])
net = network(edge_list , vertex.attr = node_list, matrix.type = "edgelist")

#Transform between the different data structure
as.sociomatrix(net2)
class(as.sociomatrix(net2))

as.matrix(net1,matrix.type = "edgelist")


##detach("package:igraph")  #if set.vertex.attribute() not working, use
# Node attribute
# Create the variable
gender <- c(rep("Female",3),rep("Male",2))
# Take a look at our variable
print(gender)
# Add it to the network object
set.vertex.attribute(net1, # the name of the network object
                     "Gender", # the name we want to reference the variable by in that object
                     gender # the value we are giving that variable
) 

age <- round(rnorm(5,20,3))
set.vertex.attribute(net1,"Age",age)



summary.network(net1, # the network we want to look at
                print.adj = T # if TRUE then this will print out the whole adjacency matrix.
)

#Visualizing a network 
num_nodes = 5
node_colors <- rep("",num_nodes)
for(i in 1:num_nodes){
  if(get.node.attr(net1,"Gender")[i] == "Female"){
    node_colors[i] <- "royalblue"
  }else{
    node_colors[i] <- "maroon"
  }
}
print(node_colors)

plot.network(net1, # network object
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes (1~5)
)


library(igraph)
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=10, directed=T) 
plot(g1)
g2 <- graph( c("A","B","B","C","C","A"), isolates=c("D","E","F") )
plot( g2  )
plot(graph_from_literal(1--+2, 3+--2, 1+-+3))


E(g1) # The edges of the object
V(g1) # The vertices of the object
g1[] #network matrix
V(g1)$gender <- c(rep("male", 5),rep("female",5)) #create a variable
V(g2)$name #names of nodes 
E(g1)$type <- "email" #assign email to edges 
E(g1)$weight <- c(10,3,1) #assign edge weight

V(g1)$color = V(g1)$gender #assign the "gender" attribute as the vertex color
V(g1)$color = gsub("female","red", V(g1)$color) #Females will be red
V(g1)$color = gsub("male","blue", V(g1)$color) #Males will be blue
plot.igraph( g1, vertex.label = NA, layout = layout.fruchterman.reingold )

#examine attributes
edge_attr(g1) 
vertex_attr(g1)

#http://kateto.net/networks-r-igraph


#another way in igraph
g_adj_u <- graph.adjacency(netmat1, mode="undirected")
plot(g_adj_u)
g_adj_d <- graph.adjacency(netmat1, mode="directed")
plot(g_adj_d)
g_el_u <- graph.edgelist(netmat2, directed=FALSE)
plot(g_el_u)

shortest.paths(g_adj_u) #shortest path
average.path.length(g_adj_u)

#無向圖
degree.distribution(g_adj_u)
#有向圖
degree.distribution(g_adj_d, mode="in")
degree.distribution(g_adj_d, mode="out")

###
# other method to import network data
library(tidyverse)
edge_list <- tibble(from = c(1,1,2,2,3,5), to = c(2,3,3,4,2,3),weight=c(1,3,4,3,1,2))
node_list <- tibble(id = 1:5, label = LETTERS[1:5])

library(network)
net1 <- network(edge_list, vertex.attr = node_list, matrix.type = "edgelist", ignore.eval = FALSE)
plot(net1, vertex.cex = 5) #vertex.cex for node size
plot(net1, vertex.cex = 3, mode = "circle")


library(igraph)
net2 <- graph_from_data_frame(d = edge_list, vertices = node_list, directed = TRUE)
net2



## ptt network analysis (need ptt.R in lec12)
# get author and his pusher#####################################################
temp<-list()
nauthor = length(article.table$author)

for(i in 1:nauthor){
  temp[[i]] = which(push.table$url==article.table$url[i])}
df<-list()
for(i in 1:nauthor){
  n = length(temp[[i]])
  df[[i]]= cbind(rep(article.table$author[i],n), push.table$author[temp[[i]]])}

dd=do.call(rbind,df) #rbind list

d = as.data.frame(dd)
write.table(d,"ptt.txt")
###############################################################################
d = read.table("ptt.txt",h=T)
library(dplyr)
#依照推文和被推者分組，計算每個pair的數量
edge2 <- group_by(d,V1,V2)
edge2 <- summarise(edge2,weight=n())
#from 表示推文者 to 表示被推文的
edge2 <- data.frame(from = edge2$V2, to = edge2$V1, weight = edge2$weight)
edge2 <-edge2[1:66,]


g <- graph.data.frame(edge2,directed = TRUE)

V(g) #nodes
V(g)$name #names of each node
vertex_attr(g) #all attributes of the nodes
E(g) #edges
E(g)$weight # weights for each edges
edge_attr(g) # all attributes of the edges
g[] # adjacency matrix

V(g)$size <- strength(g) #importance
V(g)$label <- ifelse( strength(g)>=10, V(g)$name, NA )

plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=5)
plot(g,
     vertex.color = "red", # change color of nodes
     vertex.label.color = "blue", # change color of labels
     vertex.label.cex = 1.2, # change size of labels to 120% of original size
     edge.curved=.55, # add a 55% curve to the edges
     edge.color="grey",# change edge color to grey
     vertex.size=3, # change vertex size
     layout = layout.auto) 

#layout=layout_on_grid # 簡單的網格布局
#layout.auto #自動布局
#layout_as_star #星形布局
#layout.circle #環形布局
#layout_randomly #隨機布局
#layout_as_tree(g) #樹狀布局
#

#找最大值
degree(g, mode="all") # # of connections between a node and all other nodes
closeness(g, mode="all", weights=NA, normalized=T) ## key person has a larger value
betweenness(g, directed=F, weights=NA, normalized = T) ## find key person
distances(g, v=V(g)["abc22826320"], to=V(g), weights=NA)



##################################################
demo(package="igraph", community)


##################################################
# Example of http://kateto.net/networks-r-igraph #
##################################################
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
head(nodes)
head(links)

nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

class(net)
plot(net, edge.arrow.size=.4,vertex.label=NA)
net <- simplify(net, remove.multiple = T, remove.loops = T) 

plot(net, edge.arrow.size=.4, edge.curved=.1)
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 


# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12

plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#clustering
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net) 

##Exercise 
install.packages("igraphdata")
# https://cran.r-project.org/web/packages/igraphdata/README.html
library(igraphdata)
data(UKfaculty)
plot(UKfaculty)


### TDM from Lec 12.
termDocMatrix <- as.matrix(tdm)

#Transform Data into an Adjacency Matrix
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
 # transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
layout1 <- layout.fruchterman.reingold(g)
plot(g)


