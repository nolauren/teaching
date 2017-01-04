# Supreme Court Workshop

# Before doing anything else, we need to install the `igraph` package.
# This can be done with the following line of code (assuming you ar
# connected to the internet). To run them, simply highlight lines 5, 6, and 7,
# and then hit the Run button in the upper right side of this panel. This
# may take a while, but only needs to done once for your machine.

if (!require("igraph")) install.packages("igraph")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")

# Next, we will load these packages into R with the following lines of code.
# This needs to be done ever time you use R. Again, highlight the lines and
# hit Run above.

library(igraph)
library(dplyr)
library(readr)

# We can now read in the dataset of citations. We use a function called `read_csv`
# to read in a csv file, similar to the files we used last time.

edges <- read_csv("https://statsmaths.github.io/stat_data/scotus_edges.csv")
nodes <- read_csv("https://statsmaths.github.io/stat_data/scotus_nodes.csv")

# Nothing seems to happen, right? Well, not exactly... The datasets should appear up
# in the Environment tab. Double click on them to see the data in RStudio. What do you
# see?

# We can construct a graph object based on the edges defined in the all_counts table
# using the `graph.edgelist` function. It will be easier to use undirected graphs,
# so let's do that for now:

G <- graph.edgelist(as.matrix(edges), directed=FALSE)

# Each case is associated with a particular issue area that the Supreme Court
# comments on, and we can see this mapping in the themes dataset. A description
# of each issue is includes in the nodes table, but you may also view them in
# your browser here: http://scdb.wustl.edu/documentation.php?var=issue

# Let's take the subgraph of cases that have to do with desegregation, codes 20040 and 20050.
# We first look up all of the ids associated with these cases:

usids <- filter(nodes, issue %in% c(20040, 20050))$usid

# And then construct a subgraph of the results:

H <- induced.subgraph(G, usids)

# Now that we have a reasonably sized graph, we can try to plot it:

plot(H)

# The result will show up in the lower right hand corner. Hit the zoom button to make it large
# enough so that we can see it. Nice, but a bit messy, right? We can remove the labels and
# make it look nicer by changing the vertex sizes to be smaller:

plot(H, vertex.label=NA, vertex.size=5)

# We can color the plot based on metadata in the nodes table:
node_subset <- left_join(data_frame(usid = names(V(H))), nodes)
V(H)$color <- node_subset$liberal_flag
plot(H, vertex.label=NA, vertex.size=5)

# Or, those that changed precidence:
V(H)$color <- node_subset$issue
plot(H, vertex.label=NA, vertex.size=5)

# In R, we can quickly calculate the eigenvalue centrality scores of the graph H. Let's do
# that using the `evcent` function and pulling of the vector of the output (these are the
# actual scores).

eigen_centrality <- evcent(H)$vector

# And the nicest way to look at the results is to find the cases with the highest eigenvalues.
# Let's add these back into the nodes table and sort the results

scores <- data_frame(usid = names(eigen_centrality), eigen_score = eigen_centrality)
nodes <- left_join(nodes, scores)
nodes <- arrange(nodes, desc(eigen_score))

# Take a look at the results in the nodes table. Do the results seem surprising at all?

# In order to color the plot based on the eigenvalue centrality scores, we need a line of
# relatively complex R code.
V(H)$color <- rev(heat.colors(30))[cut(log(0.01 + eigen_centrality), 30, labels = FALSE)]

# Now, let's plot the graph again:
plot(H, vertex.label=NA, vertex.size=5)

# Betweeness centrality is an alternative measurment of centrality that can be quite useful.
# Let's calculate it here:
between_centrality <- betweenness(H)

# We'll add it to the original dataset again:
scores <- data_frame(usid = names(between_centrality), between_score = between_centrality)
nodes <- left_join(nodes, scores)
nodes <- arrange(nodes, desc(between_score))

# Plotting this, notice how different it is in places compared to the eigenvector centrality
V(H)$color <- rev(heat.colors(30))[cut(log(1 + between_centrality), 30, labels = FALSE)]
plot(H, vertex.label=NA, vertex.size=5)

# So, how similar are these measurments?
cor(between_centrality, eigen_centrality)
plot(between_centrality, eigen_centrality)

# Construct a measurement of gatekeepers and then add this to the nodes table and sort:
gate_keepers <- as.integer(eigen_centrality < 0.36 & between_centrality > 200)
gates <- data_frame(usid = names(eigen_centrality), gate_keepers = gate_keepers)
nodes <- left_join(nodes, gates)
nodes <- arrange(nodes, desc(gate_keepers))

# What are all of these cases about?

# Finally, let's plot the gate keepers using color on the plot:
V(H)$color <- gate_keepers
plot(H, vertex.label=NA, vertex.size=5)

# Community detection:
set.seed(1)
w <- edge.betweenness.community(H)

# How many nodes are in each group:
sort(table(w$membership))

# Let's construct a table and add this to the set of nodes:
groups <- data_frame(usid = w$names, group = w$membership)
nodes <- left_join(nodes, groups)
nodes <- arrange(nodes, desc(group), term)

# As usual, we can visualize these in the plot window:
V(H)$color <- w$membership
plot(H, vertex.label=NA, vertex.size=5)

# Now, on your own: Try using topics c(20010, 20130, 20140), having to do with sex discrimination,
# or an area of interest to you. Notice how easy it is to re-run all of our analyses with this change.



