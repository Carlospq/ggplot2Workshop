#Part of this workshop has been adapted from (Thomas Lin Pedersen) GitHub repository: https://github.com/thomasp85/ggplot2_workshop
#Link to youtube video: https://www.youtube.com/watch?v=h29g21z0a68
#Plenty of examples here: http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization

#It's dangerous to go alone! Take this (cheat sheet): https://rstudio.github.io/cheatsheets/data-visualization.pdf



# Libraries needed for this workshop:
# Package names
packages <- c('ggplot2', 'dplyr', 'grid', 'gridExtra', 'tidyr', 'reshape2')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Optional (for some examples):
optional_packages <- c('maps', 'ggrepel', 'ggraph', 'igraph', 'tidygraph', 'GGally', 'treeio', 'ggtree', 'chorddiag', 'wordcloud2', 'gt', 'RColorBrewer')

# Install packages not yet installed
installed_packages <- optional_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(optional_packages[!installed_packages])
}

# Packages loading
invisible(lapply(optional_packages, library, character.only = TRUE))


##########################################################################################
### reshaping data ###

# A dataset can be written in two different formats: wide and long. A wide format has values that do not repeat in the first column. A long format has values that do repeat in the first column.
# Iris data is presented in wide format. In some cases ggplot will require long format in order to plot the data correctly.
# To transform from wide to long we have several options:

data(iris)
head(iris)

# gather() from tidyr
# To transform from long to wide you can use spread()
gather(iris,                      # Data object
       feature,                   # Name of new key column (made from names of data columns)
       value,                     # Name of new value column
       Sepal.Length:Petal.Width,  # Names of source columns that contain values
       factor_key=TRUE)           # Treat the new key column as a factor (instead of character vector)

# melt() from reshape2
head(
  melt(iris, id.vars=c("Species"))    # Specify id.vars: the variables to keep but not split apart on
)  
# There are options for melt that can make the output a little easier to work with:
head(
  melt(iris,
        id.vars=c("Species"),                            # ID variables - all the variables to keep but not split apart on
        measure.vars=c("Sepal.Length", "Sepal.Width",    # The source columns
                       "Petal.Length", "Petal.Width"),
        variable.name="variable",                        # Name of the destination columns that will identify the original columns that the measurement came from
        value.name="value")
)


##########################################################################################
### basic plot with ggplot and mapping ###

# The main function of ggplot is the ggplot() function. It has two main arguments that you need to be aware of:

# Data: the (default) dataset to use for the plot 
# Mapping: the aesthetic mappings 

# When calling ggplot() we typically don’t call these arguments by their name and instead simply supply them in order.

# Data
#
# The data argument obviously needs a dataset and this should usually come in the form of a data.frame or a tibble.
# Let’s starts with a very simple data set, iris, that contains 4 measures + species name for each plant.

# Mapping
#
# The mapping argument should be supplied with a call to the aes() function that lists the mappings between variables in your data and aesthetics of the plot.
# Which aesthetics you choose to map to depends on what geoms you intend to use.
# But for now, let’s assume that we want to map "Petal.Length" to the x axis, and "Petal.Width" to the y axis.
# To do so, we simply use aes(x = Petal.Length, y = Petal.Width) as the input to the mapping argument.
# Because the first two arguments of aes() are x and y, we can do without calling them explicitly.
ggplot(data = iris,
       mapping = aes(x = Petal.Length,
                     y = Petal.Width))

# Why does this result in a blank canvas? After all, we did tell ggplot to map Petal.Length to the x axis and Petal.Width to the y axis?
# The answer is that we still haven’t told ggplot2 which layers we want to use.

# Layers
#
# To add new layers to the main function we only need to 'add' (+) the code for the new layer.
#
# The aesthetic mapping is not enough by itself—we also need a layer to display our data.
# In ggplot2, we accomplish this by adding (stacking) layers to the output of ggplot().
# There are several ways to construct layers in ggplot, but the simplest and most common approach is to use the functions named geom_*() (such as geom_point()).

# Aesthetics
# Aesthetics mapped in ggplot() will be shared between all layers
# Each aesthetic can have its own mapping
# You can look at the documentation of the geom_*() function using the help function in R
?geom_point()
# Under Aesthetics we can see that this layer understands the following aesthetics:
#
#   x
#   y
#   alpha
#   colour
#   fill
#   group   
#   shape
#   size
#   stroke

# Exercises:
# 1) Generate a dotplot for Petal.Length ~ Petal.Width
# 2) Add a regression line (geom_smooth() with `formula = y ~ x` to the plot
# 3) Map the speceies information to the color of the dots
# 4) Change size and shape of dots
# 5) Add a rectangle ( geom_rect() ) and fill it in green


##########################################################################################
### geoms() ###

# There are more than 40 geoms in the ggplot2 package with many more geoms developed in other packages
# Exercises:
# 1) Plot Species names instead of dots using geom_text. You will need to map Species names to `label`
# 2) Generate a boxplot or violinplot for Petal.Length data with one box for each species
# 3) Repeat the boxplot but now try to have all variables (Petal.Length, Petal.Width, Sepal.Length, Sepal.Width) in the same plot (*You need the long format to do this plot)
# 4) Plot the distribution of Petal.Length by species (geom_density())
# 5) Add the bar plot on top of the previous density plot


##########################################################################################
### stats ###

# To know which statistics are calculated on each stat_* function check the section 'Computed variables' on its help page
# We can access this calculated values using after_stat()
# ?stat_count

# Exercises:
# 1) Using mpg dataset Generate a barplot showing car counts per classes. Show counts in percentage of the total. This will require a new mapping in the geom


##########################################################################################
### annotation ###
# This function adds geoms to a plot, but unlike a typical geom function, the properties of annotations are not mapped from variables of a data frame,
# but are instead passed in as vectors. This is useful for adding small annotations (such as text labels)
# First argument of annotate() is the geom that will be used to annotate( annotate(geom='...', ... ) )

# Using the dot plot from the first examples...
p <- ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) +
            geom_point(size=3) +
            stat_smooth(method = "lm", 
                        formula = y ~ x, 
                        geom = "smooth", 
                        alpha=.2)
p

# Exercieses:
# 1) Annotate the names of the species inside the plot
# 2) Highlight dots of setosa by annotating a rectangle that cover all dots


##########################################################################################
### Multi plots ###

# facet_grid() allows us to split data in different panels according to specific variables
# To do so it is recommended to use the long format
# This function will split (map) the data into 'rows ~ columns'

# Exercises:
# 1) Plot each measure (Sepal.Length, Sepal.Width, ...) in different rows, map Species to colors
# 2) Now split the previous plot by placing each Species in different columns (keep variables in different row)
# 3) Make the plots easier to read by adjusting the Y scale in each row (check ?facet_grid to find the correct option)

# facet_grid draws 1 plot on a canvas. We can draw different plots on 1 canvas using grid.arrange (libraries grid & gridExtra)
# Shape of grid can be specified by a layout_matrix 
# We need to pass a list of ggplot objects (or 'grobs') as input
# *A graphical object (“grob”) is a description of a graphical item. These basic classes provide default behaviour for validating, drawing, and modifying graphical objects.

# Exercises:
# 1) Save 2 plots into 2 different grob objects
# 2) Use grid.arrange() to plot the previous 2 objects together, either in 2 columns or in two rows
# 3) Draw the 2 plots with the same size and make them share the same legend (Hint: use the following code to extract the legend as a grob)

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 


##########################################################################################
###  Scales & Coordinates ###

# Coordinates allows us to transform mappings. 
# For example:
#   - a cartesian coordinates system allows us to zoom in and out of a plot
#   - a polar coordinate system interprets x and y as angles and radius

# The commands for scales follow the pattern scale_'aesthetic'_'mehtod'()

# Exercises:
# 1.1) Using Sepal.Length draw a bar plot and fill the bars by species
# 1.2) Scale x axis into bins 
# 1.3) zoom in by scaling the y axis (scale_y_continuous) and set the limits to show only data below 10 counts
# 1.4) repeat the zoom in but this time using coord_cartesian(). Compare results with previous step

# 2.1) Using the following exponential data transform the axis to obtain a straight line (diagonal)
data <- data.frame(x=1:100, y=10**c(1:100))

# 3.1) Use the following data to create a bar chart with only one stacked bar, and fill by group (hint: You can always map "" to a aesthetic)
# 3.2) Transform the y axis into angles using coord_polar(...)
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)


##########################################################################################
### colors & themes ###

# manual control of color/fill scales
# The comands follow the pattern scale_'aesthetic'_'mehtod'()
# scale_fill_distiller() # to provide a ColorBrewer palette
# scale_color_viridis()  # to use Viridis. Do not forget discrete=FALSE for a continuous variable.
# scale_fill_viridis_[b: binary; c:continuos; d:discrete]()

# scale_fill_manual()    # to provide manual colors to fill
# list of color names for ggplot2: http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Exercises: 
# 1) Using the data above draw a heatmap (geom_tile()) 
# 2) Change the filling using different scale_*
# 3) Use iris data to draw again a box plot and change color and fill manually


# Title and labs
p <- ggplot(iris, aes(Species, Petal.Length, color=Species, fill=Species)) +
  geom_boxplot(alpha=.2) +
  scale_color_manual(values = c("sienna", "seagreen", "indianred2")) +
  # Hide legend for fill_manual
  scale_fill_manual(values = c("chocolate2", "olivedrab3", "tomato3"), guide = "none")

p + ggtitle("Distribution of petal length") +
    xlab("Species name") +
    ylab("Petal length (cm)")

p + labs(title="Distribution of petal length",
         x="Species name", y="Petal length (cm)",
         fill="Groups: Species")

# Any other aesthetic in the plot can be modify using theme
# Inside theme(...) you can control aesthetics for panel, plot, axis, legend, ...
# Each aesthetic is defined by an `element_'shape'` constructor (text, line, blank, ...)
# e.g. ggplot() + theme(axis.text = element_text(size=12)) -> To control the fontsize of (both) axis texts 
# There are some predefined themes in ggplot such as theme_bw(). theme_minimal(), theme_classic(), ...

# Exercises:
# 1) Modify the aesthetics of your last plot to make it more appealing








##########################################################################################
### Useful examples ###

## Find plenty of examples here: https://r-graph-gallery.com/index.html
## Some of the following examples require extra libraries
## This libraries allow you to use new plotting functions similar to ggplot2. They also follow the Grammar of Graphic (same as ggplot2)



## Create your own legend
#create data frame
df <- data.frame(x=c(1, 2, 2, 3, 5, 6, 8, 8, 9, 9, 10, 11, 12, 15, 15),
                 y=c(2, 3, 3, 4, 5, 5, 6, 7, 8, 8, 9, 10, 16, 19, 28))

#create plot with three fitted regression models
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(se=FALSE, aes(color='Linear')) +
  geom_smooth(formula=y~poly(x, 2), se=FALSE, aes(color='Quadratic')) +
  geom_smooth(formula=y~poly(x, 3), se=FALSE, aes(color='Cubic')) +
  scale_color_manual(name='Regression Model',
                     breaks=c('Linear', 'Quadratic', 'Cubic'),
                     values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple'))




## Time series

# Dummy data
set.seed('12345')
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

ggplot(data, aes(x=day, y=value, color=value)) +
  geom_line() + 
  geom_point() +
  annotate(geom="text", x=as.Date("2016-09-9"), y=1.9, 
           label="Value dropped below 2\non September 10th, 2016", hjust=1.2) +
  annotate(geom="point", x=as.Date("2016-09-10"), y=1.9, size=10, shape=21, color="red", fill="transparent") +
  xlab("") + 
  scale_x_date(date_labels = "%Y %b %d", date_minor_breaks = "2 day") +
  theme_bw() +
  #You can zoom in into specific time frames using scale_x_date(limit=...)
  scale_x_date(limit=c(as.Date("2016-06-01"),as.Date("2017-1-1")))

# Dual axis
data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  temperature = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)

ggplot(data, aes(x=day)) +
  geom_line( aes(y=temperature), size = 2, color = "#69b3a2") + 
  geom_line( aes(y=price / 10), size = 2, color = "steelblue") +
  scale_y_continuous(
    name = "Temperature (Celsius °)",
    # add secondary axis
    sec.axis = sec_axis( trans=~.*10, name="Price ($)")) +
  theme_bw() +
  theme(axis.title.y = element_text(color = "#69b3a2", size=13, hjust = 1),
        axis.title.y.right = element_text(color = "steelblue", size=13, hjust = 0)) +
  ggtitle("Temperature down, price up")




## Volcano plot

library(ggrepel)
# Get data 
# download.file("https://raw.githubusercontent.com/biocorecrg/CRG_RIntroduction/master/de_df_for_volcano.rds", "de_df_for_volcano.rds", method="curl")
tmp <- readRDS("de_df_for_volcano.rds")

# remove rows that contain NA values
de <- tmp[complete.cases(tmp), ]

# add a column of NAs for DifferentialExpressed flag
de$diffexpressed <- "NO"

# if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
de$diffexpressed[de$log2FoldChange > 0.6 & de$pvalue < 0.05] <- "UP"

# if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"
de$diffexpressed[de$log2FoldChange < -0.6 & de$pvalue < 0.05] <- "DOWN"

# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
de$delabel <- NA
de$delabel[de$diffexpressed != "NO"] <- de$gene_symbol[de$diffexpressed != "NO"]

# Plot (theme 1)
ggplot(data=de, aes(x=log2FoldChange, y=-log10(pvalue), col=diffexpressed, label=delabel)) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(show.legend = FALSE) +
  # Add lines for thresholds
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red") +
  # Change point color
  scale_color_manual(values=c("UP"="blue", "NO"="black", "DOWN"="red"))

# Plot (theme 2)
ggplot(de, aes(x = log2FoldChange, y = -log10(pvalue), fill = diffexpressed, size = diffexpressed, alpha = diffexpressed, label=delabel)) + 
  geom_point(shape = 21, colour = "black") +
  geom_text_repel(size = 4, show.legend = FALSE) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") + 
  geom_vline(xintercept = c(-0.6, 0.6), linetype = "dashed") +
  theme_classic() +
  scale_fill_manual(values = c("UP" = "#ffad73", "DOWN" = "#26b3ff", "NO" = "grey") ) +
  scale_size_manual(values = c("UP" = 2, "DOWN" = 2, "NO" = 1)) +
  scale_alpha_manual(values = c("UP" = 2, "DOWN" = 2, "NO" = 1)) +
  scale_x_continuous(breaks = c(seq(-1, 1, 0.2)), limits = c(-1, 1)) 




## 2d Histogram

# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)


# Plots
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

ggplot(data, aes(x=x, y=y) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# Area + contour
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))



## Phylogenetic Trees
# Full documentation here: https://yulab-smu.top/treedata-book/index.html
library("treeio")
library("ggtree")

nwk <- system.file("extdata", "sample.nwk", package="treeio")
tree <- read.tree(nwk)

ggplot(tree, aes(x, y)) + 
  geom_tree() +
  theme_tree()


# Or
ggtree(tree, color="firebrick", size=1, linetype="dotted") +
  geom_point(aes(shape=isTip, color=isTip), size=3) 
  
ggtree(tree, color="firebrick", size=1, linetype="dotted", layout="circular") +
  geom_nodepoint(color="#b5e521", alpha=1/4, size=10) +
  geom_tippoint(color="#FDAC4F", shape=8, size=3) +
  geom_tiplab(color='firebrick', hjust = -0.5)




## Radar/Spider charts

# Approach using coord_polar()
# Get mean of measures by species
df <- iris %>%
        group_by(Species) %>%
        summarise_at(vars(1:4), list(name = mean))
meltdf <- melt(df, value.name = "mean")

ggplot(meltdf, aes(x=variable, y=mean, color=Species, group=Species)) +
  geom_point(size=2) +
  geom_line() +
  coord_polar() +
  theme_minimal()

# Using radarchart() from fmsb
# Input data format is very specific. Each row must be an entity. Each column is a quantitative variable. First 2 rows provide the min and the max that will be used for each variable. 
library(fmsb)

fmsb_df <- rbind(max=8, 0, df)
fmsb_df <- fmsb_df[,2:5]
legend_names <- c("setosa", "versicolor", "virginica")
colors_in <-     c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
radarchart(fmsb_df,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
legend(x=0.7, y=1, legend = legend_names, bty = "n", pch=20 , col=colors_border , text.col = "darkgrey", cex=1.2, pt.cex=3)




## Networks

library(ggraph)
library(tidygraph)
library(igraph)

# Create graph from highschool friendships data
network <- as_tbl_graph(highschool) %>% 
              mutate(Popularity = centrality_degree(mode = 'in'))

# Not specifying the layout - defaults to "auto"
ggraph(network) + 
  geom_edge_link(aes(colour = factor(year))) + 
  geom_node_point(aes(size = Popularity), color="darkgreen", alpha=.5) +
  facet_grid(~year) +
  theme_bw()

ggraph(network, layout = 'linear') + 
  geom_edge_arc(aes(colour = factor(year)))

network <- graph_from_data_frame(flare$edges, vertices = flare$vertices)
ggraph(network, 'circlepack') + 
  geom_node_circle(aes(fill = depth), size = 0.25, n = 50) + 
  coord_fixed()

ggraph(network, 'circlepack') + 
  geom_edge_link() + 
  geom_node_point(aes(colour = depth), size = 2) +
  coord_fixed()

# Creating graph with new data from scratch:
links <- data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A", "E", "F", "G", "H", "I","I"),
  importance=as.factor(sample(1:4, 12, replace=T))
)
nodes <- data.frame(
  name=LETTERS[1:10],
  carac=c( rep("young",3),rep("adult",2), rep("old",5))
)

# Turn it into igraph object
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 

# Plot
ggraph(network, layout = 'kk') + 
  geom_edge_link(mapping = aes(linetype = importance)) +
  geom_node_point(size = 10, color="black") +
  geom_node_point(aes(color=carac), size = 9) +
  geom_node_text(aes(label=name), color="white") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')


# Circus plot from network
# Dummy data
  d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
  d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
  hierarchy <- rbind(d1, d2)
  
  # create a vertices data.frame. One line per object of our hierarchy, giving features of nodes.
  vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) )
  vertices$group  <-  hierarchy$from[ match( vertices$name, hierarchy$to ) ]
  vertices$value <- runif(111)
  angles <- append(rep(0, 11), 90 - 360 * (1:100-0.5) / 100)
  hjust <- ifelse( angles < -90, 1, 0)
  
  # Create a graph object with the igraph library
  mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )

# create a dataframe with connection between leaves (individuals)
  all_leaves <- paste("subgroup", seq(1,100), sep="_")
  connect <- rbind( 
    data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
    data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
    data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
    data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) 
  )
  # The connection object must refer to the ids of the leaves:
  from <- match( connect$from, vertices$name )
  to <- match( connect$to, vertices$name )

# plot 1
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()

# plot 2
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  geom_node_text(aes(label=name), angle=angles, hjust=c(rep(0.5,11), rep(-0.3, 100))) +
  scale_x_continuous(expand = c(.2, .2)) +
  scale_y_continuous(expand = c(.2, .2)) +
  theme_void()

# plot 3
library(RColorBrewer)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.3, width=0.9, aes(color=group)) +
  scale_edge_color_manual(values= rep( brewer.pal(9,"Paired") , 30), guide = "none") +
  #scale_edge_colour_distiller(palette = "RdPu") +
  
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angles, hjust=0, colour=group), size=2, alpha=1) +
  
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, alpha=0.2, size=value)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30), guide = "none") +
  scale_size_continuous(range = c(0.1,10), guide = "none") +
  scale_alpha(guide = "none") +
  
  theme_void() +
  
  theme(
    legend.position="right",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))


# Circus plots
library(chorddiag) #devtools::install_github("mattflor/chorddiag")
m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
chorddiag(m, groupColors = groupColors, groupnamePadding = 20)




## Wordcloud
library(wordcloud2) 

# Gives a proposed palette
wordcloud2(demoFreq, size=1.6, color='random-dark')




## Lolipop
# Create data
data <- data.frame(
  x=LETTERS[1:26],
  y=abs(rnorm(26))
)

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y") +
  coord_flip()

# Change baseline
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")

# Dummy example with iris
df <- cbind(iris[iris$Species=="setosa", c("Petal.Length", "Petal.Width")], id=1:50)
ggplot(df) +
  geom_point(aes(x=Petal.Length-Petal.Width, y=id), size = 3, color="darkred", alpha=.7) +
  geom_point(aes(x=Petal.Length+Petal.Width, y=id), size = 3, color="seagreen", alpha=.7) +
  geom_segment(aes(x=Petal.Length-Petal.Width, xend=Petal.Length+Petal.Width, y=id, yend=id)) +
  theme_bw()




## Circular bar plots

# Create dataset
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)
angles <- 90 - 360 * (data$id-0.5) / nrow(data)

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, labels = id)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  geom_text(aes(label=paste("mister", id)), vjust=0.5, hjust=-0.3, angle = angles) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)




## Tables into plots
library(gt)
library(dplyr)

# Create a simple data frame
data = data.frame(
  Country = c("USA", "China", "India", "Brazil"),
  Capitals = c("Washington D.C.", "Beijing", "New Delhi", "Brasília"),
  Population = c(331, 1441, 1393, 212),
  GDP = c(21.43, 14.34, 2.87, 1.49)
)

data %>%
  gt() %>%
  tab_header(title = md("What a **nice title**"),
             subtitle = md("Pretty *cool subtitle* too, `isn't it?`")) %>%
  tab_spanner(label = "Number",
              columns = c(GDP, Population)) %>%
  tab_spanner(label = "Label",
              columns = c(Country, Capitals))

  
  
  
  
  
  
  
