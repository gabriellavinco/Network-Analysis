#                       ##############################################                             #
#                       ###         NETWORK ANALYSIS REPORT        ###                             #
#                       ###      BY GABRIELLA VINCO (r0825871)     ###                             #
#                       ##############################################                             #


# clear everything
rm(list=ls())
setwd("~/Desktop/networkanalysis/Week 1")
#check wd
getwd()
# packages
library(sm)
library(ggplot2)
library(reshape2)
library(ggpubr)
s

#-------------------
### 1.  LOAD DATA
#-------------------
affective_w1 <- as.matrix(read.csv("RECENS_data/2100_affective_w1.csv",
                                   header=TRUE, row.names=1, sep=","))
affective_w2 <- as.matrix(read.csv("RECENS_data/2100_affective_w2.csv",
                                   header=TRUE, row.names=1, sep=","))
sex <- as.matrix(read.csv("RECENS_data/2100_sex.csv",
                          header=TRUE, row.names=1, sep=","))
drink <- as.matrix(read.csv("RECENS_data/2100_drink.csv",
                          header=TRUE, row.names=1, sep=","))
trust_w1 <- as.matrix(read.csv("RECENS_data/2100_trust_w1.csv",
                              header=TRUE, row.names=1, sep=","))
trust_w2 <- as.matrix(read.csv("RECENS_data/2100_trust_w2.csv",
                              header=TRUE, row.names=1, sep=","))


# Renaming columns and rows to simpler terms
student_codes <- colnames(affective_w1)
student_number <- c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
dfs <- list(affective_w1, affective_w2, drink, trust_w1, trust_w2)

rownames(affective_w1) <- student_number
rownames(affective_w2) <- student_number
colnames(affective_w1) <- rownames(affective_w1)
colnames(affective_w2) <- rownames(affective_w2)
rownames(drink) <- student_number
rownames(trust_w1) <- student_number
rownames(trust_w2) <- student_number
colnames(trust_w1) <- rownames(trust_w1)
colnames(trust_w2) <- rownames(trust_w2)


#--------------------------------
### 2. INITIAL DATA EXPLORATION
#--------------------------------
## Affective Wave 1 Exploration
class(affective_w1)
mode(affective_w1)
dim(affective_w1)
rownames(affective_w1)
colnames(affective_w1)
table(affective_w1)
affective_w1

## Affective Wave 2 Exploration
class(affective_w2)
mode(affective_w2)
dim(affective_w2)
rownames(affective_w2)
colnames(affective_w2)
table(affective_w2)
affective_w2

# Recoding the Variables
friendship_w1 <- affective_w1
friendship_w1[friendship_w1 %in% c(-2:1)] <- 0
friendship_w1[friendship_w1 == 2] <- 1

friendship_w2 <- affective_w2
friendship_w2[friendship_w2 %in% c(-2:1)] <- 0
friendship_w2[friendship_w2 == 2] <- 1

# (0) Absent, (1) Non Drinking, (2) Frequently Drinking, (3) Usually Drinking
drink_w1 <- drink[, 1] 
drink_w2 <- drink[, 2]
drink_w1[drink_w1 %in% NA] <- 0 
drink_w2[drink_w2 %in% NA] <- 0
drink[drink %in% NA] <- 0 

# (1) M, (2) F
sex <- data.frame(sex)
names(sex) <- c('sex')
sex$Male <- 0
sex$Male[sex$sex == 1] <- 1
sex$Female <- 0
sex$Female[sex$sex == 2] <- 1
sex$sex <- NULL
# sex

# changing column and row names to represent number in order of class (simplify)
student_id <- data.frame(student_number)
names(student_id) <- c('NEW_ID')
student_id$OG_ID <- student_codes

#-----------------------------------------------------
### DEEPER EXPLORATION (CLASS SIZE AND MISSING DATA)
#-----------------------------------------------------

# How many students are in the classroom?
# (as many as rows in your data objects)
class_size <- dim(friendship_w1)[1]
class_size

# How many boys and girls?
gender_comp <- table(sex)
gender_comp

# How much missing data is in the friendship network?
friend_miss_w1 <- sum(is.na(friendship_w1))
friend_miss_w2 <- sum(is.na(friendship_w2))
friend_miss_w1
friend_miss_w2
# those on diagonal are not proper missing values, so you can subtract the 
# diagonal length (23) from the total missing data
friend_miss_w1 <- friend_miss_w1 - 23
friend_miss_w2 <- friend_miss_w2 - 23
friend_miss_w1
friend_miss_w2

# proportion of missing 
friend_miss_w1_prop <- friend_miss_w1 / ( nrow(friendship_w1) * (ncol(friendship_w1) - 1) )
# 0.08695652
friend_miss_w2_prop <- friend_miss_w2 / ( nrow(friendship_w2) * (ncol(friendship_w2) - 1) )
# 0.06521739

# How many students were absent at the time of the data collection?
absent_w1 <-round(friend_miss_w1/class_size, digits=0)
absent_w1
# 2
absent_w2 <- round(friend_miss_w2/class_size, digits=0)
absent_w2
# 1


#----------------------------
### DENSITY AND RECIPROCITY
#----------------------------

library(sna)

# How dense is the friendship network?
friend_dens_w1 <- gden(friendship_w1)
friend_dens_w1
# 0.2034632
friend_dens_w2 <- gden(friendship_w2)
friend_dens_w2
# 0.2241015

# How many of the dyads are reciprocated?
friend_rec_w1 <- grecip(friendship_w1)
friend_rec_w2 <- grecip(friendship_w2)
friend_rec_w2

# correcting for the 0 affected reciprocity
friend_rec_w1 <- grecip(friendship_w1, measure="dyadic.nonnull")
friend_rec_w1
# 0.3174603
friend_rec_w2 <- grecip(friendship_w2, measure="dyadic.nonnull")
friend_rec_w2
# 0.4166667 

# How do the in- and outdegree distributions look like in the classroom?
friend_ind_w1 <- degree(friendship_w1, diag=F, cmode ="indegree") # indegrees
friend_ind_w2 <- degree(friendship_w2, diag=F, cmode ="indegree") # indegrees
friend_outd_w1 <- degree(friendship_w1, diag=F, cmode ="outdegree") # outdegrees
friend_outd_w2 <- degree(friendship_w2, diag=F, cmode ="outdegree") # outdegrees

# plots looking at comparison of LIKE DEGREES and varying waves
indegree <- c(friend_ind_w1, friend_ind_w2)
outdegree <- c(friend_outd_w1, friend_outd_w2)
wave_1 <- rep("Wave 1", 23)
wave_2 <- rep("Wave 2", 23)
labels <- c(wave_1, wave_2)

degree_df <-data.frame(labels, indegree, outdegree)
indegree_df <- data.frame(degree_df$labels, degree_df$indegree)
outdegree_df <- data.frame(degree_df$labels, degree_df$outdegree)

# density plots of in & out degree
ggplot(indegree_df, aes(x=indegree, color=labels)) + geom_density()
ggplot(outdegree_df, aes(x=outdegree, color=labels)) + geom_density()

#density and histogram plots of in & out degree
ggplot(indegree_df, aes(x=indegree, color=labels, fill=labels)) + geom_density(alpha=.4) + geom_histogram(aes(y=..density..), alpha=0.2, position="identity", bins=8)
ggplot(outdegree_df, aes(x=outdegree, color=labels, fill=labels)) + geom_density(alpha=.4) + geom_histogram(aes(y=..density..), alpha=0.2, position="identity", bins = 12)


# plots looking at comparison of LIKE WAVE and varying degrees
wave_1_all <- c(friend_ind_w1, friend_outd_w1)
wave_2_all <- c(friend_ind_w2, friend_outd_w2)
ind <- rep("indegree", 23)
out <- rep("outdegree", 23)
labels2 <- c(ind, out)

degree_df2 <-data.frame(labels, wave_1_all, wave_2_all)
wave1_df <- data.frame(degree_df2$labels, degree_df2$wave_1_all)
wave2_df <- data.frame(degree_df2$labels, degree_df2$wave_2_all)

# density plots of wave 1 & 2
ggplot(wave1_df, aes(x=wave_1_all, color=labels)) + geom_density()
ggplot(wave2_df, aes(x=wave_2_all, color=labels)) + geom_density()

#density and histogram plots of wave 1 & 2
ggplot(wave1_df, aes(x=wave_1_all, color=labels, fill=labels)) + geom_density(alpha=.4) + geom_histogram(aes(y=..density..), alpha=0.2, position="identity", bins=12)
ggplot(wave2_df, aes(x=wave_2_all, color=labels, fill=labels)) + geom_density(alpha=.4) + geom_histogram(aes(y=..density..), alpha=0.2, position="identity", bins = 12)


#--------------------
### ASSORTATIVITY
#--------------------

# Plot the friendship network
friend_plot_w1 <- gplot(friendship_w1)
friend_plot_w2 <- gplot(friendship_w2)

# detach sna
detach(package:sna)
detach(package:igraph)
# now reload igraph
library(igraph)

# transform adjacency matrices to igraph objects
graph_w1 <- graph.adjacency(friendship_w1)
graph_w2 <- graph.adjacency(friendship_w2)

# asses gender homophily in friendship choice
assortativity(graph_w1, sex$Female)
# 0.4400727
assortativity(graph_w2, sex$Female)
# 0.1166667

# asses drinking homophily in friendship choice
assortativity(graph_w1, drink_w1)
# -0.08440455
assortativity(graph_w2, drink_w2)
# 0.103087

# degree homophily: do students with similar number of friends tend to be friends with each other?
assortativity.degree(graph_w1)
# -0.02733108
assortativity.degree(graph_w2)
# -0.07870153

#--------------------------
### PLOTTING THE NETWORKS
#--------------------------

graph_w12 <- graph.adjacency(friendship_w1 + friendship_w2)
myLayout <- layout.fruchterman.reingold(graph_w12)
student_labels <- c(1:23)

par(mfrow=c(1,2)) 
# Plot about friendship, drinking, and sex in Wave 1
plot(graph_w1,
     vertex.shape = ifelse(drink_w1 >= 2 | drink_w1 == 1, "square", "circle"),
     vertex.color = ifelse(sex$Female == 1,'pink', 'lightblue'), 
     vertex.label.color=c("black"),
     vertex.label.cex=c(.5),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.3,
     vertex.size = 12,
     layout = myLayout ,
     main = "Wave 1: Friendship Network")
legend("topleft", # maybe try and get the shapes in the legend as well
       c("Females", "Males"), fill = c("pink", "lightblue"),
       inset =.02 ,
       cex = 0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")

# Plot about friendship, drinking, and sex in Wave 2
plot(graph_w2,
     vertex.shape = ifelse(drink_w1 >= 2 | drink_w1 == 1, "square", "circle"),
     vertex.color = ifelse(sex$Female == 1,'pink', 'lightblue'), 
     vertex.label.color=c("black"),
     vertex.label.cex=c(.5),
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.3,
     vertex.size = 12,
     layout = myLayout ,
     main = "Wave 2: Friendship Network")
legend("topleft",  # maybe try and get the shapes in the legend as well
       c("Females", "Males"), fill = c("pink", "lightblue"),
       inset =.02 ,
       cex =0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")

detach(package:sna)
library(igraph)

#-------------------------------------
# GENDER FRIENDSHIP EVALUATION WAVE 1
#-------------------------------------
gg_w1 <- friendship_w1[sex$Female==1, sex$Female==1]
gb_w1 <- friendship_w1[sex$Female==1, sex$Female==0]
bb_w1 <- friendship_w1[sex$Female==0, sex$Female==0]
bg_w1 <- friendship_w1[sex$Female==0, sex$Female==1]

table(sex$Female)
dim(gg_w1)
dim(gb_w1)
dim(bb_w1)
dim(bg_w1)

# now create an empty selection table which we can fill in later
friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("female", "male")
colnames(friend.selection) <- c("female", "male")
# it looks like this
friend.selection # rownames: sex of sender; colnames: sex of receiver

detach(package:igraph)
library(sna)

friend.selection[1,1] <- gden(gg_w1, diag=FALSE)
friend.selection[1,2] <- gden(gb_w1, diag=TRUE)
friend.selection[2,2] <- gden(bb_w1, diag=FALSE)
friend.selection[2,1] <- gden(bg_w1, diag=TRUE)

# here is our selection table
friend.selection
#        female      male
# female 0.242690058 0.0270793
# male   0.003780718 0.5000000

# normalize by average density
(friend.selection.norm <- friend.selection / gden(friendship_w1))
#        female      male
# female 1.19279582 0.1330919
# male   0.01858183 2.4574468


# density among boys is much higher than that among girls
# this probably has to do with the fact that there are only 3 boys as
# compared to 20 girls
# when it comes to interacting outside of your sex, girls are more likely
# to select boys than the other way around


friend.selection[1,1]/friend.selection[1,2]
# 8.962197
# girls tend to select girls over boys 8.962197x more

friend.selection[2,2]/friend.selection[2,1]
# 132.25
# where as boys tend to select boys over girls 132.23x more

# homophily among boys is muuuuuuccch stronger than that of girls
# this is obviously due to the unbalanced proportion between the genders
# in this recens sample


# again we start by creating an empty table
friend.selection.degree <- matrix(NA, 2, 2)
rownames(friend.selection.degree) <- c("female", "male")
colnames(friend.selection.degree) <- c("female", "male")

# fill in the cells
friend.selection.degree[1,1] <- mean(degree(gg_w1, diag=FALSE, cmode="outdegree"))
friend.selection.degree[1,2] <- mean(degree(gb_w1, diag=TRUE, cmode="outdegree"))
friend.selection.degree[2,2] <- mean(degree(bb_w1, diag=FALSE, cmode="outdegree"))
friend.selection.degree[2,1] <- mean(degree(bg_w1, diag=TRUE, cmode="outdegree"))

# look at the result
friend.selection.degree
# more readable this way
print(friend.selection.degree, digits=1)
#        female male
# female   4.15  0.6
# male     0.09  1.0

# compare it with the density-based calculations
print(friend.selection.norm, digits=2)
#        female male
# female  1.193 0.13
# male    0.019 2.46

# the F/F cell in this section is higher than that of the previous section
# this is the case because the size of the network has more of an effect on the 
# density. since we have a smaller network with a major difference in gender groups
# this isn't surprising that there is such a large difference in these values

# density looks at proportion of present ties
# outdegree looks at activity


#-------------------------------------
# GENDER FRIENDSHIP EVALUATION WAVE 2
#-------------------------------------
gg_w2 <- friendship_w2[sex$Female==1, sex$Female==1]
gb_w2 <- friendship_w2[sex$Female==1, sex$Female==0]
bb_w2 <- friendship_w2[sex$Female==0, sex$Female==0]
bg_w2 <- friendship_w2[sex$Female==0, sex$Female==1]

table(sex$Female)
dim(gg_w2)
dim(gb_w2)
dim(bb_w2)
dim(bg_w2)

# now create an empty selection table which we can fill in later
friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("female", "male")
colnames(friend.selection) <- c("female", "male")
# it looks like this
friend.selection # rownames: sex of sender; colnames: sex of receiver

detach(package:igraph)
library(sna)

friend.selection[1,1] <- gden(gg_w2, diag=FALSE)
friend.selection[1,2] <- gden(gb_w2, diag=TRUE)
friend.selection[2,2] <- gden(bb_w2, diag=FALSE)
friend.selection[2,1] <- gden(bg_w2, diag=TRUE)

# here is our selection table
friend.selection
#         female       male
# female 0.25000000 0.03059273
# male   0.03047619 0.33333333

# normalize by average density
(friend.selection.norm <- friend.selection / gden(friendship_w2))
#         female      male
# female 1.1155660 0.1365129
# male   0.1359928 1.4874214

# density among boys is still higher than that among girls
# but not to the extent that it was before

friend.selection[1,1]/friend.selection[1,2]
# 8.171875
# girls tend to select girls over boys 8.171875x more

friend.selection[2,2]/friend.selection[2,1]
# 10.9375
# where as boys tend to select boys over girls 10.9375x more

# homophily among boys is still stronger than that among girls but
# it is much more balanced than within the first wave


# again we start by creating an empty table
friend.selection.degree <- matrix(NA, 2, 2)
rownames(friend.selection.degree) <- c("female", "male")
colnames(friend.selection.degree) <- c("female", "male")

# fill in the cells
friend.selection.degree[1,1] <- mean(degree(gg_w2, diag=FALSE, cmode="outdegree"))
friend.selection.degree[1,2] <- mean(degree(gb_w2, diag=TRUE, cmode="outdegree"))
friend.selection.degree[2,2] <- mean(degree(bb_w2, diag=FALSE, cmode="outdegree"))
friend.selection.degree[2,1] <- mean(degree(bg_w2, diag=TRUE, cmode="outdegree"))

# look at the result
friend.selection.degree
# more readable this way
print(friend.selection.degree, digits=1)
#         female male
# female    4.4  0.7
# male      0.7  0.7

# compare it with the density-based calculations
print(friend.selection.norm, digits=2)
#        female male
# female   1.12 0.14
# male     0.14 1.49

#------------------------------------------
## LOOK FOR SIMILARITIES BETWEEN THE WAVES
#------------------------------------------

# switch packages to sna
detach(package:igraph)
library(sna)

# The Hamming distance
# perhaps the most simple dissimilarity or distance measure known to mankind
# there is an sna function for calculating the Hamming distance
?hdist # counts the number of ties that are in different states (1-0)
(hamming <- hdist(friendship_w1, friendship_w2))
# but we should probably divide this raw number by the the maximum possible distance,
# which is when all ties are in different states
(hamming.prop <- hamming/nties(friendship_w1)) # distance proportionate to max distance

# The simple matching coefficient
# a similar measure but from the point of view of similarity/stability instead of
# distance/change
(matching <- 1 - hamming.prop)
# 0.8142292

# 3.Calculate the Jaccard index between the networks at wave 1 and 2.
A <- sum((friendship_w1 * friendship_w2)==1, na.rm=TRUE) # #ties that exist in both networks
BplusC <- sum((friendship_w1 + friendship_w2)==1, na.rm=TRUE) # #ties that exist in only one network
(jaccard <- A/(A+BplusC))

# 0.4318182

# The relation between Wave 2 and Wave 1 Friendships
(qapI <- netlogit(friendship_w2, friendship_w1, nullhyp="qap", reps=100))
# Network Logit Model
# 
# Coefficients:
#             Estimate  Exp(b)     Pr(<=b) Pr(>=b) Pr(>=|b|)
# (intercept) -2.036882 0.1304348  0       1       0        
# x1           2.496414 12.1388889 1       0       0        
# 
# Goodness of Fit Statistics:
#   
# Null deviance: 597.4929 on 431 degrees of freedom
# Residual deviance: 365.8983 on 429 degrees of freedom
# Chi-Squared test of fit improvement:
#   231.5946 on 2 degrees of freedom, p-value 0 
# AIC: 369.8983 	BIC: 378.0305 
# Pseudo-R^2 Measures:
#   (Dn-Dr)/(Dn-Dr+dfn): 0.3495269 
#   (Dn-Dr)/Dn: 0.3876107 

#-------------------------------
## GENDER EFFECT ON FRIENDSHIP
#-------------------------------
# what is the effect of same sex on friendship?

# IMPORTANT: variables always have to be in matrix form!
# so first we need to create a same sex matrix for this,
# which contains 1 if two students have the same sex and 0 otherwise
same.sex <- sex$Female %*% t(sex$Female) # %*% is matrix multiplication (not cell-wise)
# we have a matrix that contains the values 1 (both boys), 4 (both girls), 2 (opposite sex)
# now we only have to recode it
same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1

# run QAP (note how we need to group the x variables in a list)
(qap3 <- netlogit(friendship_w2, list(friendship_w1, same.sex), nullhyp="qap", reps=100))

# Network Logit Model
# 
# Coefficients:
#             Estimate   Exp(b)     Pr(<=b) Pr(>=b) Pr(>=|b|)
# (intercept) -2.1669169  0.1145302 0.0     1.0     0.00     
# x1           2.4646834 11.7597588 1.0     0.0     0.00     
# x2           0.1838767  1.2018677 0.7     0.3     0.57     
# 
# Goodness of Fit Statistics:
#   
#   Null deviance: 597.4929 on 431 degrees of freedom
# Residual deviance: 365.58 on 428 degrees of freedom
# Chi-Squared test of fit improvement:
#   231.9128 on 3 degrees of freedom, p-value 0 
# AIC: 371.58 	BIC: 383.7783 
# Pseudo-R^2 Measures:
#   (Dn-Dr)/(Dn-Dr+dfn): 0.3498391 
#   (Dn-Dr)/Dn: 0.3881433 


#------------------
### BLOCKMODELING
#------------------
detach(package::igraph)
library(sna)

# wave 1

# How to quantify equivalence? There are also many measures
# e.g. Jaccard index, Hamming distance, Simple matching coefficient, correlation coeffs., ...

gplot(friendship_w1)

equiv.w1 <- equiv.clust(friendship_w1, cluster.method="ward.D2", method="hamming")
# what we got is a cluster tree
plot(equiv.w1)

bm.w1 <- blockmodel(friendship_w1, equiv.w1, k=9)

par(mfrow=c(1,2))
# we can look at the matrix format of the original network
plot.sociomatrix(friendship_w1, diaglab=FALSE)
# and of the rearranged one
plot.sociomatrix(bm.w1$blocked.data, diaglab=FALSE)
# the second one looks more "tidy", doesn't it?

str(bm.w1, 1)
# probably by bm.w3$block.membership
# but watch out - they are in the wrong order!
bm.w1$order.vector

block.members <- bm.w1$block.membership[order(bm.w1$order.vector)]

gplot(friendship_w1, vertex.col=block.members)


# wave 2

# How to quantify equivalence? There are also many measures
# e.g. Jaccard index, Hamming distance, Simple matching coefficient, correlation coeffs., ...

gplot(friendship_w2)
ggtitle('Friendship Wave 2')
equiv.w2 <- equiv.clust(friendship_w2, cluster.method="ward.D2", method="hamming")
# what we got is a cluster tree
plot(equiv.w2)

bm.w1 <- blockmodel(friendship_w1, equiv.w1, k=9)

par(mfrow=c(1,2))
# we can look at the matrix format of the original network
plot.sociomatrix(friendship_w2, diaglab=FALSE)
# and of the rearranged one
plot.sociomatrix(bm.w2$blocked.data, diaglab=FALSE)
# the second one looks more "tidy", doesn't it?

str(bm.w2, 1)
# probably by bm.w3$block.membership
# but watch out - they are in the wrong order!
bm.w2$order.vector

block.members <- bm.w1$block.membership[order(bm.w2$order.vector)]

gplot(friendship_w2, vertex.col=block.members)


#---------------------------
### CLIQUES AND COMMUNITIES
#---------------------------
friendship_w1 <- friendship_w1 + t(friendship_w1) 

friendship_w1[friendship_w1==2] <- 1

# make sure that igraph is detached and sna is loaded
detach(package:sna)
library(igraph)

# The small-world phenomenon: it is widely observed that real-life networks
# (especially large ones) have relatively short average path lengths and high
# clustering (many closed triads); as a consequence, most nodes in the network
# can be reached in only a few steps - it's a small world.

# How small is our analyzed community in this sense?

# Let's put the data into igraph format
friend1 <- graph.adjacency(friendship_w1)
friend1 <- as.undirected(friend1)

# is our network connected?
is.connected(friend1)

# how many components of different size are there?
components <- decompose.graph(friend1)

vcount(components[[1]])

# count the nodes in each component and make a frequency table
table(sapply(components, vcount))
# the main component contains two thirds of the vertices (33 of them)

# look at the network again to confirm the component sizes given by igraph
myLayout <- layout.fruchterman.reingold(friend1)
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)
# seems right

# let's save the main component in a separate object
main.component <- components[[1]]
# this is how you get the component memberships
main.component.members <- as.numeric(substr(names(V(main.component)),2,3))


# we focus on the main component - is it a small world?
average.path.length(main.component) # average path length
diameter(main.component) # length of longest path
transitivity(main.component) # ratio of closed to open triads

# MINITASK: What do you think? Is this a "small world"? Is the average path length short?
#           Is clustering high at the same time?


random.nets <- sna::rgraph(33, 200, edge_density(main.component, loops=F), mode="graph")
rnGdist<-numeric();for (i in 1:200){rnGdist<-c(rnGdist,max(sna::geodist(random.nets[i,,])$gdist))}
rnGtrans<-sna::gtrans(random.nets, mode="graph")

mean(rnGdist[!rnGdist%in%Inf])

mean(rnGtrans)


# regardless of whether we name it a "small-world" network or not, it is clear that
# it has an internal structure - we can explore this

# how many cliques are in the network?
cliques <- cliques(friend1)
length(cliques) # this looks like a long object - why?

# of course, each clique can be a subset of larger cliques...
# maybe counting maximal cliques is more meaningful 
table(sapply(maximal.cliques(friend1), length))


# we can assign a k for each node in the network
cores <- graph.coreness(friend1)

# and we can plot the network with node colors showing k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout)



# we can easily try, for instance, the fast-greedy community detection algorithm on our data
communities <- fastgreedy.community(friend1)
length(communities) # how many communities were identified by the algorithm?
sizes(communities) # shat are their sizes?
membership(communities) # who belongs to which community?

# let's look at the network with nodes colored according to community membership
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout)
# looks nice


par(mfrow=c(2,2))
# the original friendship network
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color="skyblue",
     layout=myLayout,
     main="original network")
# the blockmodel (from the previous script)
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= block.members,
     layout=myLayout,
     main="blockmodel")
# the k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     vertex.color= cores,
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 6,
     vertex.label = "",
     layout=myLayout,
     main="fast'n'greedy")
par(mfrow=c(1,1))


########################
### BEGINNNING OF ERGM
########################



# install.packages("statnet")
# do not load it just yet

# there are two other useful packages for plotting:
# install.packages("latticeExtra")
# install.packages("RColorBrewer")




# let's make some nice plots with the igraph package
library(igraph)

graph1 <- graph.adjacency(friendship_w1)
graph2 <- graph.adjacency(friendship_w2)
graph12 <- graph.adjacency(friendship_w1 + friendship_w2)
myLayout <- layout.fruchterman.reingold(graph12)

# Plotting the networks without attributes
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network Without Attributes Wave 1")
plot(graph2,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network Without Attributes Wave 2")
par(mfrow = c(1, 1))


# Are there groups/clusters in this classroom?
# How stable is the group structure?

# Plotting the networks with gender homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = ifelse(sex$Female == 1, "pink", "lightblue"),
     vertex.shape = ifelse(sex$Female == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = NA,
     layout = myLayout,
     main = "Network With Sex Factor Wave 1")
legend("topleft",  # maybe try and get the shapes in the legend as well
       c("Females", "Males"), fill = c("pink", "lightblue"),
       inset =.02 ,
       cex =0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")
plot(graph2,
     vertex.color = ifelse(sex$Female == 1, "pink", "lightblue"),
     vertex.shape = ifelse(sex$Female == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network With Sex Factor Wave 2")
legend("topleft",  # maybe try and get the shapes in the legend as well
       c("Females", "Males"), fill = c("pink", "lightblue"),
       inset =.02 ,
       cex =0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")
par(mfrow = c(1, 1))


# How much do you think sex homophily explains clustering in the class?

# Plotting the networks with drinking homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = drink_w1,
     vertex.shape = ifelse(drink_w1 <= 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network With Drink Factor Wave 1")
legend("topleft",  # maybe try and get the shapes in the legend as well
       c("0", "1", "2", "3", "4"), fill = c("white", "orange", "lightblue", "forestgreen", "yellow"),
       inset =.02 ,
       cex =0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")
plot(graph2,
     vertex.color = drink_w2,
     vertex.shape = ifelse(drink_w2 <= 1, "square", "circle"),
     edge.width = 2,
     edge.color = "black",
     edge.arrow.size = 0.2,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network With Drink Factor Wave 2")
legend("topleft",  # maybe try and get the shapes in the legend as well
       c("0", "1", "2", "3", "4"), fill = c("white", "orange", "lightblue", "forestgreen", "yellow"),
       inset =.02 ,
       cex =0.5 ,
       box.lty=2, box.lwd=0.2, box.col="black")

par(mfrow = c(1, 1))


# Does drinking seem to be aligned with friendship clusters?




#-----------
#### ERGM
#-----------

# detach the igraph package
detach(package:igraph)

# we model the structure of the first friendship network
library(statnet)

# need to remove the missing values and recode with 0s
# creating a new variable because we are only doing this for the
friend_ergm_w1 <- friendship_w1
friend_ergm_w2 <- friendship_w2
friend_ergm_w1[friend_ergm_w1 %in% NA] <- 0 
friend_ergm_w2[friend_ergm_w2 %in% NA] <- 0 

#only interested in connections so all values above 1 can stay as 1
# recode the network - all values larger than 1 should be 1
threshold <- 1
friend_ergm_w1[friend_ergm_w1 >= threshold] <- 1
friend_ergm_w2[friend_ergm_w2 >= threshold] <- 1

drinkthreshold <-2

ergm_drink_w1 <- drink_w1
ergm_drink_w2 <- drink_w2
ergm_drink_w1[ergm_drink_w1 >= drinkthreshold] <- 1
ergm_drink_w1[ergm_drink_w1 < drinkthreshold] <- 0
ergm_drink_w2[ergm_drink_w2 >= drinkthreshold] <- 1
ergm_drink_w2[ergm_drink_w2 < drinkthreshold] <- 0
# to make things more simple, we should use objects of class "network"
friend1 <- network(friend_ergm_w1)
friend2 <- network(friend_ergm_w2)
# you can add vertex/node/actor attributes to a network object by
# using the %v% operator
friend1 %v% "sex" <- sex$Female
friend1 %v% "drink" <- ergm_drink_w1
friend2 %v% "sex" <- sex$Female
friend2 %v% "drink2" <- ergm_drink_w2

# what do we get?
class(friend1) # network object
mode(friend1)  # of list mode
friend1        # some more information

# Network attributes:
# vertices = 23 
# directed = TRUE 
# hyper = FALSE 
# loops = FALSE 
# multiple = FALSE 
# bipartite = FALSE 
# total edges= 126 
# missing edges= 0 
# non-missing edges= 126 
# 
# Vertex attribute names: 
# drink sex vertex.names 
# 
# No edge attributes


# let's try a model with just a "constant" - a parameter for density
ergm1 <- ergm(friend1~edges)
ergm1
# Generally, the first term that people use is the edges term. 
# It is a statistic which counts how many edges there are in the network

# Call:
# ergm(formula = friend1 ~ edges)
# 
# Maximum Likelihood Coefficients:
#   edges  
#   -1.104  

summary(ergm1)

# Call:
#   ergm(formula = friend1 ~ edges)
# 
# Maximum Likelihood Results:
#   
#       Estimate Std.Error MCMC  %z value   Pr(>|z|)    
# edges  -1.1039     0.1028      0  -10.74   <1e-04 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 701.5  on 506  degrees of freedom
# Residual Deviance: 568.0  on 505  degrees of freedom
# 
# AIC: 570  BIC: 574.2  (Smaller is better. MC Std. Err. = 0)


# it is the odds ratio of if the tie is present or not
#transforming to probability we get density
plogis(coef(ergm1)[['edges']])
# 0.2490119
gden(friend1)
# 0.2490119

# reciprocity is a universal tendency in friendships - include it in the model
ergm2 <- ergm(friend1~edges+mutual)

# Starting maximum pseudolikelihood estimation (MPLE):
#   Evaluating the predictor and response matrix.
# Maximizing the pseudolikelihood.
# Finished MPLE.
# Starting Monte Carlo maximum likelihood estimation (MCMLE):
#   Iteration 1 of at most 60:
#   Error in ergm.MCMLE(init, nw, model, initialfit = (initialfit <- NULL),  : 
#   Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.
#   In addition: Warning messages:
#   1: In mple.existence(pl) : The MPLE does not exist!

# If the MPLE does not exist then the estimates produced by
# software are determined by the precise convergence characteristics of the algorithm. As such, the
# estimates are arbitrary and misleading.

#   2: In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
#   Unable to reach target effective size in iterations alotted.
#   3: In ergm_MCMC_sample(s.obs, control.obs, theta = mcmc.init, verbose = max(verbose -  :
#                                                                                                                                                                           Unable to reach target effective size in iterations alotted.

# a useful option, good for debugging bad models:
# use the verbose=TRUE argument to get more feedback about the estimation
# ergm2 <- ergm(friend1~edges+mutual, verbose=TRUE)

# now look at the results
# summary(ergm2)
# MINITASK: How can you interpret the reciprocity parameter?


# how about clustering?
ergm3 <- ergm(friend1~edges+gwesp(decay=0, fixed=TRUE))
# this took more time... did the estimation converge?
mcmc.diagnostics(ergm3)
# quite well; in case you see the trace plots going up, down or oscillating,
# you can continue the estimation starting from where you left off
ergm3b <- ergm(friend1~edges+gwesp(0, fixed=T), control=control.ergm(init=coef(ergm3)))
mcmc.diagnostics(ergm3b)
# this is probably better
# watch out: if your model is stuck in a local trap (degeneracy), continuing the estimation
# might make things worse and worse by every run...

# look at the results
summary(ergm3b)
# but what does 'gwesp.fixed.0' mean?


plogis(coef(ergm3)[['edges']])
# 0.03866844
plogis(coef(ergm3)[['gwesp.fixed.0']])
# 0.8328741

# what are the potential confounding factors causing clustering?
ergm4 <- ergm(friend1~edges+gwesp(0, fixed=T)+nodematch("sex"), verbose=TRUE)
mcmc.diagnostics(ergm4)
summary(ergm4)


plogis(coef(ergm4)[['gwesp.fixed.0']])
# 0.8216426
plogis(coef(ergm4)[['nodematch.sex']])
# 0.6420188

# what are the potential confounding factors causing clustering?
ergm5 <- ergm(friend1~edges+gwesp(0, fixed=T)+nodematch("sex")+nodeicov("sex")+nodeocov("sex"), verbose=TRUE)
mcmc.diagnostics(ergm5)
summary(ergm5)

plogis(coef(ergm5)[['gwesp.fixed.0']])
# 0.8245628
plogis(coef(ergm5)[['nodematch.sex']])
# 0.8009012
plogis(coef(ergm5)[['nodeicov.sex']])
# 0.3247418
plogis(coef(ergm5)[['nodeocov.sex']])
# 0.3236221

# what are the potential confounding factors causing clustering?
ergm6 <- ergm(friend1~edges+gwesp(0, fixed=T)+nodematch("sex")+nodeicov("sex")+nodeocov("sex")+nodeofactor("drink"))
mcmc.diagnostics(ergm6)
summary(ergm6)


# Monte Carlo Maximum Likelihood Results:
#   
#                 Estimate Std. Error  MCMC %  zvalue Pr(>|z|)    
#   edges          -2.9663     0.4985      0  -5.950  < 1e-04 ***
#   gwesp.fixed.0   1.5734     0.3447      0   4.565  < 1e-04 ***
#   nodematch.sex   1.4204     0.3668      0   3.873 0.000108 ***
#   nodeicov.sex   -0.7511     0.3966      0  -1.894 0.058243 .  
#   nodeocov.sex   -0.7406     0.3863      0  -1.917 0.055223 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 701.5  on 506  degrees of freedom
# Residual Deviance: 522.6  on 501  degrees of freedom
# 
# AIC: 532.6  BIC: 553.7  (Smaller is better. MC Std. Err. = 0.288)


plogis(coef(ergm6)[['gwesp.fixed.0']])
# 0.8240712
plogis(coef(ergm6)[['nodematch.sex']])
# 0.8028746
plogis(coef(ergm6)[['nodeicov.sex']])
# 0.3167101
plogis(coef(ergm6)[['nodeocov.sex']])
# 0.3213349

# ergms do not model individual outcomes, but rather ties. tie or no tie
# ergms express how different factors contribute to the probability that a 
# network tie is observed
# determined by surrounding ties


#wave 2

class(friend2) # network object
mode(friend2)  # of list mode
friend2        # some more information



# what are the potential confounding factors causing clustering?
ergm7 <- ergm(friend2~edges+gwesp(0, fixed=T)+nodematch("sex")+nodeofactor("drink2"))
mcmc.diagnostics(ergm7)
summary(ergm7)

# Monte Carlo Maximum Likelihood Results:
#   
#   Estimate Std. Error MCMC % z value Pr(>|z|)    
#   edges          -3.1924     0.3404      0  -9.378   <1e-04 ***
#   gwesp.fixed.0   1.1954     0.2624      0   4.556   <1e-04 ***
#   nodematch.sex   0.4029     0.2043      0   1.972   0.0486 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 701.5  on 506  degrees of freedom
# Residual Deviance: 489.5  on 503  degrees of freedom

# AIC: 495.5  BIC: 508.2  (Smaller is better. MC Std. Err. = 0.4928)

plogis(coef(ergm7)[['gwesp.fixed.0']])
# 0.7677082
plogis(coef(ergm7)[['nodematch.sex']])
# 0.5993783

## GOODNESS OF FIT

ergm1_gof <- gof(ergm1) 
ergm6_gof <- gof(ergm6) 
ergm7_gof <- gof(ergm7) 

pdf("ergm1_gof.pdf")
plot(ergm1_gof)
dev.off()

pdf("ergm6_gof.pdf")
plot(ergm6_gof)
dev.off()

pdf("ergm7_gof.pdf")
plot(ergm7_gof)
dev.off()


## SAOM

# install.packages("RSiena", repos="http://R-Forge.R-project.org")

library(RSiena)
library(igraph)

nActors <- dim(friendship_w1)[1]

# 2. create internal SIENA objects

# create dependent network variable
friendship.dependent <- sienaDependent(array(c(friendship_w1, friendship_w2),
                                             dim=c(nActors, nActors, 2)))

# create constant actor covariates
drink1.coCovar <- coCovar(drink_w1)
sex.coCovar <- coCovar(sex$Female)

mySienaData <- sienaDataCreate(friendship.dependent,
                               drink1.coCovar,
                               sex.coCovar)

# print report to check
print01Report(mySienaData,
              modelname="mannheim_network_1")


##############################################
# 3. Specify SIENA model

mySienaEffects <- getEffects(mySienaData)


# network effects
mySienaEffects <- includeEffects(mySienaEffects, transTrip, cycle3)
mySienaEffects <- includeEffects(mySienaEffects, inPop)

# homophily effects and ego alter control
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="sex.coCovar")
mySienaEffects <- includeEffects(mySienaEffects, sameX, interaction1="drink.coCovar")

mySienaEffects # check parameters before estimation


##########################################
# 4. Create SIENA algorithm

mySienaModel <- sienaAlgorithmCreate(projname = "mannheim_network_1")

##########################################
# 5. Estimate

siennaresult <- siena07(mySienaModel,
                        data = mySienaData,
                        effects = mySienaEffects,
                        returnDeps = TRUE)

siennaresult

siena.table(siennaresult, type="html", file="results.html")


##########################################
# SIENA GOF


gofResult.indegree <- sienaGOF(siennaresult, IndegreeDistribution, varName = "friendship.dependent", verbose = TRUE)
pdf("plot(gofResult.indegree).pdf")
plot(gofResult.indegree)
dev.off()

gofResult.outdegree <- sienaGOF(siennaresult, OutdegreeDistribution, varName = "friendship.dependent", verbose = FALSE)
pdf("plot(gofResult.outdegree).pdf")
plot(gofResult.outdegree)
dev.off()

# copied from the script on the RSiena website
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
        x <- networkExtraction(i, data, sims, period, groupName, varName)
        require(sna)
        a <- sna::geodist(x)$gdist
        if (cumulative)
        {
                gdi <- sapply(levls, function(i){ sum(a<=i) })
        }
        else
        {
                gdi <- sapply(levls, function(i){ sum(a==i) })
        }
        names(gdi) <- as.character(levls)
        gdi
}

gofResult.geodesic <- sienaGOF(siennaresult, GeodesicDistribution, varName = "friendship.dependent", verbose = FALSE)
pdf("plot(gofResult.geodesic).pdf")
plot(gofResult.geodesic)
dev.off()

# copied from Siena website GOF example script
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
        unloadNamespace("igraph") # to avoid package clashes
        require(sna)
        require(network)
        x <- networkExtraction(i, data, sims, wave, groupName, varName)
        tc <- sna::triad.census(x)[1,levls]
        # triad names are transferred automatically
        tc
}


gofResult.triadcensus <- sienaGOF(siennaresult, TriadCensus, varName = "friendship.dependent", verbose = TRUE)
pdf("plot(gofResult.triadcensus, center = TRUE, scale = TRUE).pdf")
plot(gofResult.triadcensus, center = TRUE, scale = TRUE)
dev.off()

# plot(gofResult.indegree)
# plot(gofResult.outdegree)
# plot(gofResult.geodesic)
# plot(gofResult.triadcensus, center = TRUE, scale = TRUE)
# 
# ##########################################
# Get one of the simulated networks

adjList1 <- siennaresult$sims[[1]]$Data1$friendship.dependent$`1`
adjList <- list(list())

#transform to igraph adjacency list
for(i in 1:29){
        adjList[[i]] <- adjList1[adjList1[,1] == i, 2]
}

simGraph <- graph.adjlist(adjList)

plot(simGraph)

##########################################
# Plot real network and one simulated network (the first one)


graph1 <- graph.adjacency(friendship_w2)
graph2 <- simGraph

myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
     #     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")
plot(graph2,
     vertex.color = "darkblue",
     #     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Simulated network")
par(mfrow = c(1, 1))


