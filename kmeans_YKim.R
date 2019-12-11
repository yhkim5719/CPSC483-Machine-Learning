# call libraries
library("tidyverse")
library("R.matlab")

# read a file "kmeansdata.mat"
a <- readMat("kmeansdata.mat")
df <- as.data.frame(a$X)

# create 3 centroids (3,3), (6,2), and (8,5)
centroid <- data.frame("X" = c(3,6,8), "Y" = c(3,2,5))

# calculate the distances from each centroid to every points
# make a distance matrix with those distance
cal_dist <- function(km_data, centroid) {
  dist_matrix <- data.frame(NA, nrow = nrow(km_data), ncol=nrow(centroid))
  for (i in 1:nrow(centroid)) {
    for (j in 1:nrow(km_data)) {
      dist_matrix[j,i] <- sqrt(rowSums((t(km_data)[,j] - centroid[i,])^2))
    }
  }
  return(dist_matrix)
}

# labeling every points with the closest centroid
label_closest <- function(dist_matrix) {
  labeling <- data.frame(NA)
  for (i in 1:nrow(dist_matrix)) {
    labeling[i,] <- which.min(dist_matrix[i,])
  }
  return (labeling)
}

# calculate new centroids
new_c <- function(lb_data, centroid) {
  new_centroid <- data.frame(centroid)
  for (i in 1:nrow(new_centroid)) {
#    new_data <- km_data %>% mutate(label = labeling[,1])
    tmp <- lb_data %>% filter(label == i)
    new_centroid[i,1] <- mean(tmp[,1])
    new_centroid[i,2] <- mean(tmp[,2])
  }
  print(new_centroid)
  return (new_centroid)
}

# add labeling to dataset
lb_data <- function(km_data, labeling) {
  new_data <- km_data%>% mutate(label = labeling[,1])
  return(new_data)
}

# plot the initial point and 3 diff coloring centroids
km_init_plot <- function (km_data, centroid) {
  plot(df, pch = 20, col = "blue", xlab = "X", ylab = "Y")
  points(centroid, pch = 23,  cex = 2, col = "blue", bg = c("black", "red", "green"), lwd = 2)
}

# plot the initial point and 3 diff coloring centroids
km_plot <- function (lb_df, centroid) {
  lb_col <- as.factor(lb_df[,3])
  plot(df, pch = 20, col = lb_col, xlab = "X", ylab = "Y")
  points(centroid, pch = 23, cex = 2, col = "blue", bg = c("black", "red", "green"), lwd = 2)
}

# K-means function with # of iteration, dataset, init_centroid
K_means <- function (num_iter, km_data, centroid) {
  km_init_plot(km_data, centroid)
  for (i in 1:num_iter) {
    print(i)
    dist_matrix <- cal_dist(km_data, centroid)
    lb <- label_closest(dist_matrix)
    lb_df <- lb_data(df, lb)
    new_centroid <- new_c(lb_df, centroid)
    centroid <- new_centroid
    # To watch each centroids of each iteration, comment out "if statement", line 76 and 78
    if (i == 1) {
      km_plot(lb_df, centroid)
    }
  }
  km_plot(lb_df, centroid)
}