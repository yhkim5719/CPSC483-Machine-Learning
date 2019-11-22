# calculate the current Total Entropy
total_E <- function(ds) {
  values<-unique(ds[,length(ds)])
  num_la <- nrow(ds[ds[,length(ds)] == values[1],])    #replace "1" with "leave-alone", later TODO
  num_fi <- nrow(ds[ds[,length(ds)] == values[2],])    #replace "0" with "force-into", later TODO
  total <- num_la + num_fi
  te <- -(num_la/total)*log2(num_la/total)-(num_fi/total)*log2(num_fi/total)
  #   cat("num_fi =", num_fi, "num_la =", num_la, "\n")
  #   view(ds)
  if(num_fi == 0 || num_la == 0) {
    te <- 0
  }
  #   cat("\nCurrent Total Entropy = ", te, "\n")
  if(te == 0) {
    cat("This group will be", ds[1,length(ds)],"\n")
  }
  return(te)
}

# calculate the ith feature's probabilities, P(i=v1), P(i=v2), P(i=v3), etc
P <- function(ds, i) {
  values <- unique(ds[,i])
  #  cat("length of values = ", length(values), "\n")
  p <- vector(mode = "double", length = length(values))
  for (j in 1:length(values)) {
    p[j] <- nrow(ds[ds[,i] == values[j],])/nrow(ds)
    cat("P(", colnames(ds[i]),"=",values[j],") = ", p[j], "\n")
  }
  return(p)
}

# calculate the ith feature's entropy, E(i=v1), E(i=v2), E(i=v3), etc,.
E <- function(ds, i) {
  values <- unique(ds[,i])
  labels <- unique(ds[,length(ds)])
  e <- vector(mode = "double", length = length(values))
  for (j in 1:length(values)) {
    tmp_ds <- filter(ds, ds[,i] == values[j])
    #       view(tmp_ds)
    la <- nrow(tmp_ds[tmp_ds[,length(tmp_ds)] == labels[1],])   #replace "1" with "leave-alone", later TODO
    e[j] <- -la/nrow(tmp_ds) * log2(la/nrow(tmp_ds)) - (1 - la/nrow(tmp_ds)) * log2(1 - la/nrow(tmp_ds))
    if (la == 0 || la/nrow(tmp_ds) == 1) {
      e[j] <- 0
    }
    cat("E(", colnames(ds[i]),"=", values[j],") = ", e[j], "\n")
  }
  return(e)
}

# calculate Information Gain(IG) of ith feature
IG <- function(ds, i) {
  te <- total_E(ds)
  cat("\nCurrent Total Entropy = ", te, "\n")
  ig <- te - sum(P(ds, i) * E(ds, i))
  cat("IG(", colnames(ds[i]),") = ", ig, "\n")
  return (ig)
}

# calculate all the IGs from each attributes
# and add them to a vector
all_IG <- function(ds) {
  ig <- vector(mode = "double", length = length(ds) - 1)
  for (i in 1:length(ig)) {
    ig[i] <- IG(ds, i)
  }
  for (i in 1:length(ig)) {
    cat("IG(", colnames(ds[i]),") = ", ig[i], "\n")
  }
  return (ig)
}

# if the Entropy of this set is not 0,
# pick the label of the highest IG
# then split the dataset by that label's attribute
non_zero_E <- function(ds, ig) {
  max_attr <- which.max(ig)
  cat("Add Node :", colnames(ds[max_attr]), "\n")
  values <- unique(ds[max_attr])
  for(i in 1:nrow(values)) {
    cat("Branch :", values[i, 1], "of", colnames(ds[max_attr]),"-- ")
    sub_ds <- subset(ds, ds[,max_attr] == values[i,1])
    sub_ds[,max_attr] <- NULL
    ID3(sub_ds)
  }
}

# check if all labels are same
check_label <- function(ds) {
  value <- unique(ds[length(ds)])
  return(nrow(value))
}

# ID3 Algorithm
# check the current entropy, if 0, make a node with "positive" or "negative"
# if not 0, calculate IGs of each attributes
# pick the highest IG
# split and recursively call ID3

ID3 <- function(ds) {
  if (length(ds) == 1) {
    cat("Decision Tree is created!\n")
  } else if (check_label(ds) == 1) {
    cat(ds[1,length(ds)],"   // leaf node \n")
  } else {
    ig <- all_IG(ds)
    non_zero_E(ds,ig)        
  }
}
