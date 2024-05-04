library(R.matlab)
library(trajr)


# Read the database file.
df <- readMat("fishDetections_total3102.mat")$fish.detections

#### Extract features ####

n <- dim(df)[3] # Number of trajectories.

minFrames <- 10 # Only trajectories with a minimum number of frames will be considered.

total <- NULL # data frame to save features.

for(i in 1:n){
  
  if(i %% 100)print(paste0("Processing trajectory ", i, "/", n))
  
  trj <- df[,,i]
  
  if(length(trj$frame.number) < minFrames)next;
  
  # Compute center of bounding box.
  x.coord <- trj$bounding.box.x + (trj$bounding.box.w / 2)
  y.coord <- trj$bounding.box.y + (trj$bounding.box.h / 2)
  times <- trj$frame.number - trj$frame.number[1] # Make times start at 0.
  
  tmp <- data.frame(x.coord, y.coord, time=times)
  
  tmp.trj <- TrajFromCoords(tmp, fps = 1)
  
  resampled <- TrajResampleTime(tmp.trj, 1)
  
  derivs <- TrajDerivatives(resampled)
  
  f.meanSpeed <- mean(derivs$speed)
  f.sdSpeed <- sd(derivs$speed)
  f.minSpeed <- min(derivs$speed)
  f.maxSpeed <- max(derivs$speed)
  
  f.meanAcc <- mean(derivs$acceleration)
  f.sdAcc <- sd(derivs$acceleration)
  f.minAcc <- min(derivs$acceleration)
  f.maxAcc <- max(derivs$acceleration)
  
  features <- data.frame(id=paste0("id",i), label=trj$classDescription[1],
                         f.meanSpeed, f.sdSpeed, f.minSpeed, f.maxSpeed,
                         f.meanAcc, f.sdAcc, f.minAcc, f.maxAcc)
  
  total <- rbind(total, features)
}

# Save dataset.
write.csv(total, "fishFeatures.csv", quote = F, row.names = F)


#### Make some plots. ####

# Read dataset.
dataset <- read.csv("fishFeatures.csv", stringsAsFactors = T)

# Print first rows of the dataset.
head(dataset)

table(dataset$label)

#### Make a MDS plot but first normalize the data ####

normalize <- function(trainset, testset){
  # Function to normalize a train and test set
  # based on the parameters learned from the trainset.
  
  # Iterate columns
  for(i in 1:ncol(trainset)){
    
    c <- trainset[,i] # trainset column
    c2 <- testset[,i] # testset column
    
    # Skip if the variable is not numeric or integer.
    if(class(c) != "numeric" && class(c) != "integer")next;
    
    max <- max(c, na.rm = T) # Learn the max value from the trainset's column.
    min <- min(c, na.rm = T) # Learn the min value from the trainset's column.
    
    if(max==min){ # If all values are the same set it to max.
      trainset[,i] <- max
      testset[,i] <- max
    }
    else{
      
      # Normalize trainset's column.
      trainset[,i] <- (c - min) / (max - min)
      
      # Truncate max values in testset.
      idxs <- which(c2 > max)
      if(length(idxs) > 0){
        c2[idxs] <- max
      }
      
      # Truncate min values in testset.
      idxs <- which(c2 < min)
      if(length(idxs) > 0){
        c2[idxs] <- min
      }
      
      # Normalize testset's column.
      testset[,i] <- (c2 - min) / (max - min)
    }
  }
  
  return(list(train=trainset, test=testset))
}


dataset <- normalize(dataset, dataset)$train

summary(dataset)

labels <- unique(dataset$label)

cols <- as.integer(dataset$label) + 1

d <- dist(dataset[,3:ncol(dataset)])

fit <- cmdscale(d, k = 2) # k is the number of dimensions.

x <- fit[,1]; y <- fit[,2]

#png("mdsFishes.png", width = 5, height = 4, units = "in", res = 720)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="MDS trajectories features", pch=19, col=cols, cex=0.7)
legend("topleft", legend = labels, pch=19, col=unique(cols), cex=0.7, horiz = F)
#dev.off()
