library(R.matlab)


# Link to download the dataset (.mat file).
# https://homepages.inf.ed.ac.uk/rbf/Fish4Knowledge/GROUNDTRUTH/BEHAVIOR/

# Fish4Knowledge project link:
# https://homepages.inf.ed.ac.uk/rbf/Fish4Knowledge/


# Read data.
df <- readMat("fishDetections_total3102.mat")$fish.detections

# Print data frame dimensions.
dim(df)

# Read one of the trajectories (first one).
trj <- df[,,1]

# Inspect its structure.
str(trj)

# Count how many frames this trajectory has.
length(trj$bounding.box.x)

###########################
#### Plot trajectories ####
###########################
library(trajr)

# Select first trajectory.
trj <- df[,,1]

# Compute center of bounding box.
x.coord <- trj$bounding.box.x + (trj$bounding.box.w / 2)
y.coord <- trj$bounding.box.y + (trj$bounding.box.h / 2)
times <- trj$frame.number - trj$frame.number[1] # Make times start at 0.

# Store trajectory in a data frame.
tmp <- data.frame(x.coord, y.coord, time=times)

# Create a trajectory object.
trj.obj <- TrajFromCoords(tmp, fps = 1)

#png("traj_resampled_plot.png", width = 6, height = 4, units = "in", res = 200)
par(mar=c(5,5,2,2))
plot(trj.obj, lwd = 1, xlab="x", ylab="y")
points(trj.obj, draw.start.pt = T, pch = 1, col = "blue", cex = 1.2)

resampled <- TrajResampleTime(trj.obj, 1)
points(resampled, pch = 4, col = "red", cex = 0.8)

legend("bottomright", 
       c("Starting point",
         "Original trajectory",
         "Resampled trajectory"),
       pch = c(16,1,4),
       col=c("black","blue","red"))

#dev.off()
