# Generative Art: Strange Shapes --------------------------------------------------------------
# Packages
devtools::install_github("cutterkom/generativeart")
library(generativeart)
library(ambient)
library(dplyr)

# Set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, 
                   IMG_SUBDIR)
LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, 
                       LOGFILE)
# Create the directory structure
generativeart::setup_directories(IMG_DIR, 
                                 IMG_SUBDIR, 
                                 IMG_SUBDIR2, 
                                 LOGFILE_DIR)

# Include a specific formula
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

# Call the main function to create five images with a polar coordinate system
generativeart::generate_img(formula = my_formula, 
                            nr_of_img = 5, # Set the number of images to generate
                            polar = TRUE, 
                            filetype = "png", 
                            color = "#c1a06e", 
                            background_color = "#1a3657")

# A different formula
my_formula <- list(
  x = quote(runif(1, -10, 10) * x_i - runif(1, -5, 5) * cos(y_i^3)),
  y = quote(runif(1, -10, 10) * sqrt(abs(y_i)) - sin(x_i))
)

generativeart::generate_img(formula = my_formula, 
                            nr_of_img = 5, 
                            polar = TRUE, 
                            filetype = "png", 
                            color = "#c1a06e", 
                            background_color = "#1a3657")

rm(list = ls())



# Metropolis: Generative City Visualizations --------------------------------------------------
# Packages
library(tidyverse)


# Function to generate streets
generate_streets <- function(n, width, height, r, delta, p_branch, initial_pts) {
  # Initialize data frames
  points <- tibble(x = double(n), y = double(n), dir = double(n), level = integer(n))
  edges <- tibble(x = double(n), y = double(n), xend = double(n), yend = double(n), level = integer(n))
  
  # Seed initial points
  points[1:initial_pts, ] <- map_dfr(1:initial_pts, ~tibble(
    x = runif(1, 0, width), 
    y = runif(1, 0, height), 
    dir = runif(1, -2*pi, 2*pi), 
    level = 1
  ))
  
  # Create a text progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  for (i in (initial_pts + 1):n) {
    repeat {
      # Pick a point at random from existing ones
      random_point <- sample_n(points[1:(i - 1), ], 1)
      branch <- runif(1) <= p_branch
      alpha <- random_point$dir + runif(1, -delta, delta) + 
        (branch * (ifelse(runif(1) < 0.5, -1, 1) * pi/2))
      v <- c(cos(alpha), sin(alpha)) * r * (1 + 1 / (branch * (random_point$level + 1) + !branch * random_point$level))
      xj <- random_point$x + v[1]
      yj <- random_point$y + v[2]
      
      # Check if the new point is within bounds and not too close to others
      if (xj >= 0 && xj <= width && yj >= 0 && yj <= height) {
        min_dist <- min(sqrt((points$x[1:(i - 1)] - xj)^2 + (points$y[1:(i - 1)] - yj)^2))
        if (min_dist >= r) {
          lvl_new <- ifelse(branch, random_point$level + 1, random_point$level)
          points[i, ] <- list(xj, yj, alpha, lvl_new)
          edges[i, ] <- list(xj, yj, random_point$x, random_point$y, lvl_new)
          break
        }
      }
    }
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  p <- ggplot(edges %>% filter(level > 0)) +
    geom_segment(aes(x, y, xend = xend, yend = yend, size = -level), lineend = "round") +
    xlim(0, width) +
    ylim(0, height) +
    coord_equal() +
    scale_size_continuous(range = c(0.5, 0.5)) +
    theme_void() +
    theme(legend.position = "none")
  
  # Display plot
  return(p)
  
  # return(list(points = points, edges = edges %>% filter(level > 0)))
}


# Parameters
# Parameters explanation:
# n: Determines the total number of street segments to generate.
# width, height: Dimensions of the canvas where streets are drawn.
# r: Affects street density.
# delta: Affects the curvature of the streets.
# p_branch: Probability of a new street segment branching off from an existing one.
# initial_pts: Number of initial points from which streets will originate.

n <- 5000
# Fewer and shorter streets
# n <- 1000

width <- 10000
height <- 10000

r <- 75
# Make streets more spread out
# r <- 125

delta <- 2 * pi / 180
# Make streets curvy
# delta <- 0.25

p_branch <- 0.1
# Make streets branch less
# p_branch <- 0.05

initial_pts <- 5
# Increase number of points from which streets originate
# initial_pts <- 8

file_name <- "plots/city_streets_art_01.jpeg"

# Generate streets aRt!
p <- generate_streets(n, width, height, r, delta, p_branch, initial_pts)

# Display plot
p

# Save plot
ggsave(file_name, p, width = 20, height = 20, units = "cm", dpi = 300)