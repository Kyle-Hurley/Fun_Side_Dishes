# Pi Digit Frequency --------------------------------------------------------------------------

# Load necessary library
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
} else if (!requireNamespace("ggthemes", quietly = TRUE)) {
  install.packages("ggthemes")
  library(ggthemes)
} else {
  library(ggplot2)
  library(ggthemes)
}

# Defining URL to download csv (100k digits)
url<-"https://www.angio.net/pi/digits/100000.txt"

# Read file
pi_raw <-readr::read_file(url)

# First 10000 digits of Pi
pi_digits <- as.numeric(strsplit(substr(pi_raw, 3, 10002), "")[[1]])

# Create a data frame
df <- as.data.frame(table(pi_digits))

# Plot
ggplot(df, aes(x = as.factor(pi_digits), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Pi's Digits", x = "Digit", y = "Frequency") +
  theme_wsj()


# Monte Carlo Sim to Estimate Pi --------------------------------------------------------------

# Number of points
n <- 10000

# Generate random points
x <- runif(n, min = -1, max = 1)
y <- runif(n, min = -1, max = 1)
points_df <- data.frame(x = x, y = y)

# Determine if each point is inside the unit circle
points_df$inside_circle <- ifelse(x^2 + y^2 <= 1, 'Inside', 'Outside')

# Estimate Pi
pi_estimate <- 4 * sum(points_df$inside_circle == 'Inside') / n

# Print the estimate
print(paste("Estimated Pi:", pi_estimate))

# Plot using ggplot
ggplot(data = points_df, aes(x = x, y = y, color = inside_circle)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Inside" = "blue", "Outside" = "red")) +
  labs(title = paste("Monte Carlo Simulation to Estimate Pi"),
       subtitle = paste("Estimate:",  round(pi_estimate, 4), "\nNumber of points:", n),
       caption = "Points inside the circle are used to estimate Pi") +
  theme_solarized() + 
  theme(legend.position = "none", 
        axis.title = element_blank()) +
  coord_fixed()  # Ensure the x and y axes are scaled equally


# Pi Music Generator --------------------------------------------------------------------------

# Load necessary library
if (!requireNamespace("tuneR", quietly = TRUE)) {
  install.packages("tuneR")
  library(tuneR)
} else {
  library(tuneR)
}

# Define a simple scale (C major in this example)
notes <- c(261.6, 293.7, 329.7, 349.2, 392.0, 440.0, 493.9, 523.3, 587.4, 659.3)

# Defining URL to download csv (100k digits)
url<-"https://www.angio.net/pi/digits/100000.txt"

# Read file
pi_raw <-readr::read_file(url)

# First 100 digits of Pi
pi_digits <- as.numeric(strsplit(substr(pi_raw, 3, 102), "")[[1]])

# Map digits to notes (0 corresponds to C, 1 to D, etc.)
pi_notes <- notes[pi_digits + 1]

# Generate a list of Wave objects
pi_music <- lapply(
  pi_notes, 
  function(freq) 
    sine(freq, duration = sample(1:5, 1)/10, samp.rate = 8000, xunit = "time")
)

# Combine into a single Wave object
pi_music_combined <- Reduce(bind, pi_music)

# Play the music (make sure your R session has audio support)
play(pi_music_combined)
