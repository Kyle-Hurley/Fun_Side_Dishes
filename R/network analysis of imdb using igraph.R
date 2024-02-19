
# Load Libraries ----------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(igraph)
library(ggplot2)
library(gridExtra)
library(tidyr)


# Download IMDB Data ------------------------------------------------------

# Define URLs of the datasets
urls <- c(name_basics = "https://datasets.imdbws.com/name.basics.tsv.gz",
          title_basics = "https://datasets.imdbws.com/title.basics.tsv.gz", 
          title_principals = "https://datasets.imdbws.com/title.principals.tsv.gz")

# Function to download and read a dataset
read_dataset <- function(url) {
  
  # Create temp file for dataset on local disk
  dest_file <- tempfile()
  
  # Download the dataset
  download.file(url, destfile = dest_file, mode = "wb")
  
  # Read the dataset
  data <- read_tsv(gzfile(dest_file))
  
  # Delete temporary file from local disk
  unlink(dest_file)
  
  return(data)
}

# Iterate over URLs and download datasets
df <- map(urls, read_dataset)
rm(urls, read_dataset)

# Clean up datasets
df$name_basics <- df$name_basics %>% 
  select(nconst, primaryName)
df$title_basics <- df$title_basics %>% 
  filter(!titleType %in% c("short", "videoGame")) %>% 
  select(tconst, primaryTitle, startYear)
df$title_principals <- df$title_principals %>% 
  select(tconst, nconst, category) %>% 
  filter(category %in% c("actor", "actress")) %>% 
  select(tconst, nconst)

# Join
df <- inner_join(df$title_basics, df$title_principals, by = "tconst") %>% 
  inner_join(df$name_basics, by = "nconst") %>% 
  relocate(nconst, tconst, primaryName, primaryTitle, startYear)

# Convert startYear to numeric
df <- df %>% 
  filter(startYear != "\\N") %>% 
  mutate(startYear = as.numeric(startYear))



# 1st Degree Connections Over Time ----------------------------------------


# Get Kevin Bacon's unique nconst
kb_nconst <- df %>% 
  filter(primaryName == "Kevin Bacon") %>% 
  distinct(nconst, primaryName) %>% 
  slice(1) %>% 
  pull(nconst)

# Get others for comparison
of_interest <- c("Sydney Sweeney", "Matthew Broderick" , "Viola Davis", "Patrick Stewart")
of_interest <- df %>% 
  select(nconst, primaryName) %>% 
  filter(primaryName %in% of_interest) %>% 
  distinct()

# Add Kevin
of_interest <- of_interest %>% 
  add_row(nconst = kb_nconst, primaryName = "Kevin Bacon")

# Create vector of release years
data_years <- unique(df$startYear)[unique(df$startYear) != "\\N"]
data_years <- data_years[order(data_years)]

# Define earliest start year
st_yr <- df %>% 
  filter(primaryName == "Patrick Stewart", 
         startYear != "\\N") %>% 
  filter(startYear == min(as.numeric(startYear))) %>% 
  pull(startYear)

# Finalize release years vector
data_years <- data_years[data_years >= st_yr & data_years <= 2023]


# Initialize data frame to hold degrees over time data
actor_degrees <- data.frame(Name = of_interest$primaryName, 
                            nconst = of_interest$nconst)

# Get Degrees for each release year for each actor of interest
for (yr in data_years) {
  
  yr <- as.numeric(yr)
  
  temp_graph <- df %>% 
    filter(startYear <= yr) %>% 
    igraph::graph_from_data_frame(directed = FALSE)
  
  actor_degrees[, paste0("yr_", eval(yr))] <- rep(NA, nrow(of_interest))
  
  const_vec <- V(temp_graph)$name[which(V(temp_graph)$name %in% of_interest$nconst)]
  
  for (const in const_vec) {
    
    vertex_id <- which(V(temp_graph)$name == const)
    
    actor_degrees[actor_degrees$nconst == const, paste0("yr_", eval(yr))] <- degree(temp_graph)[vertex_id]
    
  }
}

rm(temp_graph, yr, vertex_id, st_yr, data_years, const, const_vec, actor_vertex_ids)

# Pivot longer
hold <- actor_degrees %>% 
  select(!nconst) %>% 
  select(Name, yr_1966:yr_2023) %>% 
  pivot_longer(cols = starts_with("yr_"), 
               names_to = "Year", 
               names_prefix = "yr_", 
               values_to = "Degrees") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Degrees = ifelse(is.na(Degrees), 0, Degrees)) %>% 
  filter(Degrees > 0)

# Plot!
ggplot(data = hold, aes(x = Year, y = Degrees, color = Name, group = Name)) + 
  geom_line(size = 2) + 
  geom_point(size = 3, shape = 21, fill = "white") + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "1st Degree Connections Over Time", 
       ylab = "") + 
  annotate("text", x = 1980, y = 250, 
           label = "Sharp increases\nindicate high demand!", 
           size = 16/.pt) + 
  geom_curve(aes(x = 1980, y = 220, xend = 1990, yend = 150), 
             arrow = arrow(length = unit(0.03, "npc")), 
             curvature = 0.3, size = 1.25, color = "black") + 
  theme(
    panel.grid.major = element_line(color = "#e1e1e1"), 
    panel.background = element_blank(), 
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    legend.position = "bottom", 
    legend.box = "horizontal",
    legend.key = element_rect(fill = NA), 
    legend.text = element_text(size = 14), 
    legend.title = element_blank(), 
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, angle = 45, hjust = 1),
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank() 
  )

ggsave(filename = file.path(getwd(), "1st Degree Connections Over Time.png"), 
       plot = last_plot())


# Number of Connections in the 1st Degree -----------------------------------


# Create graph of vertices and edges
graph <- graph_from_data_frame(d = df, directed = FALSE)

# Get Kevin's vertex in graph
kb_vertex <- which(V(graph)$name == kb_nconst)

# What percentile is Kevin Bacon for number of 1st degree connections??
all_degrees <- degree(graph)
kb_degree <- all_degrees[kb_vertex]
all_degrees <- sort(all_degrees)
kb_percentile <- (match(kb_degree, all_degrees) / length(all_degrees)) * 100
degrees <- as.data.frame(table(degree(graph, mode = "all")))
degrees$Var1 <- as.numeric(degrees$Var1)

# For comparison
actor_vertex_ids <- which(V(graph)$name %in% of_interest$nconst)
actor_degrees <- degree(graph)[actor_vertex_ids]
actor_degrees <- actor_degrees[match(of_interest$nconst, names(actor_degrees))]
names(actor_degrees) <- of_interest$primaryName
actor_degrees <- data.frame(Name = names(actor_degrees), 
                            "1st Degree Connections" = as.vector(actor_degrees), 
                            "Percentile" = round(
                              (match(actor_degrees, all_degrees) / length(all_degrees)) * 100, 
                              digits = 2
                            ),
                            check.names = FALSE)
actor_degrees <- actor_degrees[order(actor_degrees$`1st Degree Connections`, decreasing = TRUE), ]

ggplot(degrees, aes(x = Var1, y = Freq)) + 
  geom_col(fill = "#69b3a2") + 
  xlim(0, 20) + 
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000), 
                labels = c("0.5", "1.0", "1.5", "2.0")) + 
  ylab("Number of People (million)") + 
  xlab("Number of Connections") + 
  annotate("text", 
           label = paste0(
             "Most TV/Movie performers only have \n1 to 4 1st degree connections!"
           ), 
           x = 9, y = 900000, size = 16/.pt) + 
  geom_curve(aes(x = 8, y = 800000, xend = 5, yend = 500000), 
             arrow = arrow(length = unit(0.03, "npc")), 
             curvature = -0.2, size = 1.25) + 
  annotation_custom(tableGrob(actor_degrees, 
                              rows = NULL, 
                              cols = c(
                                "", 
                                "1st Degree\nConnections", 
                                "Percentile"), 
                              theme = ttheme_minimal()), 
                    xmin = 14, xmax = 19, ymin = 1000000, ymax = 1500000) + 
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 14, vjust = -0.2), 
    axis.title.y = element_text(size = 14, vjust = 1.2), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14, angle = 45, hjust = 1), 
    axis.ticks = element_blank(), 
    legend.position = "none", 
    panel.background = element_blank(), 
    panel.grid.major.y = element_line(color = "#e1e1e1"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  )

ggsave(filename = file.path(getwd(), "People by Number of Connections in 1st Degree.png"), 
       plot = last_plot())

rm(degrees, actor_degrees, actor_vertex_ids, kb_percentile, kb_degree, all_degrees)


# Compare 6 Degrees of Separation -----------------------------------------


ss_vertex <- which(V(graph)$name == of_interest[of_interest$primaryName == "Sydney Sweeney", ]$nconst)
ps_vertex <- which(V(graph)$name == of_interest[of_interest$primaryName == "Patrick Stewart", ]$nconst)
# Calculate the shortest paths from Viola Davis to all other nodes
dist_to_vd <- distances(graph = graph, 
                        v = ss_vertex, 
                        # v = of_interest[of_interest$primaryName == "Viola Davis", ]$nconst, 
                        to = V(graph), mode = "all")
dist_to_ps <- distances(graph = graph, 
                        v = ps_vertex, 
                        to = V(graph), mode = "all")

# Prepare data for both actors
vd_path_freq <- as.data.frame(table(PathLength = as.numeric(dist_to_vd[-ss_vertex]) - 1))
vd_path_freq$Actor <- "Sydney Sweeney"

ps_path_freq <- as.data.frame(table(PathLength = as.numeric(dist_to_ps[-ps_vertex]) - 1))
ps_path_freq$Actor <- "Patrick Stewart"

# Combine data for both actors
combined_path_freq <- rbind(vd_path_freq, ps_path_freq)

ggplot(combined_path_freq, aes(x = PathLength, y = Freq, fill = Actor)) +
  geom_col(position = position_dodge(0.8), width = 0.75) + 
  scale_fill_manual(values = c("Sydney Sweeney" = "#984EA3", "Patrick Stewart" = "#4DAF4A")) + 
  geom_text(aes(label = scales::comma(Freq)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, size = 10/.pt) + 
  xlim("0", "1", "2", "3", "4", "5", "6") + 
  labs(x = "Degrees From", 
       y = "Number of People (Log Scale)") + 
  scale_y_log10(breaks = c(100, 10000, 1000000), 
                labels = c("100", "10,000", "1,000,000")) + 
  annotate("text", x = 2, y = 300000, 
           label = "Even newer performers can quickly\nbecome central in the TV/Movie network", 
           size = 16/.pt) + 
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, vjust = -0.2),
    axis.title.y = element_text(size = 14, vjust = 1.2),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, angle = 45, hjust = 1), 
    axis.ticks = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 14), 
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "#e1e1e1"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  )

ggsave(filename = file.path(getwd(), "Sydney Sweeney and Patrick Stewart Degrees From Histogram.png"), 
       plot = last_plot())
