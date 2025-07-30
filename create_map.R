library(dplyr)
library(tibble)


chr_lengths <- data.frame(
  Chromosome = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                 "11", "12", "13", "14", "15", "16", "17", "18", "19", 
                 "20", "21", "22", "X", "Y"),
  Length = c(248956422, 242193529, 198295559, 190214555, 181538259, 
              170805979, 159345973, 145138636, 138394717, 133797422,
              135086622, 133275309, 114364328, 107043718, 101991189,
              90338345, 83257441, 80373285, 58617616, 64444167,
              46709983, 50818468, 156040895, 57227415)
)
chr_lengths$Chromosome <- factor(chr_lengths$Chromosome, levels = c(as.character(1:22), "X", "Y"))
chr_lengths <- chr_lengths[order(chr_lengths$Chromosome), ]

# Create the map with precise, non-overlapping boundaries
chr_map <- chr_lengths %>%
  mutate(cumulative_end = cumsum(as.numeric(Length))) %>%
  mutate(cumulative_start = lag(cumulative_end, default = 0) + 1) %>%
  # Correct the start of the first chromosome to be 0
  mutate(cumulative_start = if_else(Chromosome == "1", 0, cumulative_start))

# The total length is simply the end of the last chromosome
total_genome_length <- max(chr_map$cumulative_end)

# Save the final, correct map
saveRDS(
  list(
    chr_map = chr_map,
    total_genome_length = total_genome_length
  ), 
  file = "chr_map_data.rds"
)

print("Final, precise chr_map_data.rds file created successfully. ✅")
