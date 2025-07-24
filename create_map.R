library(dplyr)

chr_lengths <- data.frame(
  Chromosome = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                 "11", "12", "13", "14", "15", "16", "17", "18", "19", 
                 "20", "21", "22", "X", "Y"),
  chr_len = c(248956422, 242193529, 198295559, 190214555, 181538259, 
              170805979, 159345973, 145138636, 138394717, 133797422,
              135086622, 133275309, 114364328, 107043718, 101991189,
              90338345, 83257441, 80373285, 58617616, 64444167,
              46709983, 50818468, 156040895, 57227415)
)
chr_map <- chr_lengths %>% 
  mutate(Chr_numeric = as.numeric(factor(Chromosome, levels = c(1:22, "X", "Y")))) %>%
  arrange(Chr_numeric) %>%
  mutate(total = lag(cumsum(as.numeric(chr_len)), default = 0))

# --- Save the new, correct map file ---
saveRDS(chr_map, "chr_map.rds")

print("New chr_map.rds file created successfully.")
