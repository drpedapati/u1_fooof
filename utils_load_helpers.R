# Create mapping function using vectorization instead of sapply
map_roi_to_atlas <- function(roi_vector) {
  # Split the vector once
  parts <- str_split(roi_vector, "-", simplify = TRUE)
  region <- parts[, 1]
  hemisphere <- parts[, 2]
  
  # Convert hemisphere format (lh -> L, rh -> R)
  hemisphere_formatted <- ifelse(hemisphere == "lh", "L", "R")
  
  # Create the matching atlas format
  paste(region, hemisphere_formatted)
}
