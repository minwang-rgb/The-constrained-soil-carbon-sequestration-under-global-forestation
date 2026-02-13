# Load required libraries
requiredPackages <- c('car', 'ggplot2', 'tidyr',
                      'parallel', 'lubridate', 'corrplot', 'rlang',
                      'future.apply', 'lightgbm', 'dplyr', 'caret',
                      'SHAPforxgboost', 'xgboost', 'data.table',
                      'pdp', 'doParallel', 'foreach')

for (p in requiredPackages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Load dataset

setwd("F:/model")
df0 <- read.csv("20260108dataset.csv") %>% as.data.frame()


V1 <- "passive" #alternatively replace with 'passive', 'SOC',"active"
df0 <- df0 %>%
  filter(Var1 == V1) %>%
  dplyr::select(-Var1)
colnames(df0)[1] <- "yi"

# Rename columns
df <- plyr::rename(df0,
                   c("Longitude"  = "Lon",
                     "Latitude" = "Lat",
                     "Soil.layer" = "Soillayer",
                     "Agenumber" = "Age",
                     "Recovery.mode" = "Recovmode",
                     "Vege.model" = "Vegetype",
                     "Landuse"= "LUtype"
                     
                   ))

target_var <- "yi"

# Convert categorical variables to numeric
char2num <- function(data) {
  factor(data) %>% as.numeric()
}



df$Soillayer <- as.numeric(as.character(df$Soillayer))
df$LUtype <- as.numeric(as.character(df$LUtype))
df$Recovmode <- as.numeric(as.character(df$Recovmode))
df$Vegetype <- as.numeric(as.character(df$Vegetype))
df$Altitude <- as.numeric(as.character(df$Altitude))
str(df)

# Convert ALL columns to numeric (including those already numeric for consistency)
df <- df %>%
  mutate(across(everything(), ~ {
    # First convert to character to handle any factor variables
    x <- as.character(.)
    
    # Then convert to numeric, suppressing warnings about NAs
    suppressWarnings(as.numeric(x))
  }))

# For the bio* variables that were integers, ensure they become proper numeric
bio_cols <- grep("^bio", names(df), value = TRUE)
df[bio_cols] <- lapply(df[bio_cols], as.numeric)

# Verify the conversion
str(df)  # Should show all columns as numeric now



# Remove columns with >1% missing values
df <- df[, sapply(df, function(x) mean(is.na(x)) <= 0.1)]
df <- df %>% filter(if_all(everything(), is.finite))

# Remove high VIF variables (Multicollinearity Check)
remove_high_vif <- function(data, response_var, threshold = 5) {
  removed_vars <- c()
  repeat {
    formula <- as.formula(paste(response_var, "~ ."))
    lm_model <- lm(formula, data = data)
    vif_values <- vif(lm_model)
    max_vif <- max(vif_values)
    
    if (max_vif < threshold) break
    
    remove_var <- names(which.max(vif_values))
    removed_vars <- c(removed_vars, remove_var)
    data <- data[, !names(data) %in% remove_var]
  }
  return(list(data = data, removed_vars = removed_vars))
}

# Identify and remove collinear variables
lin_combos <- findLinearCombos(df[, -which(names(df) == "yi")])
if (length(lin_combos$remove) > 0) {
  df <- df[, -lin_combos$remove]
}

# Apply VIF-based feature selection
result <- remove_high_vif(df, response_var = "yi")
data.predictor.VIF <- setdiff(names(result$data), "yi")
df.clean <- result$data
#save(df.clean, file = 'df.clean.yi.Rda')
write.csv(df.clean, file = "df.clean.yi.csv", row.names = FALSE)  # Save as CSV
#save(data.predictor.VIF, file = 'data.predictor.VIF.yi.Rda')
write.csv(data.predictor.VIF, file = "data.predictor.VIF.yi.csv", row.names = FALSE)  # Save as CSV










