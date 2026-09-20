library(readxl)
library(tidyverse)
library(cowplot)
library(patchwork)

claude <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Claude One.xlsx")


kathryn <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Kathryn Updated.xlsx")
maya <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Maya.xlsx")
lauren <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Lauren.xlsx")


#Function for counting features by session for each rater
count_features_by_session <- function(df){
  
  #Extract column names except feature column
  all_cols <- names(df)[names(df) != "Feature"]
  
  #Get feature type 
  prt_features <- df$Feature[grep("^PRT-", df$Feature)]
  cbt_features <- df$Feature[grep("^CBT-", df$Feature)]
  
  #sort columns by session type 
  prt_cols <- all_cols[sapply(all_cols, function(col){
    parts <- strsplit(col, "-")[[1]]
    return(parts[3] == "PRT")
  })]
  
  cbt_cols <- all_cols[sapply(all_cols, function(col){
    parts <- strsplit(col, "-")[[1]]
    return(parts[3]=="CBT")
  })]
  
  #Initialize counters
  prt_in_prt <- 0
  prt_in_cbt <- 0
  cbt_in_prt <- 0
  cbt_in_cbt <- 0
  
  num_prt_sessions <- length(prt_cols)
  num_cbt_sessions <- length(cbt_cols)
  
  #Count PRT features in PRT sessions 
  for(feature in prt_features){
    for (col in prt_cols){
      feature_val <- df[df$Feature == feature, col]
      if(!is.na(feature_val) && feature_val == 1){
        prt_in_prt <- prt_in_prt + 1
      }
    }
  }
  #Count PRT features in CBT sessions 
  for(feature in prt_features){
    for(col in cbt_cols){
      feature_val <- df[df$Feature == feature, col]
      if(!is.na(feature_val) && feature_val == 1){
        prt_in_cbt <- prt_in_cbt + 1
      }
    }
  }
  for(feature in cbt_features){
    for(col in prt_cols){
      feature_val <- df[df$Feature == feature, col]
      if(!is.na(feature_val) && feature_val ==1){
        cbt_in_prt <- cbt_in_prt + 1
      }
    }
  }
  for(feature in cbt_features){
    for(col in cbt_cols){
      feature_val <- df[df$Feature == feature, col]
      if(!is.na(feature_val) && feature_val ==1){
        cbt_in_cbt <- cbt_in_cbt + 1
      }
    }
  }
  total_prt_in_prt <- length(prt_features) * length(prt_cols)
  total_prt_in_cbt <- length(prt_features) * length(cbt_cols)
  total_cbt_in_prt <- length(cbt_features) * length(prt_cols)
  total_cbt_in_cbt <- length(cbt_features) * length(cbt_cols)
  
  avg_prt_in_prt <- ifelse(num_prt_sessions > 0, prt_in_prt / num_prt_sessions, 0)
  avg_prt_in_cbt <- ifelse(num_cbt_sessions > 0, prt_in_cbt / num_cbt_sessions, 0)
  avg_cbt_in_prt <- ifelse(num_prt_sessions > 0, cbt_in_prt / num_prt_sessions, 0)
  avg_cbt_in_cbt <- ifelse(num_cbt_sessions > 0, cbt_in_cbt / num_cbt_sessions, 0)
  
  #Create matrices  (fixed: column-major fill now matches dimnames)
  counts_matrix <- matrix(
    c(prt_in_prt, cbt_in_prt, prt_in_cbt, cbt_in_cbt),
    nrow = 2,
    dimnames = list(
      c("PRT Features", "CBT Features"),
      c("PRT Sessions", "CBT Sessions")
    )
  )
  totals_matrix <- matrix(
    c(total_prt_in_prt, total_cbt_in_prt, total_prt_in_cbt, total_cbt_in_cbt),
    nrow = 2,
    dimnames = list(
      c("PRT Features", "CBT Features"),
      c("PRT Sessions", "CBT Sessions")
    )
  )
  # Create the average features per session matrix
  avg_per_session_matrix <- matrix(
    c(avg_prt_in_prt, avg_cbt_in_prt, avg_prt_in_cbt, avg_cbt_in_cbt),
    nrow = 2,
    dimnames = list(
      c("PRT Features", "CBT Features"),
      c("PRT Sessions", "CBT Sessions")
    )
  )
  
  percentage_matrix <- matrix(
    c(
      ifelse(total_prt_in_prt > 0, prt_in_prt / total_prt_in_prt * 100, 0),
      ifelse(total_cbt_in_prt > 0, cbt_in_prt / total_cbt_in_prt * 100, 0),
      ifelse(total_prt_in_cbt > 0, prt_in_cbt / total_prt_in_cbt * 100, 0),
      ifelse(total_cbt_in_cbt > 0, cbt_in_cbt / total_cbt_in_cbt * 100, 0)
    ),
    nrow = 2, 
    dimnames = list(
      c("PRT Features", "CBT Features"),
      c("PRT Sessions", "CBT Sessions")
    )
  )
  return(list(
    counts = counts_matrix, 
    totals = totals_matrix,
    per_session = avg_per_session_matrix,
    percentages = percentage_matrix
  ))
}


count_features_by_session(kathryn)
count_features_by_session(claude)
count_features_by_session(maya)
count_features_by_session(lauren)




library(ggplot2)
library(patchwork)  

#Build matrices
kathryn_features_per_session <- matrix(
  c(6.85, 0.00,
    0.05, 5.43),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("PRT Features", "CBT Features"),
                  c("PRT Sessions", "CBT Sessions")))

maya_features_per_session <- matrix(
  c(5.40, 0.36,
    0.10, 5.54),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("PRT Features", "CBT Features"),
                  c("PRT Sessions", "CBT Sessions")))

lauren_features_per_session <- matrix(
  c(4.90, 0.36,
    0.25, 5.61),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("PRT Features", "CBT Features"),
                  c("PRT Sessions", "CBT Sessions")))

claude_features_per_session <- matrix(
  c(3.20, 1.00,
    1.00, 5.75),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("PRT Features", "CBT Features"),
                  c("PRT Sessions", "CBT Sessions")))

avg_features_per_session <- matrix(
  c(5.08, 0.43,
    0.35, 5.57),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("PRT Features", "CBT Features"),
                  c("PRT Sessions", "CBT Sessions")))


#Function for building heatmaps
all_values    <- c(kathryn_features_per_session, maya_features_per_session,
                   lauren_features_per_session, claude_features_per_session,
                   avg_features_per_session)
shared_limits <- range(all_values)

 
create_heatmap <- function(matrix_data, subtitle, limits,
                           show_axis_text = FALSE) {
  
  df <- as.data.frame(as.table(matrix_data))
  names(df) <- c("Feature_Type", "Session_Type", "Value")
  df$Feature_Type <- factor(df$Feature_Type,
                            levels = rev(c("PRT Features", "CBT Features")))
  df$Session_Type <- factor(df$Session_Type,
                            levels = c("PRT Sessions", "CBT Sessions"))
  
  ggplot(df, aes(x = Session_Type, y = Feature_Type, fill = Value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Value)), color = "black", size = 3) +
    scale_fill_gradient(low = "#FFEDD8", high = "#D95F0E",
                        limits = limits, name = "Avg\nFeatures") +
    labs(subtitle = subtitle, x = "", y = "") +
    scale_x_discrete(labels = c("PRT Sessions" = "PRT", "CBT Sessions" = "CBT")) +
    theme_minimal() +
    theme(
      plot.subtitle   = element_text(hjust = 0.5, face = "bold", size = 9),
      axis.text.x     = if (show_axis_text) element_text(size = 8) else element_blank(),
      axis.text.y     = if (show_axis_text) element_text(size = 8) else element_blank(),
      legend.title    = element_text(face = "bold", size = 8),
      legend.text     = element_text(size = 8),
      panel.grid      = element_blank(),
      plot.margin     = margin(4, 4, 4, 4),
      aspect.ratio    = 1
    )
}

#Put each panel on the same scale
p_kathryn <- create_heatmap(kathryn_features_per_session, "Rater 1", shared_limits)
p_maya    <- create_heatmap(maya_features_per_session,    "Rater 2", shared_limits)
p_lauren  <- create_heatmap(lauren_features_per_session,  "Rater 3", shared_limits)
p_claude  <- create_heatmap(claude_features_per_session,  "Claude",  shared_limits)
p_avg     <- create_heatmap(avg_features_per_session, "Across All Raters",
                            shared_limits, show_axis_text = TRUE)

#Combine heat maps
combined_heatmaps <- (p_kathryn / p_maya / p_lauren / p_claude / p_avg) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Average Number of Features Per Session",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  ) &
  theme(legend.position = "right")

print(combined_heatmaps)

ggsave("combined_heatmaps.png", combined_heatmaps, width = 4, height = 14, dpi = 300)