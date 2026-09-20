library(readxl)
library(tidyverse)
library(patchwork)


kathryn <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Kathryn Updated.xlsx")
maya <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Maya.xlsx")
lauren <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Lauren.xlsx")
claude <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Claude One.xlsx")
claude_sonnet <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/LLM Assignment Claude Sonnet.xlsx")
claude_shot <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/Claude Fidelity Assessment One Shot no CoT.xlsx")
claude_cot <- read_excel("/Users/zanwynia/Dropbox/2024_LLM_Project/Fidelity Assessments/Claude Fidelity Assessment CoT.xlsx")


###-------------------------------------------------------------------------------------------------------------
#Changing hyphens to underscores in all datasets to make processing easier

names(kathryn) <- gsub("-", "_", names(kathryn))
names(maya) <- gsub("-", "_", names(maya))
names(lauren) <- gsub("-", "_", names(lauren))
names(claude) <- gsub("-", "_", names(claude))
names(claude_sonnet) <- gsub("-", "_", names(claude_sonnet))
names(claude_shot) <- gsub("-", "_", names(claude_shot))
names(claude_cot) <- gsub("-", "_", names(claude_cot))

kathryn$Feature <- gsub("-", "_", kathryn$Feature)
maya$Feature <- gsub("-", "_", maya$Feature)
lauren$Feature <- gsub("-", "_", lauren$Feature)
claude$Feature <- gsub("-", "_", claude$Feature)
claude_sonnet$Feature <- gsub("-", "_", claude_sonnet$Feature)
claude_shot$Feature <- gsub("-", "_", claude_shot$Feature)
claude_cot$Feature <- gsub("-", "_", claude_cot$Feature)

###-------------------------------------------------------------------------------------------------------------
#Function to convert column names so that they start with the session info (e.g. S6) Rater that subject ID.
#Having column names start with numbers would require further pre-processing that I'm hoping to avoid

convert_col_name <- function(old_name){
  #Split column names by underscore
  parts <- strsplit(old_name, "_")[[1]]
  
  #Identify the three pieces of information in each column name (subject ID, session number, & therapy type)
  if(length(parts) == 3) {
    subject_id  <- parts[1]
    session_num <- parts[2]
    therapy_type <- parts[3]
    
    #Reformat the column name so session number comes first
    new_name <- paste(session_num, subject_id, therapy_type, sep="_")
    return(new_name)
  }
  
}

names(kathryn) <- sapply(names(kathryn), convert_col_name)
names(lauren) <- sapply(names(lauren), convert_col_name)
names(maya) <- sapply(names(maya), convert_col_name)
names(claude) <- sapply(names(claude), convert_col_name)
names(claude_sonnet) <- sapply(names(claude_sonnet), convert_col_name)
names(claude_shot) <- sapply(names(claude_shot), convert_col_name)
names(claude_cot) <- sapply(names(claude_cot), convert_col_name)

#Changed the features column to NULL based on the function, renaming it back to feature
names(kathryn)[names(kathryn) == "NULL"] <- "feature"
names(lauren)[names(lauren) == "NULL"] <- "feature"
names(maya)[names(maya) == "NULL"] <- "feature"
names(claude)[names(claude) == "NULL"] <- "feature"
names(claude_sonnet)[names(claude_sonnet) == "NULL"] <- "feature"
names(claude_shot)[names(claude_shot) == "NULL"] <- "feature"
names(claude_cot)[names(claude_cot) == "NULL"] <- "feature"

###RUN CODE TO HERE FIRST AND THEN STOP BEFORE RUNNING CONFUSION MATRIX/HEAT MAP CODE 


###-------------------------------------------------------------------------------------------------------------
#Creating a function to identify the session type (CBT or PRT) and session number

session_info <- function(session_name){
  #Split the string of the session names to extract pertinent info
  parts <- strsplit(session_name, "_")[[1]]
  
  #Extracting the following components 
  #[1] = session number
  #[2] = subject id 
  #[3] = therapy type 
  
  number <- parts[1]
  subject <- parts[2]
  type <- parts[3]
  
  return(list(
    number = number,
    subject = subject,
    type = type
  ))
}



###-------------------------------------------------------------------------------------------------------------
#Creating a function to create confusion matrix and calculate kappa statistic 

confusion_matrix <- function(df1, df2, feature, relevant_cols) {
  # Creating vectors to store values from raters
  rater1_value <- unlist(df1[df1$feature == feature, relevant_cols])
  rater2_value <- unlist(df2[df2$feature == feature, relevant_cols])
  
  # Create a confusion matrix
  conf_matrix <- table(factor(rater1_value, levels = c(0,1)),
                       factor(rater2_value, levels = c(0,1)))
  
  # Calculate accuracy (i.e. observed agreement)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Calculate regular kappa
  obsv_agree <- accuracy
  expected_agree <- sum(rowSums(conf_matrix) * colSums(conf_matrix)) / sum(conf_matrix)^2
  
  if(is.na(expected_agree)) {
    kappa <- NA
    pabak <- NA
  } else if(expected_agree == 1) {
    kappa <- NA
    pabak <- (2*accuracy) - 1
  } else {
    kappa <- (obsv_agree - expected_agree)/(1 - expected_agree)
    
    # Calculate PABAK
    # PABAK = (2 * accuracy) - 1
    pabak <- (2 * accuracy) - 1
  }
  
  # Calculate prevalence index
  n <- sum(conf_matrix)
  prevalence_index <- abs((conf_matrix[1,1] - conf_matrix[2,2])) / n
  
  # Calculate bias index
  bias_index <- abs((conf_matrix[1,2] - conf_matrix[2,1])) / n
  
  return(list(
    table = conf_matrix,
    accuracy = accuracy,
    kappa = kappa,
    pabak = pabak,
    prevalence_index = prevalence_index,
    bias_index = bias_index,
    expected_agree = expected_agree,
    n_sessions = length(relevant_cols)
  ))
}


###-------------------------------------------------------------------------------------------------------------
#Function to analyze PRT features 

analyze_prt_features <- function(df1, df2){
  
  
  #getting all column names to isolate the PRT sessions 
  all_cols <- names(df1)[names(df1) != "feature"]
  
  #Subsetting PRT specific sessions
  prt_cols <- all_cols[sapply(all_cols, function(col) {
    info <- session_info(col)
    return(info$type == "PRT")
  
    
      
  })]
  
  
  #Isolate PRT features
  
  prt_features <- df1$feature[grep("^PRT_", df1$feature)]

  
  results <- list()
  
  #For all PRT features analyze across ALL PRT sessions
  
  for(feature in prt_features){
    results[[feature]] <- confusion_matrix(df1, df2, feature, prt_cols)
  }
  
  return(results)

}

###-------------------------------------------------------------------------------------------------------------
# Function to analyze specific CBT sessions
analyze_specific_cbt_features <- function(df1, df2, sessions = c("S5", "S6", "S8")) {
  # Get all column names except feature
  all_cols <- names(df1)[names(df1) != "feature"]
  
  # Get all CBT features for specified sessions
  base_cbt_features <- df1$feature[grep(paste0("^CBT_(", paste(gsub("S", "", sessions), collapse="|"), ")"), df1$feature)]
  
  # Add only PRT_CBT as an additional feature
  additional_features <- "PRT_CBT"
  additional_features_present <- additional_features[additional_features %in% df1$feature]
  
  # Combine all CBT features to analyze
  cbt_features <- c(base_cbt_features, additional_features_present)
  
  results <- list()
  
  for(feature in cbt_features) {
    if(feature == "PRT_CBT") {
      # PRT_CBT should be compared with CBT columns from the specified sessions
      relevant_cols <- all_cols[sapply(all_cols, function(col){
        info <- session_info(col)
        return(info$type == "CBT" && info$number %in% sessions)
      })]
    } else {
      # For regular CBT features, extract the session number
      feature_session_num <- strsplit(feature, "_")[[1]][2]
      feature_session_num <- paste0("S", feature_session_num)
      
      # Only process if feature is from specified sessions
      if(feature_session_num %in% sessions) {
        relevant_cols <- all_cols[sapply(all_cols, function(col){
          info <- session_info(col)
          return(info$type == "CBT" && info$number == feature_session_num)
        })]
      }
    }
    
    if(exists("relevant_cols") && length(relevant_cols) > 0) {
      results[[feature]] <- confusion_matrix(df1, df2, feature, relevant_cols)
    }
    
    # Reset relevant_cols for next iteration
    if(exists("relevant_cols")) {
      rm(relevant_cols, envir = environment())
    }
  }
  
  return(results)
}

analyze_cbt_features <- function(df1, df2, sessions = c("S5", "S6", "S8")) {
  # Get all column names except feature
  all_cols <- names(df1)[names(df1) != "feature"]
  
  # Get all CBT features for specified sessions
  base_cbt_features <- df1$feature[grep(paste0("^CBT_(", paste(gsub("S", "", sessions), collapse="|"), ")"), df1$feature)]
  
  # Get CBT-All features
  cbt_all_features <- df1$feature[grep("^CBT_All", df1$feature)]
  
  # Add PRT_CBT as an additional feature
  additional_features <- "PRT_CBT"
  additional_features_present <- additional_features[additional_features %in% df1$feature]
  
  # Combine all CBT features to analyze
  cbt_features <- c(base_cbt_features, cbt_all_features, additional_features_present)
  
  results <- list()
  
  for(feature in cbt_features) {
    if(feature == "PRT_CBT") {
      # PRT_CBT should be compared with CBT columns from the specified sessions
      relevant_cols <- all_cols[sapply(all_cols, function(col){
        info <- session_info(col)
        return(info$type == "CBT" && info$number %in% sessions)
      })]
    } else if(grepl("^CBT_All", feature)) {
      # For CBT-All features, use all specified CBT sessions
      relevant_cols <- all_cols[sapply(all_cols, function(col){
        info <- session_info(col)
        return(info$type == "CBT" && info$number %in% sessions)
      })]
    } else {
      # For regular CBT features, extract the session number
      feature_session_num <- strsplit(feature, "_")[[1]][2]
      feature_session_num <- paste0("S", feature_session_num)
      
      # Only process if feature is from specified sessions
      if(feature_session_num %in% sessions) {
        relevant_cols <- all_cols[sapply(all_cols, function(col){
          info <- session_info(col)
          return(info$type == "CBT" && info$number == feature_session_num)
        })]
      }
    }
    
    if(exists("relevant_cols") && length(relevant_cols) > 0) {
      results[[feature]] <- confusion_matrix(df1, df2, feature, relevant_cols)
    }
    
    # Reset relevant_cols for next iteration
    if(exists("relevant_cols")) {
      rm(relevant_cols, envir = environment())
    }
  }
  
  return(results)
}

###-------------------------------------------------------------------------------------------------------------
#For the CBT all features 

analyze_cbt_all_features <- function(df1, df2, sessions = c("S5", "S6", "S8")) {
  # Get all column names except feature
  all_cols <- names(df1)[names(df1) != "feature"]
  
  # Subset only specific CBT sessions
  cbt_cols <- all_cols[sapply(all_cols, function(col) {
    info <- session_info(col)
    return(info$type == "CBT" && info$number %in% sessions)
  })]
  
  # Identify the CBT-All features
  cbt_all_features <- df1$feature[grep("^CBT_All", df1$feature)]
  
  results <- list()
  
  # For all CBT-All features analyze across specified CBT sessions only
  for(feature in cbt_all_features) {
    results[[feature]] <- confusion_matrix(df1, df2, feature, cbt_cols)
  }
  
  return(results)
}

###-------------------------------------------------------------------------------------------------------------
#For the control features 
analyze_control_features <- function(df1, df2, cbt_sessions = c("S5", "S6", "S8")) {
  # Get all column names except feature
  all_cols <- names(df1)[names(df1) != "feature"]
  
  # Subset specific CBT sessions and all PRT sessions
  relevant_cols <- all_cols[sapply(all_cols, function(col) {
    info <- session_info(col)
    return((info$type == "CBT" && info$number %in% cbt_sessions) || 
             (info$type == "PRT"))
  })]
  
  # Define the exact control features we want to analyze
  control_features <- c("PosCont1", "PosCont2", "NegCont1", "NegCont2")
  
  # Check which control features actually exist in the dataframe
  control_features_present <- control_features[control_features %in% df1$feature]
  
  # Print detected control features for debugging
  cat("Control features found in data:", paste(control_features_present, collapse=", "), "\n")
  
  results <- list()
  
  # For each control feature, analyze across specified CBT sessions and all PRT sessions
  for(feature in control_features_present) {
    results[[feature]] <- confusion_matrix(df1, df2, feature, relevant_cols)
  }
  
  return(results)
}
###-------------------------------------------------------------------------------------------------------------
# Function to calculate average PABAK and Kappa from analysis results
calculate_average_metrics <- function(results) {
  # Extract all PABAK and Kappa values
  pabak_values <- sapply(results, function(x) x$pabak)
  kappa_values <- sapply(results, function(x) x$kappa)
  bias_values <- sapply(results, function(x) x$bias_index)
  prevalence_values <- sapply(results, function(x) x$prevalence_index)
  
  #Find features that have both PABAK and Kappa values
  valid_features <- names(pabak_values)[!is.na(pabak_values) & !is.na(kappa_values)]
  
  #Filter all vectors using valid features
  pabak_values <- pabak_values[valid_features]
  kappa_values <- kappa_values[valid_features]
  bias_values <- bias_values[valid_features]
  prevalence_values <- prevalence_values[valid_features]
  
  # Calculate statistics using filtered values
  mean_pabak <- mean(pabak_values)
  mean_kappa <- mean(kappa_values)
  mean_bias <- mean(bias_values)
  mean_prevalence <- mean(prevalence_values)
  sd_pabak <- sd(pabak_values)
  sd_kappa <- sd(kappa_values)
  median_pabak <- median(pabak_values)
  median_kappa <- median(kappa_values)
  
  # Create summary of values for each feature
  feature_summary <- data.frame(
    Feature = valid_features,
    PABAK = pabak_values,
    Kappa = kappa_values,
    Bias = bias_values,
    Prevalence = prevalence_values
  )
  
  return(list(
    mean_pabak = mean_pabak,
    mean_kappa = mean_kappa,
    mean_bias = mean_bias,
    mean_prevalence = mean_prevalence,
    sd_pabak = sd_pabak,
    sd_kappa = sd_kappa,
    median_pabak = median_pabak,
    median_kappa = median_kappa,
    feature_summary = feature_summary,
    n_features = length(valid_features)
  ))
}

calculate_average_PABAKmetrics <- function(results) {
  # Extract all PABAK values
  pabak_values <- sapply(results, function(x) x$pabak)
  bias_values <- sapply(results, function(x) x$bias_index)
  prevalence_values <- sapply(results, function(x) x$prevalence_index)
  
  # Remove any NA values
  valid_features <- names(pabak_values)[!is.na(pabak_values)]
  pabak_values <- pabak_values[valid_features]
  bias_values <- bias_values[valid_features]
  prevalence_values <- prevalence_values[valid_features]
  
  # Create summary of values for each feature
  feature_summary <- data.frame(
    Feature = valid_features,
    PABAK = pabak_values,
    Bias = bias_values,
    Prevalence = prevalence_values
  )
  
  return(list(
    mean_pabak = mean(pabak_values),
    mean_bias = mean(bias_values),
    mean_prevalence = mean(prevalence_values),
    median_pabak = median(pabak_values),
    sd_pabak = sd(pabak_values),
    feature_summary = feature_summary,
    n_features = length(valid_features)
  ))
}

# Function to print results in a formatted way
print_analysis_results <- function(results) {
  for(feature_name in names(results)) {
    cat("\nResults for feature:", feature_name, "\n")
    cat("Number of sessions analyzed:", results[[feature_name]]$n_sessions, "\n")
    cat("Confusion Matrix:\n")
    print(results[[feature_name]]$table)
    cat("Accuracy:", round(results[[feature_name]]$accuracy, 3), "\n")
    cat("Expected Agreement:", round(results[[feature_name]]$expected_agree, 3), "\n")
    cat("Cohen's Kappa:", round(results[[feature_name]]$kappa, 3), "\n")
    cat("PABAK:", round(results[[feature_name]]$pabak, 3), "\n")
    cat("Prevalence Index:", round(results[[feature_name]]$prevalence_index, 3), "\n")
    cat("Bias Index:", round(results[[feature_name]]$bias_index, 3), "\n")
    cat("----------------------------------------\n")
  }
}


# Function to print the comprehensive summary
print_metrics_summary <- function(summary_results) {
  cat("\nAgreement Statistics Summary:\n")
  cat("Number of features analyzed:", summary_results$n_features, "\n")
  cat("\nPABAK Statistics:\n")
  cat("Mean PABAK:", round(summary_results$mean_pabak, 3), "\n")
  cat("Median PABAK:", round(summary_results$median_pabak, 3), "\n")
  cat("Standard deviation PABAK:", round(summary_results$sd_pabak, 3), "\n")
  
  cat("\nKappa Statistics:\n")
  cat("Mean Kappa:", round(summary_results$mean_kappa, 3), "\n")
  cat("Median Kappa:", round(summary_results$median_kappa, 3), "\n")
  cat("Standard deviation Kappa:", round(summary_results$sd_kappa, 3), "\n")
  
  cat("\nValues by feature:\n")
  # Keep Feature column as is, round only numeric columns
  summary_results$feature_summary[] <- lapply(summary_results$feature_summary, function(x) {
    if(is.numeric(x)) round(x, 3) else x
  })
  print(summary_results$feature_summary)
}


print_PABAKmetrics_summary <- function(summary_results) {
  cat("\nAgreement Statistics Summary:\n")
  cat("Number of features analyzed:", summary_results$n_features, "\n")
  
  cat("\nPABAK Statistics:\n")
  cat("Mean PABAK:", round(summary_results$mean_pabak, 3), "\n")
  cat("Median PABAK:", round(summary_results$median_pabak, 3), "\n")
  cat("Standard deviation PABAK:", round(summary_results$sd_pabak, 3), "\n")
  
  cat("\nValues by feature:\n")
  # Keep Feature column as is, round only numeric columns
  summary_results$feature_summary[] <- lapply(summary_results$feature_summary, function(x) {
    if(is.numeric(x)) round(x, 3) else x
  })
  print(summary_results$feature_summary)
}
#Checking control features first 
#Kathryn-Maya
control_kathryn_maya <- analyze_control_features(kathryn, maya)

#Kathryn-Lauren
control_kathryn_lauren <- analyze_control_features(kathryn, lauren)
#Maya-Lauren
control_maya_lauren <- analyze_control_features(maya, lauren)
#Kathryn-Claude Opus 
control_kathryn_claude_opus <- analyze_control_features(kathryn, claude)
#Kathryn-Claude Sonnet
control_kathryn_claude_sonnet <- analyze_control_features(kathryn, claude_sonnet)
#Kathryn-Claude Shot
control_kathryn_claude_shot <- analyze_control_features(kathryn, claude_shot)
#Kathryn-Claude CoT
control_kathryn_claude_cot <- analyze_control_features(kathryn, claude_cot)
#Maya-Claude Opus
control_maya_claude_opus <- analyze_control_features(maya, claude)
#Maya Claude Sonnet
control_maya_claude_sonnet <- analyze_control_features(maya, claude_sonnet)
#Maya Claude Shot
control_maya_claude_shot <- analyze_control_features(maya, claude_shot)
#Maya Claude CoT
control_maya_claude_cot <- analyze_control_features(maya, claude_cot)
#Lauren Claude Opus
control_lauren_claude_opus <- analyze_control_features(lauren, claude)
#Lauren Claude Sonnet
control_lauren_claude_sonnet <- analyze_control_features(lauren, claude_sonnet)
#Lauren Claude Shot 
control_lauren_claude_shot <- analyze_control_features(lauren, claude_shot)
#Lauren Claude CoT
control_lauren_claude_cot <- analyze_control_features(lauren, claude_cot)


control <- rbind(
  
)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Opus for PRT 
prt_kathryn_claude <- analyze_prt_features(kathryn, claude)
prt_results_kathryn_claude <- print_analysis_results(prt_kathryn_claude)
prt_metrics_kathryn_claude <- calculate_average_metrics(prt_kathryn_claude)
print_metrics_summary(prt_metrics_kathryn_claude)


###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Sonnet for PRT 
prt_kathryn_claude_sonnet <- analyze_prt_features(kathryn, claude_sonnet)
prt_results_kathryn_claude_sonnet <- print_analysis_results(prt_kathryn_claude_sonnet)
prt_metrics_kathryn_claude_sonnet <- calculate_average_metrics(prt_kathryn_claude_sonnet)
print_metrics_summary(prt_metrics_kathryn_claude_sonnet)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Shot for PRT 
prt_kathryn_claude_shot <- analyze_prt_features(kathryn, claude_shot)
prt_results_kathryn_claude_shot <- print_analysis_results(prt_kathryn_claude_shot)
prt_metrics_kathryn_claude_shot <- calculate_average_metrics(prt_kathryn_claude_shot)
print_metrics_summary(prt_metrics_kathryn_claude_shot)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Shot w/ CoT for PRT 
prt_kathryn_claude_cot <- analyze_prt_features(kathryn, claude_cot)
prt_results_kathryn_claude_cot <- print_analysis_results(prt_kathryn_claude_cot)
prt_metrics_kathryn_claude_cot <- calculate_average_metrics(prt_kathryn_claude_cot)
print_metrics_summary(prt_metrics_kathryn_claude_cot)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude for CBT 
cbt_kathryn_claude <- analyze_cbt_features(kathryn, claude)
cbt_results_kathryn_claude <- print_analysis_results(cbt_kathryn_claude)
cbt_metrics_kathryn_claude <- calculate_average_metrics(cbt_kathryn_claude)
#PABAK specific measures here because one item had an expected agreement = 1
cbt_PABAKmetrics_kathryn_claude <- calculate_average_PABAKmetrics(cbt_kathryn_claude)
print_metrics_summary(cbt_metrics_kathryn_claude)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_claude)


control_features_kathryn_claude <- analyze_control_features(kathryn, claude)
control_features_summary_kathryn_claude <- calculate_average_PABAKmetrics(control_features_kathryn_claude)
print(control_features_summary_kathryn_claude)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Sonnet for CBT 
cbt_kathryn_claude_sonnet <- analyze_cbt_features(kathryn, claude_sonnet)
cbt_results_kathryn_claude_sonnet <- print_analysis_results(cbt_kathryn_claude_sonnet)
cbt_metrics_kathryn_claude_sonnet <- calculate_average_metrics(cbt_kathryn_claude_sonnet)
#PABAK specific measures here because one item had an expected agreement = 1
cbt_PABAKmetrics_kathryn_claude_sonnet <- calculate_average_PABAKmetrics(cbt_kathryn_claude_sonnet)
print_metrics_summary(cbt_metrics_kathryn_claude_sonnet)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_claude_sonnet)


control_features_kathryn_claude_sonnet <- analyze_control_features(kathryn, claude_sonnet)
control_features_summary_kathryn_claude_sonnet <- calculate_average_PABAKmetrics(control_features_kathryn_claude_sonnet)
print(control_features_summary_kathryn_claude_sonnet)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude Shot for CBT 
cbt_kathryn_claude_shot <- analyze_cbt_features(kathryn, claude_shot)
cbt_results_kathryn_claude_shot <- print_analysis_results(cbt_kathryn_claude_shot)
cbt_metrics_kathryn_claude_shot <- calculate_average_metrics(cbt_kathryn_claude_shot)
#PABAK specific measures here because one item had an expected agreement = 1
cbt_PABAKmetrics_kathryn_claude_shot <- calculate_average_PABAKmetrics(cbt_kathryn_claude_shot)
print_metrics_summary(cbt_metrics_kathryn_claude_shot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_claude_shot)


control_features_kathryn_claude_shot <- analyze_control_features(kathryn, claude_shot)
control_features_summary_kathryn_claude_shot <- calculate_average_PABAKmetrics(control_features_kathryn_claude_shot)
print(control_features_summary_kathryn_claude_shot)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Claude CoT for CBT 
cbt_kathryn_claude_cot <-  analyze_cbt_features(kathryn, claude_cot)
cbt_results_kathryn_claude_cot <- print_analysis_results(cbt_kathryn_claude_cot)
cbt_metrics_kathryn_claude_cot <- calculate_average_metrics(cbt_kathryn_claude_cot)
#PABAK specific measures here because one item had an expected agreement = 1
cbt_PABAKmetrics_kathryn_claude_cot <- calculate_average_PABAKmetrics(cbt_kathryn_claude_cot)
print_metrics_summary(cbt_metrics_kathryn_claude_cot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_claude_cot)


control_features_kathryn_claude_cot <- analyze_control_features(kathryn, claude_cot)
control_features_summary_kathryn_claude_cot <- calculate_average_PABAKmetrics(control_features_kathryn_claude_cot)
print(control_features_summary_kathryn_claude_cot)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Maya for PRT
prt_kathryn_maya <- analyze_prt_features(kathryn, maya)
prt_results_kathryn_maya <- print_analysis_results(prt_kathryn_maya)
prt_metrics_kathryn_maya <- calculate_average_metrics(prt_kathryn_maya)
print_metrics_summary(prt_metrics_kathryn_maya)


###-------------------------------------------------------------------------------------------------------------
#Kathryn * Maya for CBT
cbt_kathryn_maya <- analyze_cbt_features(kathryn, maya)
cbt_results_kathryn_maya <- print_analysis_results(cbt_kathryn_maya)
cbt_metrics_kathryn_maya <- calculate_average_metrics(cbt_kathryn_maya)
print_metrics_summary(cbt_metrics_kathryn_maya)

cbt_PABAKmetrics_kathryn_maya <- calculate_average_PABAKmetrics(cbt_kathryn_maya)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_maya)


###-------------------------------------------------------------------------------------------------------------
#Kathryn * Lauren for PRT
prt_kathryn_lauren <- analyze_prt_features(kathryn, lauren)
prt_results_kathryn_lauren <- print_analysis_results(prt_kathryn_lauren)
prt_metrics_kathryn_lauren <- calculate_average_metrics(prt_kathryn_lauren)
print_metrics_summary(prt_metrics_kathryn_lauren)

###-------------------------------------------------------------------------------------------------------------
#Kathryn * Lauren for CBT
cbt_kathryn_lauren <- analyze_cbt_features(kathryn, lauren)
cbt_results_kathryn_lauren <- print_analysis_results(cbt_kathryn_lauren)
cbt_metrics_kathryn_lauren <- calculate_average_metrics(cbt_kathryn_lauren)
print_metrics_summary(cbt_metrics_kathryn_lauren)
#PABAK specific metrics because two (8-1 and 8-2) features have Pe = 1
cbt_PABAKmetrics_kathryn_lauren <- calculate_average_PABAKmetrics(cbt_kathryn_lauren)
print_PABAKmetrics_summary(cbt_PABAKmetrics_kathryn_lauren)


###-------------------------------------------------------------------------------------------------------------
#Maya * Claude for PRT
prt_maya_claude <- analyze_prt_features(maya, claude)
prt_results_maya_claude <- print_analysis_results(prt_maya_claude)
prt_metrics_maya_claude <- calculate_average_metrics(prt_maya_claude)
print_metrics_summary(prt_metrics_maya_claude)

control_features_maya_claude <- analyze_control_features(maya, claude)
control_features_summary_maya_claude <- calculate_average_PABAKmetrics(control_features_maya_claude)
print(control_features_summary_maya_claude)

###-------------------------------------------------------------------------------------------------------------
#Maya * Claude Sonnet for PRT
prt_maya_claude_sonnet <- analyze_prt_features(maya, claude_sonnet)
prt_results_maya_claude_sonnet <- print_analysis_results(prt_maya_claude_sonnet)
prt_metrics_maya_claude_sonnet <- calculate_average_metrics(prt_maya_claude_sonnet)
print_metrics_summary(prt_metrics_maya_claude_sonnet)

control_features_maya_claude_sonnet <- analyze_control_features(maya, claude_sonnet)
control_features_summary_maya_claude_sonnet <- calculate_average_PABAKmetrics(control_features_maya_claude_sonnet)
print(control_features_summary_maya_claude_sonnet)

###-------------------------------------------------------------------------------------------------------------
#Maya * Claude Shot for PRT
prt_maya_claude_shot <- analyze_prt_features(maya, claude_shot)
prt_results_maya_claude_shot <- print_analysis_results(prt_maya_claude_shot)
prt_metrics_maya_claude_shot <- calculate_average_metrics(prt_maya_claude_shot)
print_metrics_summary(prt_metrics_maya_claude_shot)

control_features_maya_claude_shot <- analyze_control_features(maya, claude_shot)
control_features_summary_maya_claude_shot <- calculate_average_PABAKmetrics(control_features_maya_claude_shot)
print(control_features_summary_maya_claude_shot)

###-------------------------------------------------------------------------------------------------------------
#Maya * Claude CoT for PRT
prt_maya_claude_cot <- analyze_prt_features(maya, claude_cot)
prt_results_maya_claude_cot <- print_analysis_results(prt_maya_claude_cot)
prt_metrics_maya_claude_cot <- calculate_average_metrics(prt_maya_claude_cot)
print_metrics_summary(prt_metrics_maya_claude_cot)

control_features_maya_claude_cot <- analyze_control_features(maya, claude_cot)
control_features_summary_maya_claude_cot <- calculate_average_PABAKmetrics(control_features_maya_claude_cot)
print(control_features_summary_maya_claude_cot)

###-------------------------------------------------------------------------------------------------------------
#Maya * Claude for CBT
cbt_maya_claude <- analyze_cbt_features(maya, claude)
cbt_results_maya_claude <- print_analysis_results(cbt_maya_claude)
cbt_metrics_maya_claude <- calculate_average_metrics(cbt_maya_claude)
print_metrics_summary(cbt_metrics_maya_claude)
#PABAK specific metrics because one (6-1) feature have Pe = 1
cbt_PABAKmetrics_maya_claude <- calculate_average_PABAKmetrics(cbt_maya_claude)
print_PABAKmetrics_summary(cbt_PABAKmetrics_maya_claude)


###-------------------------------------------------------------------------------------------------------------
#Maya * Claude Sonnet for CBT
cbt_maya_claude_sonnet <- analyze_cbt_features(maya, claude_sonnet)
cbt_results_maya_claude_sonnet <- print_analysis_results(cbt_maya_claude_sonnet)
cbt_metrics_maya_claude_sonnet <- calculate_average_metrics(cbt_maya_claude_sonnet)
print_metrics_summary(cbt_metrics_maya_claude_sonnet)
#PABAK specific metrics because one (6-1) feature have Pe = 1
cbt_PABAKmetrics_maya_claude_sonnet <- calculate_average_PABAKmetrics(cbt_maya_claude_sonnet)
print_PABAKmetrics_summary(cbt_PABAKmetrics_maya_claude_sonnet)


###-------------------------------------------------------------------------------------------------------------
#Maya * Claude Shot for CBT
cbt_maya_claude_shot <- analyze_cbt_features(maya, claude_shot)
cbt_results_maya_claude_shot <- print_analysis_results(cbt_maya_claude_shot)
cbt_metrics_maya_claude_shot <- calculate_average_metrics(cbt_maya_claude_shot)
print_metrics_summary(cbt_metrics_maya_claude_shot)
#PABAK specific metrics because one (6-1) feature have Pe = 1
cbt_PABAKmetrics_maya_claude_shot <- calculate_average_PABAKmetrics(cbt_maya_claude_shot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_maya_claude_shot)


###-------------------------------------------------------------------------------------------------------------
#Maya * Claude CoT for CBT
cbt_maya_claude_cot <- analyze_cbt_features(maya, claude_cot)
cbt_results_maya_claude_cot <- print_analysis_results(cbt_maya_claude_cot)
cbt_metrics_maya_claude_cot <- calculate_average_metrics(cbt_maya_claude_cot)
print_metrics_summary(cbt_metrics_maya_claude_cot)
#PABAK specific metrics because one (6-1) feature have Pe = 1
cbt_PABAKmetrics_maya_claude_cot <- calculate_average_PABAKmetrics(cbt_maya_claude_cot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_maya_claude_cot)


###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude for PRT
prt_lauren_claude <- analyze_prt_features(lauren, claude)
prt_results_lauren_claude <- print_analysis_results(prt_lauren_claude)
prt_metrics_lauren_claude <- calculate_average_metrics(prt_lauren_claude)
print_metrics_summary(prt_metrics_lauren_claude)

control_features_lauren_claude <- analyze_control_features(lauren, claude)
control_features_summary_lauren_claude <- calculate_average_PABAKmetrics(control_features_lauren_claude)
print(control_features_summary_lauren_claude)

###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude Sonnet for PRT
prt_lauren_claude_sonnet <- analyze_prt_features(lauren, claude_sonnet)
prt_results_lauren_claude_sonnet <- print_analysis_results(prt_lauren_claude_sonnet)
prt_metrics_lauren_claude_sonnet <- calculate_average_metrics(prt_lauren_claude_sonnet)
print_metrics_summary(prt_metrics_lauren_claude_sonnet)


control_features_lauren_claude_sonnet <- analyze_control_features(lauren, claude_sonnet)
control_features_summary_lauren_claude_sonnet <- calculate_average_PABAKmetrics(control_features_lauren_claude_sonnet)
print(control_features_summary_lauren_claude_sonnet)

###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude Shot for PRT
prt_lauren_claude_shot <- analyze_prt_features(lauren, claude_shot)
prt_results_lauren_claude_shot <- print_analysis_results(prt_lauren_claude_shot)
prt_metrics_lauren_claude_shot <- calculate_average_metrics(prt_lauren_claude_shot)
print_metrics_summary(prt_metrics_lauren_claude_shot)


###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude CoT for PRT
prt_lauren_claude_cot <- analyze_prt_features(lauren, claude_cot)
prt_results_lauren_claude_cot <- print_analysis_results(prt_lauren_claude_cot)
prt_metrics_lauren_claude_cot <- calculate_average_metrics(prt_lauren_claude_cot)
print_metrics_summary(prt_metrics_lauren_claude_cot)

###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude for CBT
cbt_lauren_claude <- analyze_cbt_features(lauren, claude)
cbt_results_lauren_claude <- print_analysis_results(cbt_lauren_claude)
cbt_metrics_lauren_claude <- calculate_average_metrics(cbt_lauren_claude)
print_metrics_summary(cbt_metrics_lauren_claude)
#PABAK specific metrics because three (6-1, 6-3, & 8-1) features have Pe = 1
cbt_PABAKmetrics_lauren_claude <- calculate_average_PABAKmetrics(cbt_lauren_claude)
print_PABAKmetrics_summary(cbt_PABAKmetrics_lauren_claude)


###--------------------------------------------------------------------------------------------------------------
#Lauren * Claude Sonnet for CBT
cbt_lauren_claude_sonnet <- analyze_cbt_features(lauren, claude_sonnet)
cbt_results_lauren_claude_sonnet <- print_analysis_results(cbt_lauren_claude_sonnet)
cbt_metrics_lauren_claude_sonnet <- calculate_average_metrics(cbt_lauren_claude_sonnet)
print_metrics_summary(cbt_metrics_lauren_claude_sonnet)
#PABAK specific metrics because three (6-1, 6-3, & 8-1) features have Pe = 1
cbt_PABAKmetrics_lauren_claude_sonnet <- calculate_average_PABAKmetrics(cbt_lauren_claude_sonnet)
print_PABAKmetrics_summary(cbt_PABAKmetrics_lauren_claude_sonnet)


###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude Shot for CBT
cbt_lauren_claude_shot <- analyze_cbt_features(lauren, claude_shot)
cbt_results_lauren_claude_shot <- print_analysis_results(cbt_lauren_claude_shot)
cbt_metrics_lauren_claude_shot <- calculate_average_metrics(cbt_lauren_claude_shot)
print_metrics_summary(cbt_metrics_lauren_claude_shot)
#PABAK specific metrics because three (6-1, 6-3, & 8-1) features have Pe = 1
cbt_PABAKmetrics_lauren_claude_shot <- calculate_average_PABAKmetrics(cbt_lauren_claude_shot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_lauren_claude_shot)


###-------------------------------------------------------------------------------------------------------------
#Lauren * Claude CoT for CBT
cbt_lauren_claude_cot <- analyze_cbt_features(lauren, claude_cot)
cbt_results_lauren_claude_cot <- print_analysis_results(cbt_lauren_claude_cot)
cbt_metrics_lauren_claude_cot <- calculate_average_metrics(cbt_lauren_claude_cot)
print_metrics_summary(cbt_metrics_lauren_claude_cot)
#PABAK specific metrics because three (6-1, 6-3, & 8-1) features have Pe = 1
cbt_PABAKmetrics_lauren_claude_cot <- calculate_average_PABAKmetrics(cbt_lauren_claude_cot)
print_PABAKmetrics_summary(cbt_PABAKmetrics_lauren_claude_cot)



###-------------------------------------------------------------------------------------------------------------
#Lauren * Maya for PRT
prt_lauren_maya <- analyze_prt_features(lauren, maya)
prt_results_lauren_maya <- print_analysis_results(prt_lauren_maya)
prt_metrics_lauren_maya <- calculate_average_metrics(prt_lauren_maya)
print_metrics_summary(prt_metrics_lauren_maya)

###-------------------------------------------------------------------------------------------------------------
#Lauren * Maya for CBT
cbt_lauren_maya <- analyze_cbt_features(lauren, maya)
cbt_results_lauren_maya <- print_analysis_results(cbt_lauren_maya)
cbt_metrics_lauren_maya <- calculate_average_metrics(cbt_lauren_maya)
print_metrics_summary(cbt_metrics_lauren_maya)
#PABAK specific metrics because one (6_1) feature have Pe = 1
cbt_PABAKmetrics_lauren_maya <- calculate_average_PABAKmetrics(cbt_lauren_maya)
print_PABAKmetrics_summary(cbt_PABAKmetrics_lauren_maya)


###-------------------------------------------------------------------------------------------------------------
#Creating function to extract these values into a dataframe so I can create plots 
# Function for extracting KAPPA values
extract_kappa_metrics <- function(results, comparison_name, therapy_type){
  metrics_df <- data.frame(
    comparison = comparison_name,
    therapy_type = therapy_type, 
    feature = results$feature_summary$Feature,
    kappa = results$feature_summary$Kappa,
    bias = results$feature_summary$Bias,
    prevalence = results$feature_summary$Prevalence
  )
  return(metrics_df)
}

extract_pabak_metrics <- function(results, comparison_name, therapy_type){
  metrics_df <- data.frame(
    comparison = comparison_name,
    therapy_type = therapy_type, 
    feature = results$feature_summary$Feature,
    pabak = results$feature_summary$PABAK,
    bias = results$feature_summary$Bias,
    prevalence = results$feature_summary$Prevalence
  )
  return(metrics_df)
}

# Function to extract control feature metrics into a dataframe
extract_control_metrics <- function(control_results, comparison_name) {
  # Initialize empty dataframe
  control_df <- data.frame()
  
  # Loop through each control feature in the results
  for(feature_name in names(control_results)) {
    feature_row <- data.frame(
      comparison = comparison_name,
      feature = feature_name,
      accuracy = control_results[[feature_name]]$accuracy,
      kappa = control_results[[feature_name]]$kappa,
      pabak = control_results[[feature_name]]$pabak,
      prevalence_index = control_results[[feature_name]]$prevalence_index,
      bias_index = control_results[[feature_name]]$bias_index,
      n_sessions = control_results[[feature_name]]$n_sessions
    )
    control_df <- rbind(control_df, feature_row)
  }
  
  return(control_df)
}

###------Control data frame
# Extract all control feature results into individual dataframes
control_metrics_list <- list(
  # Human-Human comparisons
  extract_control_metrics(control_kathryn_maya, "Kathryn-Maya"),
  extract_control_metrics(control_kathryn_lauren, "Kathryn-Lauren"),
  extract_control_metrics(control_maya_lauren, "Maya-Lauren"),
  
  # Kathryn-Claude comparisons
  extract_control_metrics(control_kathryn_claude_opus, "Kathryn-Claude Opus"),
  extract_control_metrics(control_kathryn_claude_sonnet, "Kathryn-Claude Sonnet"),
  extract_control_metrics(control_kathryn_claude_shot, "Kathryn-Claude Shot"),  # Note: fix typo in original
  extract_control_metrics(control_kathryn_claude_cot, "Kathryn-Claude CoT"),
  
  # Maya-Claude comparisons
  extract_control_metrics(control_maya_claude_opus, "Maya-Claude Opus"),
  extract_control_metrics(control_maya_claude_sonnet, "Maya-Claude Sonnet"),
  extract_control_metrics(control_maya_claude_shot, "Maya-Claude Shot"),
  extract_control_metrics(control_maya_claude_cot, "Maya-Claude CoT"),
  
  # Lauren-Claude comparisons
  extract_control_metrics(control_lauren_claude_opus, "Lauren-Claude Opus"),
  extract_control_metrics(control_lauren_claude_sonnet, "Lauren-Claude Sonnet"),
  extract_control_metrics(control_lauren_claude_shot, "Lauren-Claude Shot"),
  extract_control_metrics(control_lauren_claude_cot, "Lauren-Claude CoT")
)

# Combine all dataframes into one comprehensive dataframe
control_features_combined <- do.call(rbind, control_metrics_list)

# Clean up row names
rownames(control_features_combined) <- NULL

summary(control_features_combined$pabak)
sd(control_features_combined$pabak)
###-------------------------------------------------------------------------------------------------------------
# Create dataframe for KAPPA values
kappa_metrics <- rbind(
  # Kathryn & Claude Opus comparisons
  extract_kappa_metrics(prt_metrics_kathryn_claude, "Kathryn-Claude Opus", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_claude, "Kathryn-Claude Opus", "CBT"),
  
  # Kathryn & Maya comparisons
  extract_kappa_metrics(prt_metrics_kathryn_maya, "Kathryn-Maya", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_maya, "Kathryn-Maya", "CBT"),
  
  # Kathryn & Lauren comparisons
  extract_kappa_metrics(prt_metrics_kathryn_lauren, "Kathryn-Lauren", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_lauren, "Kathryn-Lauren", "CBT"),
  
  # Maya & Claude Opus comparisons
  extract_kappa_metrics(prt_metrics_maya_claude, "Maya-Claude Opus", "PRT"),
  extract_kappa_metrics(cbt_metrics_maya_claude, "Maya-Claude Opus", "CBT"),
  
  # Lauren & Claude Opus comparisons
  extract_kappa_metrics(prt_metrics_lauren_claude, "Lauren-Claude Opus", "PRT"),
  extract_kappa_metrics(cbt_metrics_lauren_claude, "Lauren-Claude Opus", "CBT"),
  
  #Lauren & Maya comparisons
  extract_kappa_metrics(prt_metrics_lauren_maya, "Lauren-Maya", "PRT"),
  extract_kappa_metrics(cbt_metrics_lauren_maya, "Lauren-Maya", "CBT"),
  
  #Kathryn Claude Sonnet 
  extract_kappa_metrics(prt_metrics_kathryn_claude_sonnet, "Kathryn-Claude Sonnet", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_claude_sonnet, "Kathryn-Claude Sonnet", "CBT"),
  
  #Kathryn Claude Shot
  extract_kappa_metrics(prt_metrics_kathryn_claude_shot, "Kathryn-Claude Shot", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_claude_shot, "Kathryn-Claude Shot", "CBT"),
  
  #Kathryn Claude CoT
  extract_kappa_metrics(prt_metrics_kathryn_claude_cot, "Kathryn-Claude CoT", "PRT"),
  extract_kappa_metrics(cbt_metrics_kathryn_claude_cot, "Kathryn-Claude CoT", "CBT"),
  
  #Lauren Claude Sonnet
  extract_kappa_metrics(prt_metrics_lauren_claude_sonnet, "Lauren-Claude Sonnet", "PRT"),
  extract_kappa_metrics(cbt_metrics_lauren_claude_sonnet, "Lauren-Claude Sonnet", "CBT"),
  
  #Lauren Claude Shot
  extract_kappa_metrics(prt_metrics_lauren_claude_shot, "Lauren-Claude Shot", "PRT"),
  extract_kappa_metrics(cbt_metrics_lauren_claude_shot, "Lauren-Claude Shot", "CBT"),
  
  #Lauren Claude CoT
  extract_kappa_metrics(prt_metrics_lauren_claude_cot, "Lauren-Claude CoT", "PRT"),
  extract_kappa_metrics(cbt_metrics_lauren_claude_cot, "Lauren-Claude CoT", "CBT"),
  
  #Maya Claude Sonnet
  extract_kappa_metrics(prt_metrics_maya_claude_sonnet, "Maya-Claude Sonnet", "PRT"),
  extract_kappa_metrics(cbt_metrics_maya_claude_sonnet, "Maya-Claude Sonnet", "CBT"),
  
  #Maya Claude Shot
  extract_kappa_metrics(prt_metrics_maya_claude_shot, "Maya-Claude Shot", "PRT"),
  extract_kappa_metrics(cbt_metrics_maya_claude_shot, "Maya-Claude Shot", "CBT"),
  
  #Maya Claude CoT
  extract_kappa_metrics(prt_metrics_maya_claude_cot, "Maya-Claude CoT", "PRT"),
  extract_kappa_metrics(cbt_metrics_maya_claude_cot, "Maya-Claude CoT", "CBT")
  
  
  
)


summary(kappa_metrics$bias)
summary(kappa_metrics$prevalence)
summary(kappa_metrics$kappa)
sd(kappa_metrics$kappa)



kappa_df <- kappa_metrics %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(kappa, na.rm=TRUE)) %>% 
  arrange(mean)

summary(kappa_df$mean)
sd(kappa_df$mean)

human_claude_kappa <- kappa_metrics %>% 
  filter(comparison %in% c("Kathryn-Claude Opus" , "Maya-Claude Opus" ,
                           "Lauren-Claude Opus" , "Kathryn-Claude Sonnet" ,
                           "Kathryn-Claude Shot" , "Kathryn-Claude CoT" , 
                           "Lauren-Claude Sonnet" , "Lauren-Claude Shot" ,
                           "Lauren-Claude CoT", "Maya-Claude Sonnet" ,
                           "Maya-Claude Shot" , "Maya-Claude CoT"))

summary(human_claude_kappa$kappa)

kappa_df_human_claude <- human_claude_kappa %>% 
  group_by(comparison, therapy_type) %>% 
  summarize(mean = mean(kappa, na.rm=TRUE)) %>% 
  arrange(mean)


kappa_df_prt <- kappa_df_human_claude %>% 
  filter(therapy_type == "PRT")

summary(kappa_df_prt$mean)
sd(kappa_df_prt$mean)

kappa_df_cbt <- kappa_df_human_claude %>% 
  filter(therapy_type == "CBT")

summary(kappa_df_cbt$mean)
sd(kappa_df_cbt$mean)

human_kappa <- kappa_metrics %>% 
  filter(comparison == "Kathryn-Maya" | comparison == "Kathryn-Lauren" | 
           comparison == "Lauren-Maya")

summary(human_kappa$kappa)

human_kappa_df <- human_kappa %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(kappa, na.rm=TRUE)) %>% 
  arrange(mean)

summary(human_kappa_df$mean)
sd(human_kappa_df$mean, na.rm=TRUE)

human_kappa_prt <- human_kappa %>% 
  filter(therapy_type == "PRT") %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(kappa, na.rm=TRUE)) %>% 
  arrange(mean)

summary(human_kappa_prt$mean)
sd(human_kappa_prt$mean)

human_kappa_cbt <- human_kappa %>% 
  filter(therapy_type == "CBT")%>% 
  group_by(comparison) %>% 
  summarize(mean = mean(kappa, na.rm=TRUE)) %>% 
  arrange(mean)

summary(human_kappa_cbt$mean)
sd(human_kappa_cbt$mean)

###-------------------------------------------------------------------------------------------------------------

pabak_metrics <- rbind(
  # Kathryn & Claude Opus comparisons
  extract_pabak_metrics(prt_metrics_kathryn_claude, "Kathryn-Claude Opus", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_kathryn_claude, "Kathryn-Claude Opus", "CBT"),
  
  # Kathryn & Maya comparisons
  extract_pabak_metrics(prt_metrics_kathryn_maya, "Kathryn-Maya", "PRT"),
  extract_pabak_metrics(cbt_metrics_kathryn_maya, "Kathryn-Maya", "CBT"),
  
  # Kathryn & Lauren comparisons
  extract_pabak_metrics(prt_metrics_kathryn_lauren, "Kathryn-Lauren", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_kathryn_lauren, "Kathryn-Lauren", "CBT"),
  
  # Maya & Claude Opus comparisons
  extract_pabak_metrics(prt_metrics_maya_claude, "Maya-Claude Opus", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_maya_claude, "Maya-Claude Opus", "CBT"),
  
  # Lauren & Claude Opus comparisons
  extract_pabak_metrics(prt_metrics_lauren_claude, "Lauren-Claude Opus", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_lauren_claude, "Lauren-Claude Opus", "CBT"),
  
  #Lauren & Maya comparisons
  extract_pabak_metrics(prt_metrics_lauren_maya, "Lauren-Maya", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_lauren_maya, "Lauren-Maya", "CBT"),
  
  #Kathryn Claude Sonnet 
  extract_pabak_metrics(prt_metrics_kathryn_claude_sonnet, "Kathryn-Claude Sonnet", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_kathryn_claude_sonnet, "Kathryn-Claude Sonnet", "CBT"),
  
  #Kathryn Claude Shot
  extract_pabak_metrics(prt_metrics_kathryn_claude_shot, "Kathryn-Claude Shot", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_kathryn_claude_shot, "Kathryn-Claude Shot", "CBT"),
  
  #Kathryn Claude CoT
  extract_pabak_metrics(prt_metrics_kathryn_claude_cot, "Kathryn-Claude CoT", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_kathryn_claude_cot, "Kathryn-Claude CoT", "CBT"),
  
  #Lauren Claude Sonnet
  extract_pabak_metrics(prt_metrics_lauren_claude_sonnet, "Lauren-Claude Sonnet", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_lauren_claude_sonnet, "Lauren-Claude Sonnet", "CBT"),
  
  #Lauren Claude Shot
  extract_pabak_metrics(prt_metrics_lauren_claude_shot, "Lauren-Claude Shot", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_lauren_claude_shot, "Lauren-Claude Shot", "CBT"),
  
  #Lauren Claude CoT
  extract_pabak_metrics(prt_metrics_lauren_claude_cot, "Lauren-Claude CoT", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_lauren_claude_cot, "Lauren-Claude CoT", "CBT"),
  
  #Maya Claude Sonnet
  extract_pabak_metrics(prt_metrics_maya_claude_sonnet, "Maya-Claude Sonnet", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_maya_claude_sonnet, "Maya-Claude Sonnet", "CBT"),
  
  #Maya Claude Shot
  extract_pabak_metrics(prt_metrics_maya_claude_shot, "Maya-Claude Shot", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_maya_claude_shot, "Maya-Claude Shot", "CBT"),
  
  #Maya Claude CoT
  extract_pabak_metrics(prt_metrics_maya_claude_cot, "Maya-Claude CoT", "PRT"),
  extract_pabak_metrics(cbt_PABAKmetrics_maya_claude_cot, "Maya-Claude CoT", "CBT")
  
)

summary(pabak_metrics$pabak)
sd(pabak_metrics$pabak)

pabak_df <- pabak_metrics %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(pabak, na.rm=TRUE)) %>% 
  arrange(mean)

summary(pabak_df$mean)
sd(pabak_df$mean)

human_claude_pabak <- pabak_metrics %>% 
  filter(comparison %in% c("Kathryn-Claude Opus" , "Maya-Claude Opus" ,
                           "Lauren-Claude Opus" , "Kathryn-Claude Sonnet" ,
                           "Kathryn-Claude Shot" , "Kathryn-Claude CoT" , 
                           "Lauren-Claude Sonnet" , "Lauren-Claude Shot" ,
                           "Lauren-Claude CoT", "Maya-Claude Sonnet" ,
                           "Maya-Claude Shot" , "Maya-Claude CoT"))




pabak_df_prt <- human_claude_pabak %>% 
  filter(therapy_type == "PRT") %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(pabak, na.rm=TRUE)) %>% 
  arrange(mean)

summary(pabak_df_prt$mean)
sd(pabak_df_prt$mean)

pabak_df_cbt <- human_claude_pabak %>% 
  filter(therapy_type == "CBT")  %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(pabak, na.rm=TRUE)) %>% 
  arrange(mean)

summary(pabak_df_cbt$mean)
sd(pabak_df_cbt$mean)

#Human comparison 
table(pabak_metrics$comparison)

humans_only <- pabak_metrics %>% 
  filter(comparison %in% c("Kathryn-Lauren", "Kathryn-Maya", "Lauren-Maya"))

humans_only_prt <- humans_only %>% 
  filter(therapy_type == "PRT") %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(pabak, na.rm=TRUE)) %>% 
  arrange(mean)

summary(humans_only_prt$mean)
sd(humans_only_prt$mean)


humans_only_cbt <- humans_only %>% 
  filter(therapy_type == "CBT") %>% 
  group_by(comparison) %>% 
  summarize(mean = mean(pabak, na.rm=TRUE)) %>% 
  arrange(mean)

summary(humans_only_cbt$mean)
sd(humans_only_cbt$mean)


claude_cot_prt <- c(0.40, 0.35, 0.5)
summary(claude_cot_prt)
sd(claude_cot_prt)

claude_cot_cbt <- c(0.53, 0.62, 0.57)
summary(claude_cot_cbt)
sd(claude_cot_cbt)
###-------------------------------------------------------------------------------------------------------------
#Paired boxplots 

table(pabak_metrics$comparison)

pabak_metrics <- pabak_metrics %>% 
  mutate(comparison = case_when(
    comparison == "Kathryn-Claude CoT" ~ "Rater 1-Claude CoT", 
    comparison == "Kathryn-Claude Opus" ~ "Rater 1-Claude Opus", 
    comparison == "Kathryn-Claude Shot" ~ "Rater 1-Claude Shot Prompt", 
    comparison == "Kathryn-Claude Sonnet" ~ "Rater 1-Claude Sonnet",
    comparison == "Kathryn-Lauren" ~ "Rater 1-Rater 3", 
    comparison == "Kathryn-Maya" ~ "Rater 1-Rater 2", 
    comparison == "Lauren-Claude CoT" ~ "Rater 3-Claude CoT", 
    comparison == "Lauren-Claude Opus" ~ "Rater 3-Claude Opus", 
    comparison == "Lauren-Claude Shot" ~ "Rater 3-Claude Shot Prompt", 
    comparison == "Lauren-Claude Sonnet" ~ "Rater 3-Claude Sonnet", 
    comparison == "Lauren-Maya" ~ "Rater 2-Rater 3",
    comparison == "Maya-Claude CoT" ~ "Rater 2-Claude CoT", 
    comparison == "Maya-Claude Opus" ~ "Rater 2-Claude Opus", 
    comparison == "Maya-Claude Shot" ~ "Rater 2-Claude Shot Prompt", 
    comparison == "Maya-Claude Sonnet" ~ "Rater 2-Claude Sonnet"
  )) %>% 
  mutate(jittered_pabak = pabak - runif(n(), 0, 0.1))


pabak_claude_opus <- pabak_metrics %>% 
  filter(comparison == "Rater 1-Claude Opus" | comparison == "Rater 2-Claude Opus" | comparison == "Rater 3-Claude Opus" |
           comparison == "Rater 1-Rater 3" | comparison == "Rater 2-Rater 3" | comparison == "Rater 1-Rater 2")

pabak_claude_opus$comparison <- factor(pabak_claude_opus$comparison, 
                                   levels = c("Rater 1-Claude Opus", "Rater 2-Claude Opus", "Rater 3-Claude Opus", 
                                              "Rater 1-Rater 2", "Rater 1-Rater 3", "Rater 2-Rater 3"))

pabak_claude_opus_cbt <- pabak_claude_opus %>% 
  filter(therapy_type == "CBT")

pabak_claude_opus_prt <- pabak_claude_opus %>% 
  filter(therapy_type == "PRT")

opus_cbt <- pabak_claude_opus_cbt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "coral") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "CBT",
       x = "",
       y = "PABAK Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))


opus_prt <- pabak_claude_opus_prt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "lavender") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "PRT",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))

opus_cbt + opus_prt + plot_annotation(tag_levels = "A")


#SONNET
pabak_claude_sonnet <- pabak_metrics %>% 
  filter(comparison == "Rater 1-Claude Sonnet" | comparison == "Rater 2-Claude Sonnet" | comparison == "Rater 3-Claude Sonnet" |
           comparison == "Rater 1-Rater 3" | comparison == "Rater 2-Rater 3" | comparison == "Rater 1-Rater 2")

pabak_claude_sonnet$comparison <- factor(pabak_claude_sonnet$comparison, 
                                       levels = c("Rater 1-Claude Sonnet", "Rater 2-Claude Sonnet", "Rater 3-Claude Sonnet", 
                                                  "Rater 1-Rater 2", "Rater 1-Rater 3", "Rater 2-Rater 3"))

pabak_claude_sonnet_cbt <- pabak_claude_sonnet %>% 
  filter(therapy_type == "CBT")


pabak_claude_sonnet_prt <- pabak_claude_sonnet %>% 
  filter(therapy_type == "PRT")

sonnet_cbt <- pabak_claude_sonnet_cbt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "coral") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "CBT",
       x = "",
       y = "PABAK Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))

sonnet_prt <- pabak_claude_sonnet_prt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "lavender") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "PRT",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))

sonnet_cbt + sonnet_prt + plot_annotation(tag_levels = "A")

#SHOT PROMPT 
pabak_claude_shot <- pabak_metrics %>% 
  filter(comparison == "Rater 1-Claude Shot Prompt" | comparison == "Rater 2-Claude Shot Prompt" | comparison == "Rater 3-Claude Shot Prompt" |
           comparison == "Rater 1-Rater 3" | comparison == "Rater 2-Rater 3" | comparison == "Rater 1-Rater 2")


pabak_claude_shot <- pabak_claude_shot %>% 
  mutate(comparison = case_when(
    comparison == "Rater 1-Claude Shot Prompt" ~ "Rater 1-Claude Shot", 
    comparison == "Rater 2-Claude Shot Prompt" ~ "Rater 2-Claude Shot", 
    comparison == "Rater 3-Claude Shot Prompt" ~ "Rater 3-Claude Shot", 
    comparison == "Rater 1-Rater 2" ~ "Rater 1-Rater 2", 
    comparison == "Rater 1-Rater 3" ~ "Rater 1-Rater 3", 
    comparison == "Rater 2-Rater 3" ~ "Rater 2-Rater 3"
  )) 

pabak_claude_shot <- pabak_claude_shot %>%
  mutate(comparison = factor(comparison, 
                             levels = c("Rater 1-Claude Shot", "Rater 2-Claude Shot", "Rater 3-Claude Shot", 
                                        "Rater 1-Rater 2", "Rater 1-Rater 3", "Rater 2-Rater 3")))

pabak_claude_shot_cbt <- pabak_claude_shot %>% 
  filter(therapy_type == "CBT")

pabak_claude_shot_prt <- pabak_claude_shot %>% 
  filter(therapy_type == "PRT")

shot_cbt <- pabak_claude_shot_cbt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "coral") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "CBT",
       x = "",
       y = "PABAK Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))

shot_prt <- pabak_claude_shot_prt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "lavender") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "PRT",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))


shot_cbt + shot_prt + plot_annotation(tag_levels = "A")

#CHAIN OF THOUGHT
pabak_claude_cot <- pabak_metrics %>% 
  filter(comparison == "Rater 1-Claude CoT" | comparison == "Rater 2-Claude CoT" | comparison == "Rater 3-Claude CoT" |
           comparison == "Rater 1-Rater 3" | comparison == "Rater 2-Rater 3" | comparison == "Rater 1-Rater 2")

pabak_claude_cot$comparison <- factor(pabak_claude_cot$comparison, 
                                         levels = c("Rater 1-Claude CoT", "Rater 2-Claude CoT", "Rater 3-Claude CoT", 
                                                    "Rater 1-Rater 2", "Rater 1-Rater 3", "Rater 2-Rater 3"))

pabak_claude_cot_cbt <- pabak_claude_cot %>% 
  filter(therapy_type == "CBT")

pabak_claude_cot_prt <- pabak_claude_cot %>% 
  filter(therapy_type == "PRT")

cot_cbt <- pabak_claude_cot_cbt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "coral") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "CBT",
       x = "",
       y = "PABAK Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4,0.8), limits = c(-0.85, 1))


cot_prt <- pabak_claude_cot_prt %>% 
  ggplot(aes(x=comparison, y=pabak)) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.20), 
            fill = "#E8F5E9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.40), 
            fill = "#C8E6C9", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.60), 
            fill = "#A5D6A7", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.60, ymax = 0.80), 
            fill = "#81C784", alpha = 0.7, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.80, ymax = 1), 
            fill = "#4CAF50", alpha = 0.7, inherit.aes = FALSE) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7, fill = "lavender") +
  geom_point(aes(y = jittered_pabak),
             position = position_dodge(width = 0.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               position = position_dodge(width = 0.9),
               aes(group = interaction(comparison, therapy_type)),
               color = "black", fill = "white") +
  labs(title = "PRT",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size=14, face="bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 25),
    axis.text.y = element_text(face = "bold", size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 25), 
    legend.title = element_blank()
  ) + scale_y_continuous(breaks = c(-0.5, 0, 0.4, 0.8), limits = c(-0.85, 1))


cot_cbt + cot_prt + plot_annotation(tag_levels = "A")

#Figure showing the average PABAK for different features
pabak_claudeonly_cot <- pabak_claude_cot %>% 
  filter(!comparison %in% c("Rater 1-Rater 2", "Rater 1-Rater 3", 
                            "Rater 2-Rater 3"))



PABAK_byfeature_cot <- pabak_claudeonly_cot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  ggplot(aes(x= reorder(feature, -mean_pabak), y = mean_pabak)) + geom_col(fill="steelblue") + 
  theme_minimal() +   theme(
    plot.title = element_text(hjust = 0.5, size=12, face="bold", color="black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs (title = "Claude-3-7 Sonnet Few Shot Prompt with CoT",
        x = "", 
        y = "")


pabak_claudeonly_shot <- pabak_claude_shot %>% 
  filter(!comparison %in% c("Rater 1-Rater 2", "Rater 1-Rater 3", 
                            "Rater 2-Rater 3"))

PABAK_byfeature_shot <-pabak_claudeonly_shot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  ggplot(aes(x= reorder(feature, -mean_pabak), y = mean_pabak)) + geom_col(fill="steelblue") + 
  theme_minimal() +   theme(
    plot.title = element_text(hjust = 0.5, size=12, face="bold", color="black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs (title = "Claude-3-7 Sonnet Few Shot Prompt",
        x = "", 
        y = "")

pabak_claudeonly_sonnet <- pabak_claude_sonnet %>% 
  filter(!comparison %in% c("Rater 1-Rater 2", "Rater 1-Rater 3", 
                            "Rater 2-Rater 3"))

PABAK_byfeature_sonnet <- pabak_claudeonly_sonnet %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  ggplot(aes(x= reorder(feature, -mean_pabak), y = mean_pabak)) + geom_col(fill="steelblue") + 
  theme_minimal() +   theme(
    plot.title = element_text(hjust = 0.5, size=12, face="bold", color="black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs (title = "Claude-3-7 Sonnet",
        x = "", 
        y = "")

pabak_claudeonly_opus <- pabak_claude_opus %>% 
  filter(!comparison %in% c("Rater 1-Rater 2", "Rater 1-Rater 3", 
                            "Rater 2-Rater 3"))


PABAK_byfeature_opus <- pabak_claudeonly_opus %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  ggplot(aes(x= reorder(feature, -mean_pabak), y = mean_pabak)) + geom_col(fill="steelblue") + 
  theme_minimal() +   theme(
    plot.title = element_text(hjust = 0.5, size=12, face="bold", color="black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs (title = "Claude-3-Opus",
        x = "Feature", 
        y = "Mean PABAK")


  
PABAK_byfeature_opus + PABAK_byfeature_sonnet + PABAK_byfeature_shot + PABAK_byfeature_cot + 
  plot_annotation(tag_levels = 'A')


#Finding features that are consistently either very high or very low 
high_pabak_cot <- pabak_claudeonly_cot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>%
  filter(mean_pabak >= 0.8)

high_pabak_shot <- pabak_claudeonly_shot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>%
  filter(mean_pabak >= 0.8)

average_pabak_shot <- pabak_claudeonly_shot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE))

high_pabak_sonnet <- pabak_claudeonly_sonnet %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>%
  filter(mean_pabak >= 0.8)

average_pabak_sonnet <- pabak_claudeonly_sonnet %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE))

high_pabak_opus <- pabak_claudeonly_opus %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>%
  filter(mean_pabak >= 0.8)

average_pabak_opus <- pabak_claudeonly_opus %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE))

high_features <- Reduce(intersect, list(
  high_pabak_cot$feature, 
  high_pabak_shot$feature, 
  high_pabak_sonnet$feature, 
  high_pabak_opus$feature
  
))
print(high_features)

low_pabak_cot <-pabak_claudeonly_cot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  filter(mean_pabak <= 0.20)

low_pabak_shot <-pabak_claudeonly_shot %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  filter(mean_pabak <= 0.20)


low_pabak_sonnet <-pabak_claudeonly_sonnet %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  filter(mean_pabak <= 0.20)



low_pabak_opus <-pabak_claudeonly_opus %>% 
  group_by(feature) %>% 
  summarize(mean_pabak = mean(pabak, na.rm=TRUE)) %>% 
  filter(mean_pabak <= 0.20)

low_features <- Reduce(intersect, list(
  low_pabak_cot$feature, 
  low_pabak_shot$feature, 
  low_pabak_sonnet$feature, 
  low_pabak_opus$feature
))

