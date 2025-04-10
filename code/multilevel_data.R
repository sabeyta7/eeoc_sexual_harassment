##================================================================##
## Looking at the migrant youth information via a spatial network ##
##================================================================##

# Import libraries
library("dplyr")
library('tidyverse')
library('tidyr')
library('ggplot2')
library ('logistf')
library("broom")
library("dplyr")
library("knitr")
library("kableExtra")
library("stargazer")
library("gridExtra")
library("grid")
library("ggplot2")
library("sf")
library("INLA")
library("spdep")
library("lme4")
library("CCA")
library("FactoMineR")
library("cluster")
library("mice")
library("fastDummies")
library("clustMixType")
library("fossil")
library("nnet")
library("mitools")
library("brms")
library("emmeans")
library("survey")

# Setting directories

# Get the current working directory
current_dir <- getwd()

# Construct the path to the data folder
data_folder <- file.path(current_dir, "Data")

# Pullin in merged_sh_labor.csv
merged_sh_labor <- read.csv(file.path(data_folder, "merged_sh_labor.csv"))

# Looking at missing data in the demographic data, specifically CP_SEX, CP_RACE_STRING, age, number of employees ordered
# For race, we need to look at blank string
merged_sh_labor$CP_RACE_STRING[merged_sh_labor$CP_RACE_STRING == ""] <- NA
merged_sh_labor$CP_SEX[merged_sh_labor$CP_SEX == ""] <- NA
print("Missing data in the demographic data:")
print(colSums(is.na(merged_sh_labor[, c("CP_SEX", "CP_RACE_STRING", "Age", "R_NUMBER_OF_EMPLOYEES_ORDERED")])))

# Convert to ordered factor first
merged_sh_labor$R_NUMBER_OF_EMPLOYEES_ORDERED <- factor(merged_sh_labor$R_NUMBER_OF_EMPLOYEES_ORDERED, 
                                                      levels = 1:6,
                                                      ordered = TRUE)


# Create the race mapping function
race_mapping <- function(x) {
  # Handle NA values first
  if (is.na(x)) return(NA)
  
  # Split the string in case there are multiple values
  values <- strsplit(x, "")[[1]]
  
  # Sort the values to ensure consistent matching
  values <- sort(values)
  
  # Define the mapping conditions
  if (all(values %in% c("A", "S")) && length(values) == 1) return("Asian")
  if (all(values %in% c("B")) && length(values) == 1) return("Black")
  if (all(values %in% c("W")) && length(values) == 1) return("White")
  if (all(values %in% c("Z", "N")) && length(values) == 1) return("Unknown")
  
  # Check for multiple values (Biracial)
  if (length(values) > 1) return("Biracial")
  
  # Default case
  return("Other")
}

# Create ordered race variable
merged_sh_labor$CP_RACE_ORDERED <- factor(
  sapply(merged_sh_labor$CP_RACE_STRING, race_mapping),
  levels = c("White", "Black", "Asian", "Biracial", "Other", "Unknown"),
  ordered = TRUE
)

# Check the results
table(merged_sh_labor$CP_RACE_ORDERED, useNA = "always")

# Create the gender mapping function - now preserving true NAs
gender_mapping <- function(x) {
  # Handle NA values first - return NA instead of "Unknown"
  if (is.na(x)) return(NA)
  
  # Convert to character and trim whitespace to be safe
  x <- trimws(as.character(x))
  
  # Define the mapping conditions
  if (x == "Male") return("Male")
  if (x == "Female") return("Female")
  if (x == "CP Sex Not Available/Applicable") return("Unknown")
  
  # Default case
  return("Unknown")
}

# Create ordered gender variable
merged_sh_labor$SEX_ORDERED <- factor(
  sapply(merged_sh_labor$CP_SEX, gender_mapping),
  levels = c("Male", "Female", "Unknown"),
  ordered = TRUE
)

# Check the results
table(merged_sh_labor$SEX_ORDERED, useNA = "always")

# Seeing a version of the data that is just the employees ordered, age, race and sex so see complete cases numbers

reduced_data <- merged_sh_labor[, c("R_NUMBER_OF_EMPLOYEES_ORDERED", "Age", "SEX_ORDERED", "CP_RACE_ORDERED")]

# Getting rid of NAs
complete_data <- na.omit(reduced_data) #~ 16% of the data is missing


##------MICE imputation------##

# Create imputation model
imputation_vars <- c("SEX_ORDERED", "CP_RACE_ORDERED", "Age", "R_NUMBER_OF_EMPLOYEES_ORDERED")

# Create the mice imputation model with proper methods for all variables
imp_model <- mice(merged_sh_labor[,imputation_vars],
                 method = c("polr",      # for SEX_ORDERED 
                          "polr",       # for CP_RACE_ORDERED
                          "pmm",        # for Age (predictive mean matching)
                          "polr"),      # for R_NUMBER_OF_EMPLOYEES_ORDERED (since it's ordered)
                 m = 5,
                 maxit = 10)

# Consider adding diagnostic plots
plot(imp_model)
densityplot(imp_model)

# Get all imputed datasets
imputed_datasets <- complete(imp_model, "all")

# Keep all variables from the original dataset
imputed_datasets_all_vars <- lapply(imputed_datasets, 
                                   function(x) {
                                     x <- cbind(merged_sh_labor[, !names(merged_sh_labor) %in% imputation_vars], x)
                                     return(x)
                                   })



# Function to properly create dummy variables
create_dummies <- function(data) {
    # Create dummies for sex
    sex_dummies <- model.matrix(~ SEX_ORDERED - 1, data)
    colnames(sex_dummies) <- c("SEX_Male", "SEX_Female", "SEX_Unknown")
    
    # Create dummies for race
    race_dummies <- model.matrix(~ CP_RACE_ORDERED - 1, data)
    colnames(race_dummies) <- c("RACE_White", "RACE_Black", "RACE_Asian", 
                               "RACE_Biracial", "RACE_Other", "RACE_Unknown")
    
    # Add dummy variables to dataset
    data_with_dummies <- as.data.frame(cbind(
        data,
        sex_dummies,
        race_dummies
    ))
    
    return(data_with_dummies)
}

# Create dummy variables for all imputed datasets
imputed_datasets_with_dummies <- lapply(imputed_datasets_all_vars, create_dummies)

# Create year dummies, assuming your year variable is called "Year"
imputed_datasets_with_dummies <- lapply(imputed_datasets_with_dummies, function(data) {
  data %>%
    mutate(Year = as.factor(Year)) %>%
    cbind(model.matrix(~ Year + 0, .)[,-1])
})




fit_interaction_models <- function(imputed_datasets) {
  models_list <- lapply(imputed_datasets, function(data) {
    # Scale variables first
    data <- data %>%
      mutate(
        R_NUMBER_OF_EMPLOYEES_ORDERED = as.numeric(as.character(R_NUMBER_OF_EMPLOYEES_ORDERED)),
        across(c(Age, WOMEN_PERCENT, BLACK_PERCENT, HISPANIC_PERCENT,
                UNION_COVERAGE_PERCENT, OSHA_VIOLATIONS, OSHA_AVG_PENALTY,
                H_MEAN, R_NUMBER_OF_EMPLOYEES_ORDERED), scale))
    
    # Get year dummy names
    year_dummies <- grep("^Year[0-9]", colnames(data), value = TRUE)
    year_terms <- paste(year_dummies, collapse = " + ")
    
    # Calculate number of parameters for maxfun
    n_params <- length(year_dummies) + 10  # rough estimate of parameters including fixed effects
    max_fun <- 10 * n_params^2
    
    # Updated control settings with higher maxfun
    ctrl <- glmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = max_fun),
                        calc.derivs = FALSE)
    
    # Try-catch to handle convergence issues
    tryCatch({
      message("Fitting models with maxfun = ", max_fun)
      
      m1 <- glmer(as.formula(paste("SEX_Female ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                  "WOMEN_PERCENT * UNION_COVERAGE_PERCENT +",
                                  "WOMEN_PERCENT * OSHA_VIOLATIONS +",
                                  "WOMEN_PERCENT * OSHA_AVG_PENALTY +",
                                  "WOMEN_PERCENT * H_MEAN +",
                                  year_terms, "+",
                                  "(1|industry_code_merged)")),
                  family = binomial,
                  data = data,
                  control = ctrl)
      
      m2 <- glmer(as.formula(paste("SEX_Female ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                  "WOMEN_PERCENT * BLACK_PERCENT +",
                                  "WOMEN_PERCENT * HISPANIC_PERCENT +",
                                  year_terms, "+",
                                  "(1|industry_code_merged)")),
                  family = binomial,
                  data = data,
                  control = ctrl)
      
      m3 <- glmer(as.formula(paste("RACE_Black ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                  "BLACK_PERCENT * UNION_COVERAGE_PERCENT +",
                                  "BLACK_PERCENT * OSHA_VIOLATIONS +",
                                  "BLACK_PERCENT * OSHA_AVG_PENALTY +",
                                  "BLACK_PERCENT * H_MEAN +",
                                  year_terms, "+",
                                  "(1|industry_code_merged)")),
                  family = binomial,
                  data = data,
                  control = ctrl)
      
      m4 <- glmer(as.formula(paste("RACE_Black ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                  "WOMEN_PERCENT * BLACK_PERCENT +",
                                  "WOMEN_PERCENT * HISPANIC_PERCENT +",
                                  year_terms, "+",
                                  "(1|industry_code_merged)")),
                  family = binomial,
                  data = data,
                  control = ctrl)
      
      list(gender_workplace = m1,
           gender_race = m2,
           race_workplace = m3,
           race_gender = m4)
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      return(NULL)
    })
  })
  
  # Remove any NULL results from failed fits
  models_list <- models_list[!sapply(models_list, is.null)]
  return(models_list)
}

generate_wage_interaction_plot <- function(pooled_results, data) {
  # Use standardized ranges for predictors
  women_seq <- seq(-2, 2, length.out=100)
  wage_levels <- c(-1, 1)
  
  # Set mean values for controls
  mean_age <- 0  # Since we standardized
  mean_employees <- 0  # Since we standardized
  
  plot_data <- expand.grid(
    WOMEN_PERCENT = women_seq,
    H_MEAN = wage_levels
  )
  
  coefs <- pooled_results$gender_workplace
  plot_data$linear_pred <- coefs$estimate[1] + 
    coefs$estimate[grep("Age", coefs$term)] * mean_age +
    coefs$estimate[grep("R_NUMBER_OF_EMPLOYEES_ORDERED", coefs$term)] * mean_employees +
    coefs$estimate[grep("^WOMEN_PERCENT$", coefs$term)] * plot_data$WOMEN_PERCENT +
    coefs$estimate[grep("^H_MEAN$", coefs$term)] * plot_data$H_MEAN +
    coefs$estimate[grep("WOMEN_PERCENT:H_MEAN", coefs$term)] * 
      plot_data$WOMEN_PERCENT * plot_data$H_MEAN
      
  plot_data$pred_prob <- plogis(plot_data$linear_pred)
  
  # Add SE calculation here if needed
  
  return(plot_data)
}


pool_interaction_results <- function(models_list) {
  get_coefs <- function(model_type) {
    # Extract coefficients
    coefs_list <- lapply(models_list, function(x) fixef(x[[model_type]]))
    
    # Number of imputations
    m <- length(coefs_list)
    
    # Calculate pooled estimates
    coef_names <- names(coefs_list[[1]])
    pooled_results <- data.frame(term = coef_names)
    
    # For each coefficient
    for(coef in coef_names) {
      # Get estimates across imputations
      estimates <- sapply(coefs_list, function(x) x[coef])
      
      # Within-imputation variance
      within_var <- mean(sapply(models_list, function(x) 
        vcov(x[[model_type]])[coef, coef]))
      
      # Between-imputation variance
      between_var <- var(estimates)
      
      # Total variance
      total_var <- within_var + between_var * (1 + 1/m)
      
      pooled_results[pooled_results$term == coef, c("estimate", "std.error")] <- 
        c(mean(estimates), sqrt(total_var))
    }
    
    # Calculate z and p values
    pooled_results$z.value <- pooled_results$estimate / pooled_results$std.error
    pooled_results$p.value <- 2 * (1 - pnorm(abs(pooled_results$z.value)))
    
    return(pooled_results)
  }
  
  return(list(
    gender_workplace = get_coefs("gender_workplace"),
    gender_race = get_coefs("gender_race"),
    race_workplace = get_coefs("race_workplace"),
    race_gender = get_coefs("race_gender")
  ))
}


# First run models across all imputed datasets
all_models <- fit_interaction_models(imputed_datasets_with_dummies)

# Pool results
pooled_results <- pool_interaction_results(all_models)

# Generate plot data for first imputed dataset (as example)
plot_data <- generate_interaction_plots(pooled_results, imputed_datasets_with_dummies[[1]])

# View results
print(pooled_results$gender_workplace)  # Gender x workplace characteristics
print(pooled_results$gender_race)       # Gender x racial composition
print(pooled_results$race_workplace)    # Race x workplace characteristics
print(pooled_results$race_gender)


# Function to create plot data for Women% x H_MEAN interaction
generate_wage_interaction_plot <- function(pooled_results) {
  women_seq <- seq(0, 1, length.out=100)
  wage_levels <- c(-1, 1)  # Standardized low/high wages
  
  plot_data <- expand.grid(
    WOMEN_PERCENT = women_seq,
    H_MEAN = wage_levels
  )
  
  # Get coefficients
  coefs <- pooled_results$gender_workplace
  beta_0 <- coefs$estimate[coefs$term == "(Intercept)"]
  beta_women <- coefs$estimate[coefs$term == "WOMEN_PERCENT"]
  beta_wage <- coefs$estimate[coefs$term == "H_MEAN"]
  beta_int <- coefs$estimate[coefs$term == "WOMEN_PERCENT:H_MEAN"]
  
  # Calculate predicted probabilities
  plot_data$pred_prob <- plogis(
    beta_0 + 
    beta_women * plot_data$WOMEN_PERCENT +
    beta_wage * plot_data$H_MEAN +
    beta_int * plot_data$WOMEN_PERCENT * plot_data$H_MEAN
  )
  
  return(plot_data)
}

# Create plot data
wage_plot_data <- generate_wage_interaction_plot(pooled_results)

# Add confidence intervals to plot data
generate_wage_interaction_plot <- function(pooled_results) {
 women_seq <- seq(0, 1, length.out=100)
 wage_levels <- c(-1, 1)
 
 plot_data <- expand.grid(
   WOMEN_PERCENT = women_seq,
   H_MEAN = wage_levels
 )
 
 # Get coefficients and SEs
 coefs <- pooled_results$gender_workplace
 beta_0 <- coefs$estimate[coefs$term == "(Intercept)"]
 beta_women <- coefs$estimate[coefs$term == "WOMEN_PERCENT"]
 beta_wage <- coefs$estimate[coefs$term == "H_MEAN"]
 beta_int <- coefs$estimate[coefs$term == "WOMEN_PERCENT:H_MEAN"]
 
 se_0 <- coefs$std.error[coefs$term == "(Intercept)"]
 se_women <- coefs$std.error[coefs$term == "WOMEN_PERCENT"]
 se_wage <- coefs$std.error[coefs$term == "H_MEAN"]
 se_int <- coefs$std.error[coefs$term == "WOMEN_PERCENT:H_MEAN"]
 
 # Calculate linear predictor and SE
 plot_data$linear_pred <- beta_0 + 
   beta_women * plot_data$WOMEN_PERCENT +
   beta_wage * plot_data$H_MEAN +
   beta_int * plot_data$WOMEN_PERCENT * plot_data$H_MEAN
 
 plot_data$se <- sqrt(
   se_0^2 + 
   (plot_data$WOMEN_PERCENT^2 * se_women^2) +
   (plot_data$H_MEAN^2 * se_wage^2) +
   (plot_data$WOMEN_PERCENT^2 * plot_data$H_MEAN^2 * se_int^2)
 )
 
 # Calculate predicted probabilities and CIs
 plot_data$pred_prob <- plogis(plot_data$linear_pred)
 plot_data$lower_ci <- plogis(plot_data$linear_pred - 1.96 * plot_data$se)
 plot_data$upper_ci <- plogis(plot_data$linear_pred + 1.96 * plot_data$se)
 
 return(plot_data)
}

wage_plot_data <- generate_wage_interaction_plot(pooled_results)

ggplot(wage_plot_data, aes(x = WOMEN_PERCENT, y = pred_prob, color = factor(H_MEAN), fill = factor(H_MEAN))) +
 geom_line() +
 geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
 scale_color_manual(values = c("#8884d8", "#82ca9d"),
                   labels = c("Low Wage", "High Wage"),
                   name = "Industry Wage Level") +
 scale_fill_manual(values = c("#8884d8", "#82ca9d"),
                   labels = c("Low Wage", "High Wage"),
                   name = "Industry Wage Level") +
 labs(x = "Women %",
      y = "Predicted Probability Female Claimant",
      title = "Women % × Industry Mean Wage Interaction") +
 theme_minimal()


plot_model_results <- function(pooled_results, significance_level = 0.05) {
  # Function to plot a single interaction
  plot_interaction <- function(pred1, pred2, coefs, interaction_term) {
    pred1_seq <- seq(-2, 2, length.out=100)
    pred2_levels <- c(-1, 1)
    
    plot_data <- expand.grid(
      pred1 = pred1_seq,
      pred2 = pred2_levels
    )
    
    # Get coefficients and SEs
    beta_0 <- coefs$estimate[coefs$term == "(Intercept)"]
    beta_1 <- coefs$estimate[coefs$term == pred1]
    beta_2 <- coefs$estimate[coefs$term == pred2]
    beta_int <- coefs$estimate[coefs$term == interaction_term]
    
    se_0 <- coefs$std.error[coefs$term == "(Intercept)"]
    se_1 <- coefs$std.error[coefs$term == pred1]
    se_2 <- coefs$std.error[coefs$term == pred2]
    se_int <- coefs$std.error[coefs$term == interaction_term]
    
    # Calculate predictions and CIs
    plot_data$linear_pred <- beta_0 + 
      beta_1 * plot_data$pred1 +
      beta_2 * plot_data$pred2 +
      beta_int * plot_data$pred1 * plot_data$pred2
    
    plot_data$se <- sqrt(
      se_0^2 + 
      (plot_data$pred1^2 * se_1^2) +
      (plot_data$pred2^2 * se_2^2) +
      (plot_data$pred1^2 * plot_data$pred2^2 * se_int^2)
    )
    
    plot_data$pred_prob <- plogis(plot_data$linear_pred)
    plot_data$lower_ci <- plogis(plot_data$linear_pred - 1.96 * plot_data$se)
    plot_data$upper_ci <- plogis(plot_data$linear_pred + 1.96 * plot_data$se)
    
    ggplot(plot_data, aes(x = pred1, y = pred_prob, color = factor(pred2), fill = factor(pred2))) +
      geom_line() +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
      scale_color_manual(values = c("#8884d8", "#82ca9d"),
                        labels = c("Low", "High"),
                        name = paste(pred2)) +
      scale_fill_manual(values = c("#8884d8", "#82ca9d"),
                        labels = c("Low", "High"),
                        name = paste(pred2)) +
      labs(x = pred1,
           y = "Predicted Probability",
           title = paste(pred1, "×", pred2, "Interaction")) +
      theme_minimal()
  }
  
  # For each model, find significant interactions and plot them
  plot_list <- list()
  
  # Define interaction terms to check
  interactions <- list(
    gender_workplace = c("WOMEN_PERCENT:UNION_COVERAGE_PERCENT", 
                        "WOMEN_PERCENT:OSHA_VIOLATIONS",
                        "WOMEN_PERCENT:OSHA_AVG_PENALTY",
                        "WOMEN_PERCENT:H_MEAN"),
    gender_race = c("WOMEN_PERCENT:BLACK_PERCENT",
                   "WOMEN_PERCENT:HISPANIC_PERCENT"),
    race_workplace = c("BLACK_PERCENT:UNION_COVERAGE_PERCENT",
                      "BLACK_PERCENT:OSHA_VIOLATIONS",
                      "BLACK_PERCENT:OSHA_AVG_PENALTY",
                      "BLACK_PERCENT:H_MEAN"),
    race_gender = c("WOMEN_PERCENT:BLACK_PERCENT",
                   "WOMEN_PERCENT:HISPANIC_PERCENT")
  )
  
  for(model_name in names(pooled_results)) {
    model_results <- pooled_results[[model_name]]
    
    # Check each interaction
    for(int_term in interactions[[model_name]]) {
      if(model_results$p.value[model_results$term == int_term] < significance_level) {
        # Extract predictor names
        preds <- strsplit(int_term, ":")[[1]]
        
        # Create plot
        plot_list[[paste(model_name, int_term)]] <- 
          plot_interaction(preds[1], preds[2], model_results, int_term)
      }
    }
    
    # Add forest plot of fixed effects
    forest_data <- model_results %>%
      mutate(
        lower_ci = estimate - 1.96 * std.error,
        upper_ci = estimate + 1.96 * std.error
      )
    
    plot_list[[paste0(model_name, "_forest")]] <- 
      ggplot(forest_data, aes(y = term, x = estimate)) +
      geom_point() +
      geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(title = paste(model_name, "Fixed Effects"),
           x = "Estimate",
           y = "") +
      theme_minimal()
  }
  
  # Return list of plots
  return(plot_list)
}

plot_random_effects <- function(models_list) {
  # Function to extract and process random effects for one model type
  process_ranef <- function(model_name) {
    # Get random effects from each imputed dataset
    ranef_list <- lapply(models_list, function(x) {
      re <- ranef(x[[model_name]])$industry_code_merged[,1]
      data.frame(
        industry = rownames(ranef(x[[model_name]])$industry_code_merged),
        effect = re
      )
    })
    
    # Combine all datasets and calculate mean and SE
    all_re <- bind_rows(ranef_list, .id = "imputation")
    
    summarized_re <- all_re %>%
      group_by(industry) %>%
      summarize(
        estimate = mean(effect),
        se = sd(effect),
        lower = estimate - 1.96 * se,
        upper = estimate + 1.96 * se
      )
    
    # Create plot
    ggplot(summarized_re, aes(y = reorder(industry, estimate), x = estimate)) +
      geom_point() +
      geom_errorbarh(aes(xmin = lower, xmax = upper)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(title = paste(model_name, "Random Effects"),
           x = "Random Effect Estimate",
           y = "Industry") +
      theme_minimal()
  }
  
  # Create plots for each model type
  plots <- list(
    gender_workplace = process_ranef("gender_workplace"),
    gender_race = process_ranef("gender_race"),
    race_workplace = process_ranef("race_workplace"),
    race_gender = process_ranef("race_gender")
  )
  
  return(plots)
}

# Create the plots
re_plots <- plot_random_effects(all_models)

# View individual plots
re_plots$gender_workplace  # or any other model type


# Get fixed effects and significant interaction plots
fixed_plots <- plot_model_results(pooled_results)

# Get random effects plots 
random_plots <- plot_random_effects(all_models)

# Combine all plots into one list
all_plots <- c(fixed_plots, random_plots)

# Now you can view:
# Fixed effects forest plots:
all_plots$gender_workplace_forest
all_plots$gender_race_forest
all_plots$race_workplace_forest
all_plots$race_gender_forest

# Random effects plots:
all_plots$gender_workplace  # random effects
all_plots$gender_race
all_plots$race_workplace
all_plots$race_gender

# Significant interaction plots will be named like:
# "{model_name} {interaction_term}"
# e.g., "gender_workplace WOMEN_PERCENT:H_MEAN"
# They'll only appear if they were significant at p < 0.05

# To arrange multiple plots together:
library(patchwork)
# Example: Arrange gender_workplace plots
wrap_plots(
  all_plots$gender_workplace_forest,
  all_plots$gender_workplace,
  ncol = 2
)


# Only plotting statistically significant interactions (p < 0.05)
wrap_plots(
  fixed_plots[["race_workplace BLACK_PERCENT:OSHA_AVG_PENALTY"]],  # p = 0.014
  fixed_plots[["race_workplace BLACK_PERCENT:H_MEAN"]],            # p = 0.017
  fixed_plots[["race_gender WOMEN_PERCENT:BLACK_PERCENT"]],        # p = 0.0008
  ncol = 2
)

fit_combined_models <- function(imputed_datasets) {
  models_list <- lapply(imputed_datasets, function(data) {
    # Scale variables first
    data <- data %>%
      mutate(
        R_NUMBER_OF_EMPLOYEES_ORDERED = as.numeric(as.character(R_NUMBER_OF_EMPLOYEES_ORDERED)),
        across(c(Age, WOMEN_PERCENT, BLACK_PERCENT, HISPANIC_PERCENT,
                UNION_COVERAGE_PERCENT, OSHA_VIOLATIONS, OSHA_AVG_PENALTY,
                H_MEAN, R_NUMBER_OF_EMPLOYEES_ORDERED), scale))
    
    # Get year dummy names
    year_dummies <- grep("^Year[0-9]", colnames(data), value = TRUE)
    year_terms <- paste(year_dummies, collapse = " + ")
    
    # Calculate number of parameters for maxfun
    n_params <- length(year_dummies) + 25  # increased estimate for combined models
    max_fun <- 20 * n_params^2  # doubled multiplier for more iterations
    
    # Updated control settings with higher maxfun
    ctrl <- glmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = max_fun),
                        calc.derivs = FALSE)
    
    # Try-catch to handle convergence issues
    tryCatch({
      message("Fitting combined models with maxfun = ", max_fun)
      
      # Combined gender model
      gender_model <- glmer(as.formula(paste("SEX_Female ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                           # Workplace characteristics interactions
                                           "WOMEN_PERCENT * UNION_COVERAGE_PERCENT +",
                                           "WOMEN_PERCENT * OSHA_VIOLATIONS +",
                                           "WOMEN_PERCENT * OSHA_AVG_PENALTY +",
                                           "WOMEN_PERCENT * H_MEAN +",
                                           # Demographic composition interactions
                                           "WOMEN_PERCENT * BLACK_PERCENT +",
                                           "WOMEN_PERCENT * HISPANIC_PERCENT +",
                                           year_terms, "+",
                                           "(1|industry_code_merged)")),
                           family = binomial,
                           data = data,
                           control = ctrl)
      
      # Combined race model
      race_model <- glmer(as.formula(paste("RACE_Black ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +",
                                         # Workplace characteristics interactions
                                         "BLACK_PERCENT * UNION_COVERAGE_PERCENT +",
                                         "BLACK_PERCENT * OSHA_VIOLATIONS +",
                                         "BLACK_PERCENT * OSHA_AVG_PENALTY +",
                                         "BLACK_PERCENT * H_MEAN +",
                                         # Demographic composition interactions
                                         "WOMEN_PERCENT * BLACK_PERCENT +",
                                         "WOMEN_PERCENT * HISPANIC_PERCENT +",
                                         year_terms, "+",
                                         "(1|industry_code_merged)")),
                         family = binomial,
                         data = data,
                         control = ctrl)
      
      list(gender = gender_model,
           race = race_model)
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      return(NULL)
    })
  })
  
  # Remove any NULL results from failed fits
  models_list <- models_list[!sapply(models_list, is.null)]
  return(models_list)
}

pool_combined_results <- function(models_list) {
  get_coefs <- function(model_type) {
    # Extract coefficients
    coefs_list <- lapply(models_list, function(x) fixef(x[[model_type]]))
    
    # Number of imputations
    m <- length(coefs_list)
    
    # Calculate pooled estimates
    coef_names <- names(coefs_list[[1]])
    pooled_results <- data.frame(term = coef_names)
    
    # For each coefficient
    for(coef in coef_names) {
      # Get estimates across imputations
      estimates <- sapply(coefs_list, function(x) x[coef])
      
      # Within-imputation variance
      within_var <- mean(sapply(models_list, function(x) 
        vcov(x[[model_type]])[coef, coef]))
      
      # Between-imputation variance
      between_var <- var(estimates)
      
      # Total variance
      total_var <- within_var + between_var * (1 + 1/m)
      
      pooled_results[pooled_results$term == coef, c("estimate", "std.error")] <- 
        c(mean(estimates), sqrt(total_var))
    }
    
    # Calculate z and p values
    pooled_results$z.value <- pooled_results$estimate / pooled_results$std.error
    pooled_results$p.value <- 2 * (1 - pnorm(abs(pooled_results$z.value)))
    
    return(pooled_results)
  }
  
  return(list(
    gender = get_coefs("gender"),
    race = get_coefs("race")
  ))
}

# Example usage:
all_models <- fit_combined_models(imputed_datasets_with_dummies)
pooled_results <- pool_combined_results(all_models)

# View results
print(pooled_results$gender)  # Combined gender model results
print(pooled_results$race)    # Combined race model results


plot_significant_interactions <- function(pooled_results) {
  # Function to plot a single interaction
  plot_interaction <- function(pred1, pred2, coefs, model_type) {
    # Create sequence for predictor 1
    pred1_seq <- seq(-2, 2, length.out=100)
    # Use -1 and 1 for high/low levels of predictor 2
    pred2_levels <- c(-1, 1)
    
    # Create grid of predictor values
    plot_data <- expand.grid(
      pred1 = pred1_seq,
      pred2 = pred2_levels
    )
    
    # Get base coefficients
    beta_0 <- coefs$estimate[coefs$term == "(Intercept)"]
    beta_1 <- coefs$estimate[coefs$term == pred1]
    beta_2 <- coefs$estimate[coefs$term == pred2]
    interaction_term <- paste0(pred1, ":", pred2)
    beta_int <- coefs$estimate[coefs$term == interaction_term]
    
    # Get standard errors
    se_0 <- coefs$std.error[coefs$term == "(Intercept)"]
    se_1 <- coefs$std.error[coefs$term == pred1]
    se_2 <- coefs$std.error[coefs$term == pred2]
    se_int <- coefs$std.error[coefs$term == interaction_term]
    
    # Calculate predictions
    plot_data$linear_pred <- beta_0 + 
      beta_1 * plot_data$pred1 +
      beta_2 * plot_data$pred2 +
      beta_int * plot_data$pred1 * plot_data$pred2
    
    # Calculate standard errors for confidence intervals
    plot_data$se <- sqrt(
      se_0^2 + 
      (plot_data$pred1^2 * se_1^2) +
      (plot_data$pred2^2 * se_2^2) +
      (plot_data$pred1^2 * plot_data$pred2^2 * se_int^2)
    )
    
    # Transform to probability scale with confidence intervals
    plot_data$pred_prob <- plogis(plot_data$linear_pred)
    plot_data$lower_ci <- plogis(plot_data$linear_pred - 1.96 * plot_data$se)
    plot_data$upper_ci <- plogis(plot_data$linear_pred + 1.96 * plot_data$se)
    
    # Format predictor names for plotting
    pred1_label <- gsub("_", " ", pred1)
    pred2_label <- gsub("_", " ", pred2)
    
    # Create labels for high/low levels
    pred2_labels <- c("Low", "High")
    
    # Create plot
    p <- ggplot(plot_data, aes(x = pred1, y = pred_prob, color = factor(pred2), fill = factor(pred2))) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
      scale_color_manual(values = c("#8884d8", "#82ca9d"),
                        labels = pred2_labels,
                        name = pred2_label) +
      scale_fill_manual(values = c("#8884d8", "#82ca9d"),
                        labels = pred2_labels,
                        name = pred2_label) +
      labs(x = pred1_label,
           y = ifelse(model_type == "gender", 
                     "Predicted Probability Female Claimant",
                     "Predicted Probability Black Claimant"),
           title = paste(pred1_label, "×", pred2_label, "Interaction")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    return(p)
  }
  
  # List of significant interactions
  sig_interactions <- list(
    gender = list(
      c("WOMEN_PERCENT", "UNION_COVERAGE_PERCENT"),
      c("WOMEN_PERCENT", "H_MEAN"),
      c("WOMEN_PERCENT", "BLACK_PERCENT"),
      c("WOMEN_PERCENT", "HISPANIC_PERCENT")
    ),
    race = list(
      c("BLACK_PERCENT", "H_MEAN"),
      c("BLACK_PERCENT", "WOMEN_PERCENT")
    )
  )
  
  # Create plots for each significant interaction
  plots <- list()
  
  # Gender model interactions
  for(int in sig_interactions$gender) {
    plot_name <- paste("gender", int[1], int[2], sep="_")
    plots[[plot_name]] <- plot_interaction(int[1], int[2], 
                                         pooled_results$gender, 
                                         "gender")
  }
  
  # Race model interactions
  for(int in sig_interactions$race) {
    plot_name <- paste("race", int[1], int[2], sep="_")
    plots[[plot_name]] <- plot_interaction(int[1], int[2], 
                                         pooled_results$race, 
                                         "race")
  }
  
  return(plots)
}

# Example usage:
interaction_plots <- plot_significant_interactions(pooled_results)
# 
# # View individual plots:
interaction_plots$gender_WOMEN_PERCENT_UNION_COVERAGE_PERCENT
interaction_plots$gender_WOMEN_PERCENT_H_MEAN
interaction_plots$gender_WOMEN_PERCENT_BLACK_PERCENT
interaction_plots$gender_WOMEN_PERCENT_HISPANIC_PERCENT
interaction_plots$race_BLACK_PERCENT_H_MEAN
interaction_plots$race_BLACK_PERCENT_WOMEN_PERCENT
#
# # Or arrange all plots together using patchwork:
library(patchwork)
wrap_plots(interaction_plots, ncol = 3)



fit_linear_year_models <- function(imputed_datasets) {
  models_list <- lapply(imputed_datasets, function(data) {
    # Basic data prep - ensure YEAR is numeric first
    data <- data %>%
      mutate(
        YEAR = as.numeric(as.character(YEAR)),  # Ensure YEAR is numeric
        year_centered = YEAR - 1996,  # Center at 1996 (first year)
        industry_id = factor(industry_code_merged),
        R_NUMBER_OF_EMPLOYEES_ORDERED = as.numeric(as.character(R_NUMBER_OF_EMPLOYEES_ORDERED)),
        across(c(Age, WOMEN_PERCENT, BLACK_PERCENT, HISPANIC_PERCENT,
                UNION_COVERAGE_PERCENT, OSHA_VIOLATIONS, OSHA_AVG_PENALTY,
                H_MEAN, R_NUMBER_OF_EMPLOYEES_ORDERED), scale)
      )
    
    # Print first few rows to verify
    print("First few rows of transformed data:")
    print(head(data[c("YEAR", "year_centered", "industry_id")]))
    
    # Control settings
    ctrl <- glmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 20000),
                        calc.derivs = FALSE)
    
    tryCatch({
      # Gender model with linear year
      gender_model <- glmer(SEX_Female ~ year_centered +
                            R_NUMBER_OF_EMPLOYEES_ORDERED + Age +
                            WOMEN_PERCENT * UNION_COVERAGE_PERCENT +
                            WOMEN_PERCENT * OSHA_VIOLATIONS +
                            WOMEN_PERCENT * OSHA_AVG_PENALTY +
                            WOMEN_PERCENT * H_MEAN +
                            WOMEN_PERCENT * BLACK_PERCENT +
                            WOMEN_PERCENT * HISPANIC_PERCENT +
                            (1|industry_id),
                          family = binomial,
                          data = data,
                          control = ctrl)
      
      # Race model with linear year
      race_model <- glmer(RACE_Black ~ year_centered +
                           R_NUMBER_OF_EMPLOYEES_ORDERED + Age +
                           BLACK_PERCENT * UNION_COVERAGE_PERCENT +
                           BLACK_PERCENT * OSHA_VIOLATIONS +
                           BLACK_PERCENT * OSHA_AVG_PENALTY +
                           BLACK_PERCENT * H_MEAN +
                           BLACK_PERCENT * WOMEN_PERCENT +
                           WOMEN_PERCENT * HISPANIC_PERCENT +
                           (1|industry_id),
                         family = binomial,
                         data = data,
                         control = ctrl)
      
      list(gender = gender_model,
           race = race_model)
    }, error = function(e) {
      message("Error in model fitting: ", e$message)
      return(NULL)
    })
  })
  
  # Remove any NULL results
  models_list <- models_list[!sapply(models_list, is.null)]
  return(models_list)
}

# Pool results function remains the same
pool_linear_year_results <- function(models_list) {
  if (length(models_list) == 0) {
    stop("No successfully fitted models to pool")
  }
  
  get_coefs <- function(model_type) {
    coefs_list <- lapply(models_list, function(x) fixef(x[[model_type]]))
    m <- length(coefs_list)
    coef_names <- names(coefs_list[[1]])
    pooled_results <- data.frame(term = coef_names)
    
    for(coef in coef_names) {
      estimates <- sapply(coefs_list, function(x) x[coef])
      within_var <- mean(sapply(models_list, function(x) 
        vcov(x[[model_type]])[coef, coef]))
      between_var <- var(estimates)
      total_var <- within_var + between_var * (1 + 1/m)
      
      pooled_results[pooled_results$term == coef, c("estimate", "std.error")] <- 
        c(mean(estimates), sqrt(total_var))
    }
    
    pooled_results$z.value <- pooled_results$estimate / pooled_results$std.error
    pooled_results$p.value <- 2 * (1 - pnorm(abs(pooled_results$z.value)))
    
    return(pooled_results)
  }
  
  return(list(
    gender = get_coefs("gender"),
    race = get_coefs("race")
  ))
}


linear_year_models <- fit_linear_year_models(imputed_datasets_with_dummies)
linear_year_results <- pool_linear_year_results(linear_year_models)

linear_year_results$gender
linear_year_results$race


compare_year_effects <- function(fixed_results, linear_results) {
  # Extract year coefficients from fixed effects model
  year_effects <- data.frame(
    year = as.numeric(gsub("Year", "", 
                          grep("^Year", fixed_results$gender$term, value = TRUE))),
    gender_effect = fixed_results$gender$estimate[grep("^Year", fixed_results$gender$term)],
    gender_se = fixed_results$gender$std.error[grep("^Year", fixed_results$gender$term)],
    race_effect = fixed_results$race$estimate[grep("^Year", fixed_results$race$term)],
    race_se = fixed_results$race$std.error[grep("^Year", fixed_results$race$term)]
  )
  
  # Add prediction from linear model
  year_seq <- year_effects$year - min(year_effects$year)  # Center at first year
  
  # Get linear coefficients
  gender_linear_coef <- linear_results$gender$estimate[linear_results$gender$term == "year_centered"]
  race_linear_coef <- linear_results$race$estimate[linear_results$race$term == "year_centered"]
  
  gender_linear_se <- linear_results$gender$std.error[linear_results$gender$term == "year_centered"]
  race_linear_se <- linear_results$race$std.error[linear_results$race$term == "year_centered"]
  
  # Add linear predictions to data
  year_effects$gender_linear <- gender_linear_coef * year_seq
  year_effects$race_linear <- race_linear_coef * year_seq
  
  # Create plots
  gender_plot <- ggplot(year_effects, aes(x = year)) +
    geom_point(aes(y = gender_effect)) +
    geom_errorbar(aes(ymin = gender_effect - 1.96*gender_se,
                     ymax = gender_effect + 1.96*gender_se)) +
    geom_line(aes(y = gender_linear, color = "Linear Trend")) +
    geom_ribbon(aes(ymin = gender_linear - 1.96*gender_linear_se * year_seq,
                    ymax = gender_linear + 1.96*gender_linear_se * year_seq,
                    fill = "Linear Trend"), alpha = 0.2) +
    labs(title = "Year Effects Comparison - Gender Model",
         x = "Year",
         y = "Effect Size",
         color = "Model Type",
         fill = "Model Type") +
    theme_minimal()
  
  race_plot <- ggplot(year_effects, aes(x = year)) +
    geom_point(aes(y = race_effect)) +
    geom_errorbar(aes(ymin = race_effect - 1.96*race_se,
                     ymax = race_effect + 1.96*race_se)) +
    geom_line(aes(y = race_linear, color = "Linear Trend")) +
    geom_ribbon(aes(ymin = race_linear - 1.96*race_linear_se * year_seq,
                    ymax = race_linear + 1.96*race_linear_se * year_seq,
                    fill = "Linear Trend"), alpha = 0.2) +
    labs(title = "Year Effects Comparison - Race Model",
         x = "Year",
         y = "Effect Size",
         color = "Model Type",
         fill = "Model Type") +
    theme_minimal()
  
  # Calculate R-squared for how well linear trend fits fixed effects
  gender_rsq <- 1 - sum((year_effects$gender_effect - year_effects$gender_linear)^2) / 
    sum((year_effects$gender_effect - mean(year_effects$gender_effect))^2)
  
  race_rsq <- 1 - sum((year_effects$race_effect - year_effects$race_linear)^2) / 
    sum((year_effects$race_effect - mean(year_effects$race_effect))^2)
  
  # Check for non-linearity using polynomial fit
  gender_poly <- lm(gender_effect ~ poly(year, 2), data = year_effects)
  race_poly <- lm(race_effect ~ poly(year, 2), data = year_effects)
  
  # F-test for non-linearity
  gender_nonlin <- anova(
    lm(gender_effect ~ year, data = year_effects),
    gender_poly
  )
  
  race_nonlin <- anova(
    lm(race_effect ~ year, data = year_effects),
    race_poly
  )
  
  # Return results
  return(list(
    plots = list(gender = gender_plot, race = race_plot),
    fit_stats = list(
      gender_rsq = gender_rsq,
      race_rsq = race_rsq,
      gender_nonlinearity_p = gender_nonlin$`Pr(>F)`[2],
      race_nonlinearity_p = race_nonlin$`Pr(>F)`[2]
    )
  ))
}

# Example usage:
comparison <- compare_year_effects(pooled_results, linear_year_results)
# 
# # View plots
comparison$plots$gender
comparison$plots$race
# 
# # View fit statistics
print(comparison$fit_stats)




















library(ggplot2)
library(dplyr)

# 1. Single plot with facets by model
plot_all_effects <- function(models) {
  # Combine all models into one dataset
  all_effects <- bind_rows(
    lapply(names(models), function(model_name) {
      models[[model_name]] %>%
        filter(p.value < 0.05,
               !grepl("Year", term),
               term != "(Intercept)",
               !grepl(":", term)) %>%
        mutate(model = model_name)
    })
  )
  
  ggplot(all_effects, aes(x = reorder(term, estimate), y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                     ymax = estimate + 1.96*std.error), 
                 width = 0.2) +
    facet_wrap(~model) +
    coord_flip() +
    theme_minimal() +
    labs(x = "", y = "Effect Size")
}

# 2. Single combined plot showing all effects
plot_combined_effects <- function(models) {
  all_effects <- bind_rows(
    lapply(names(models), function(model_name) {
      models[[model_name]] %>%
        filter(p.value < 0.05,
               !grepl("Year", term),
               term != "(Intercept)",
               !grepl(":", term)) %>%
        mutate(model = model_name)
    })
  )
  
  ggplot(all_effects, aes(x = reorder(term, estimate), y = estimate, color = model)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                     ymax = estimate + 1.96*std.error),
                 position = position_dodge(width = 0.5),
                 width = 0.2) +
    coord_flip() +
    theme_minimal() +
    labs(x = "", y = "Effect Size", color = "Model")
}

# 3. Or even simpler, just a clean table
library(kableExtra)
make_effects_table <- function(models) {
  all_effects <- bind_rows(
    lapply(names(models), function(model_name) {
      models[[model_name]] %>%
        filter(p.value < 0.05,
               !grepl("Year", term),
               term != "(Intercept)",
               !grepl(":", term)) %>%
        mutate(model = model_name) %>%
        dplyr::select(model, term, estimate, std.error, p.value)
    })
  ) %>%
    arrange(model, desc(abs(estimate)))
  
  kable(all_effects, digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

# For faceted plot
plot_all_effects(models)

# For combined plot
plot_combined_effects(models)

# For nice table
make_effects_table(models)


library(brms)
library(tidyverse)
library(tidybayes)

library(brms)
library(cmdstanr)

# Set cmdstanr as the backend
set_cmdstan_path()
options(brms.backend = "cmdstanr")

# Function to fit models on multiple imputed datasets
fit_bayesian_pooled_models <- function(imputed_datasets) {
  # Fit models for each imputed dataset
  models_list <- lapply(imputed_datasets, function(data) {
    data <- data %>%
      mutate(
        R_NUMBER_OF_EMPLOYEES_ORDERED = as.numeric(as.character(R_NUMBER_OF_EMPLOYEES_ORDERED)),
        across(c(Age, WOMEN_PERCENT, BLACK_PERCENT, HISPANIC_PERCENT,
                UNION_COVERAGE_PERCENT, OSHA_VIOLATIONS, OSHA_AVG_PENALTY,
                H_MEAN, R_NUMBER_OF_EMPLOYEES_ORDERED), scale)
      )
    
    priors <- c(
      prior(normal(0, 2), class = "b"),
      prior(cauchy(0, 1), class = "sd")
    )
    
    m1 <- brm(
      SEX_Female ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age +
        WOMEN_PERCENT * UNION_COVERAGE_PERCENT +
        WOMEN_PERCENT * OSHA_VIOLATIONS +
        WOMEN_PERCENT * OSHA_AVG_PENALTY +
        WOMEN_PERCENT * H_MEAN +
        (1|industry_code_merged),
      family = bernoulli(),
      data = data,
      prior = priors,
      cores = 4,
      chains = 4,
      iter = 4000
    )
    
    return(m1)
  })
  
  return(models_list)
}

# Function to pool posterior distributions across imputed datasets
pool_posterior_samples <- function(models_list) {
  # Extract posterior samples from each model
  posterior_list <- lapply(models_list, function(model) {
    as_draws_df(model)
  })
  
  # Calculate pooled posterior mean and variance using Rubin's rules
  n_imp <- length(posterior_list)
  
  # Get parameter names (excluding random effects)
  param_names <- colnames(posterior_list[[1]]) %>%
    str_subset("^b_")
  
  # Initialize results dataframe
  pooled_results <- data.frame(
    parameter = param_names,
    estimate = NA,
    std.error = NA,
    conf.low = NA,
    conf.high = NA
  )
  
  # Pool for each parameter
  for (param in param_names) {
    # Extract estimates across imputations
    estimates <- lapply(posterior_list, function(x) x[[param]])
    
    # Calculate within-imputation variance (average variance across chains)
    within_var <- mean(sapply(estimates, var))
    
    # Calculate between-imputation variance
    means <- sapply(estimates, mean)
    between_var <- var(means)
    
    # Total variance following Rubin's rules
    total_var <- within_var + between_var * (1 + 1/n_imp)
    
    # Pooled estimate
    pooled_mean <- mean(means)
    
    # Calculate credible intervals
    pooled_results[pooled_results$parameter == param, c("estimate", "std.error", "conf.low", "conf.high")] <-
      c(pooled_mean, 
        sqrt(total_var),
        pooled_mean - 1.96 * sqrt(total_var),
        pooled_mean + 1.96 * sqrt(total_var))
  }
  
  return(pooled_results)
}

# Function to generate predictions using pooled results
generate_pooled_predictions <- function(models_list, predictor_range = c(-2, 2)) {
  # Create prediction grid
  women_seq <- seq(predictor_range[1], predictor_range[2], length.out = 100)
  wage_levels <- c(-1, 1)
  
  newdata <- expand_grid(
    WOMEN_PERCENT = women_seq,
    H_MEAN = wage_levels,
    Age = 0,
    R_NUMBER_OF_EMPLOYEES_ORDERED = 0,
    UNION_COVERAGE_PERCENT = 0,
    OSHA_VIOLATIONS = 0,
    OSHA_AVG_PENALTY = 0
  )
  
  # Get predictions from each model
  pred_list <- lapply(models_list, function(model) {
    fitted(model, 
          newdata = newdata,
          re_formula = NA,
          summary = FALSE)
  })
  
  # Pool predictions using Rubin's rules
  n_imp <- length(pred_list)
  n_samples <- dim(pred_list[[1]])[1]
  n_pred <- dim(pred_list[[1]])[2]
  
  # Calculate pooled predictions and intervals
  pooled_preds <- matrix(NA, n_pred, 3)  # mean, lower, upper
  
  for(i in 1:n_pred) {
    # Extract predictions for this point across all imputations
    point_preds <- sapply(pred_list, function(x) x[,i])
    
    # Within-imputation variance
    within_var <- mean(apply(point_preds, 2, var))
    
    # Between-imputation variance
    means <- apply(point_preds, 2, mean)
    between_var <- var(means)
    
    # Total variance
    total_var <- within_var + between_var * (1 + 1/n_imp)
    
    # Pooled mean
    pooled_mean <- mean(means)
    
    pooled_preds[i,] <- c(
      pooled_mean,
      pooled_mean - 1.96 * sqrt(total_var),
      pooled_mean + 1.96 * sqrt(total_var)
    )
  }
  
  # Combine with prediction grid
  plot_data <- bind_cols(
    newdata,
    data.frame(
      mean = pooled_preds[,1],
      lower = pooled_preds[,2],
      upper = pooled_preds[,3]
    )
  )
  
  return(plot_data)
}

# Function to extract and pool random effects posteriors
pool_random_effects_posterior <- function(models_list) {
  # Extract random effects from each model and calculate mean for each imputation
  re_means_list <- lapply(seq_along(models_list), function(i) {
    model <- models_list[[i]]
    
    # Get random effects parameters
    re_pars <- variables(model)[grep("r_industry_code_merged\\[.*,Intercept\\]", variables(model))]
    
    # Get random effects draws
    draws <- as_draws_df(model, variables = re_pars)
    
    # Calculate mean for each parameter in this imputation
    draws %>%
      summarize(across(everything(), mean)) %>%
      pivot_longer(everything(), 
                  names_to = "industry",
                  values_to = "mean_estimate") %>%
      mutate(
        industry = str_extract(industry, "\\[.*?,") %>%
                  str_remove("\\[") %>%
                  str_remove(","),
        imp = i
      )
  })
  
  # Get variance of draws for each imputation
  re_vars_list <- lapply(seq_along(models_list), function(i) {
    model <- models_list[[i]]
    
    re_pars <- variables(model)[grep("r_industry_code_merged\\[.*,Intercept\\]", variables(model))]
    draws <- as_draws_df(model, variables = re_pars)
    
    draws %>%
      summarize(across(everything(), var)) %>%
      pivot_longer(everything(), 
                  names_to = "industry",
                  values_to = "within_var") %>%
      mutate(
        industry = str_extract(industry, "\\[.*?,") %>%
                  str_remove("\\[") %>%
                  str_remove(","),
        imp = i
      )
  })
  
  # Combine means and calculate between-imputation variance
  re_means <- bind_rows(re_means_list)
  re_vars <- bind_rows(re_vars_list)
  
  # Calculate pooled estimates
  pooled_re <- re_means %>%
    group_by(industry) %>%
    summarize(
      # Mean estimate across imputations
      estimate = mean(mean_estimate),
      # Between-imputation variance
      between_var = var(mean_estimate),
      # Number of imputations
      m = n(),
      .groups = "drop") %>%
    # Join with within-imputation variances
    left_join(
      re_vars %>%
        group_by(industry) %>%
        summarize(within_var = mean(within_var),
                 .groups = "drop"),
      by = "industry"
    ) %>%
    # Calculate total variance and confidence intervals
    mutate(
      total_var = within_var + between_var * (1 + 1/m),
      std.error = sqrt(total_var),
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error
    ) %>%
    dplyr::select(industry, within_var, between_var, total_var, 
           estimate, std.error, lower, upper)
  
  return(pooled_re)
}

# Plot function remains the same
plot_random_effects <- function(pooled_re, top_n = 20) {
  # Select top and bottom industries by absolute effect size
  plot_data <- pooled_re %>%
    mutate(abs_effect = abs(estimate)) %>%
    arrange(desc(abs_effect)) %>%
    slice_head(n = top_n) %>%
    mutate(industry = fct_reorder(factor(industry), estimate))
  
  ggplot(plot_data, aes(y = industry, x = estimate)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    labs(x = "Random Effect Estimate (Log-Odds Scale)",
         y = "Industry Code",
         title = "Industry-Level Random Effects",
         subtitle = paste("Showing top", top_n, "industries by absolute effect size")) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}

# Example usage:
pooled_re <- pool_random_effects_posterior(all_models)
re_plot <- plot_random_effects(pooled_re)

# To see the top industries with strongest effects:
pooled_re %>%
  arrange(desc(abs(estimate))) %>%
  dplyr::select(industry, estimate, std.error, lower, upper) %>%
  head(10)



library(brms)
library(bayesplot)
library(tidyverse)
library(patchwork)

create_diagnostic_grid <- function(models_list) {
  # Create list to store plots for each diagnostic type
  nuts_plots <- list()
  trace_plots <- list()
  density_plots <- list()
  neff_plots <- list()
  
  # Generate plots for each model
  for(i in seq_along(models_list)) {
    color_scheme_set("viridis")
    model <- models_list[[i]]
    
    # NUTS plot
    nuts_plots[[i]] <- create_nuts_plot(model) +
      ggtitle(paste("NUTS Energy Diagnostic -", "Imputation", i))
    
    # Trace plot
    trace_plots[[i]] <- create_trace_plot(model) +
      ggtitle(paste("Trace Plots -", "Imputation", i))
    
    # Density plot
    density_plots[[i]] <- create_density_plot(model) +
      ggtitle(paste("Density Overlays -", "Imputation", i))
    
    # Neff plot
    neff_plots[[i]] <- create_neff_plot(model) +
      ggtitle(paste("Effective Sample Size -", "Imputation", i))
  }
  
  # Combine plots by type
  nuts_grid <- wrap_plots(nuts_plots, ncol = 2) +
    plot_annotation(title = "NUTS Energy Diagnostics Across Imputations")
  
  trace_grid <- wrap_plots(trace_plots, ncol = 2) +
    plot_annotation(title = "Trace Plots Across Imputations")
  
  density_grid <- wrap_plots(density_plots, ncol = 2) +
    plot_annotation(title = "Density Overlays Across Imputations")
  
  neff_grid <- wrap_plots(neff_plots, ncol = 2) +
    plot_annotation(title = "Effective Sample Sizes Across Imputations")
  
  return(list(
    nuts = nuts_grid,
    traces = trace_grid,
    densities = density_grid,
    neff = neff_grid
  ))
}

# Individual plot creation functions remain the same
create_nuts_plot <- function(model) {
  np <- nuts_params(model)
  mcmc_nuts_energy(np) +
    theme_minimal()
}

create_trace_plot <- function(model) {
  posterior_draws <- as_draws_df(model, subset = sample(1:8000, 2000))
  mcmc_trace(posterior_draws, 
             pars = c("b_Intercept", "b_WOMEN_PERCENT", 
                     "b_WOMEN_PERCENT:H_MEAN"),
             facet_args = list(ncol = 1)) +
    theme_minimal()
}

create_density_plot <- function(model) {
  posterior_draws <- as_draws_df(model, subset = sample(1:8000, 2000))
  mcmc_dens_overlay(posterior_draws,
                   pars = c("b_Intercept", "b_WOMEN_PERCENT", 
                           "b_WOMEN_PERCENT:H_MEAN")) +
    theme_minimal()
}

create_neff_plot <- function(model) {
  neff <- neff_ratio(model)
  mcmc_neff(neff) +
    theme_minimal()
}

# Example usage:
diagnostic_grids <- create_diagnostic_grid(all_models)

# View individual grids:
print(diagnostic_grids$nuts)      # Grid of NUTS diagnostics
print(diagnostic_grids$traces)    # Grid of trace plots
print(diagnostic_grids$densities) # Grid of density plots
print(diagnostic_grids$neff)      # Grid of effective sample size plots

library(brms)
library(bayesplot)
library(tidyverse)

create_posterior_plots <- function(models_list) {
  # Function to extract and combine posteriors from all imputations
  get_pooled_posterior <- function(models_list) {
    # Extract posteriors from each model
    posterior_list <- lapply(seq_along(models_list), function(i) {
      as_draws_df(models_list[[i]], 
                 variables = c("b_Intercept", "b_WOMEN_PERCENT",
                             "b_UNION_COVERAGE_PERCENT", "b_OSHA_VIOLATIONS",
                             "b_OSHA_AVG_PENALTY", "b_H_MEAN",
                             "b_WOMEN_PERCENT:UNION_COVERAGE_PERCENT",
                             "b_WOMEN_PERCENT:OSHA_VIOLATIONS",
                             "b_WOMEN_PERCENT:OSHA_AVG_PENALTY",
                             "b_WOMEN_PERCENT:H_MEAN")) %>%
        mutate(imp = i)
    })
    
    # Combine all posteriors
    bind_rows(posterior_list)
  }
  
  # Get pooled posterior
  posterior_df <- get_pooled_posterior(models_list)
  
  # Convert to matrix format for bayesplot
  posterior_mat <- posterior_df %>%
    dplyr::select(-imp) %>%
    as.matrix()
  
  # Create nice parameter names for plotting
  param_names <- c(
    "b_Intercept" = "Intercept",
    "b_WOMEN_PERCENT" = "Women %",
    "b_UNION_COVERAGE_PERCENT" = "Union Coverage",
    "b_OSHA_VIOLATIONS" = "OSHA Violations",
    "b_OSHA_AVG_PENALTY" = "OSHA Penalty",
    "b_H_MEAN" = "Mean Wage",
    "b_WOMEN_PERCENT:UNION_COVERAGE_PERCENT" = "Women % × Union",
    "b_WOMEN_PERCENT:OSHA_VIOLATIONS" = "Women % × OSHA Viol.",
    "b_WOMEN_PERCENT:OSHA_AVG_PENALTY" = "Women % × OSHA Pen.",
    "b_WOMEN_PERCENT:H_MEAN" = "Women % × Wage"
  )
  
  # Create plots
  # 1. Areas plot with 80% intervals
  color_scheme_set("viridis")
  p1 <- mcmc_areas(posterior_mat, 
                   prob = 0.8,
                   prob_outer = 0.95,
                   point_est = "median") +
    ggtitle("Posterior Distributions",
            "with medians, 80% and 95% intervals") +
    theme_minimal() +
    scale_y_discrete(labels = param_names)
  
  # 2. Forest plot
  p2 <- mcmc_intervals(posterior_mat,
                      prob = 0.8,
                      prob_outer = 0.95,
                      point_est = "median") +
    ggtitle("Posterior Intervals",
            "with medians, 80% and 95% intervals") +
    theme_minimal() +
    scale_y_discrete(labels = param_names) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5)
  
  # 3. Ridgeline plot
  p3 <- mcmc_areas_ridges(posterior_mat,
                         prob = 0.8,
                         prob_outer = 0.95) +
    ggtitle("Posterior Distributions (Ridgeline)",
            "with 80% and 95% intervals") +
    theme_minimal() +
    scale_y_discrete(labels = param_names)
  
  return(list(
    areas = p1,
    intervals = p2,
    ridges = p3
  ))
}

# Example usage:
posterior_plots <- create_posterior_plots(all_models)
print(posterior_plots$areas)      # Areas plot
print(posterior_plots$intervals)  # Forest plot
print(posterior_plots$ridges)     # Ridgeline plot























# Function to create the four-category outcome
create_four_category_outcome <- function(data) {
  data <- data %>%
    mutate(
      category = case_when(
        SEX_Female == 1 & RACE_Black == 1 ~ "Black_Women",
        SEX_Female == 1 & RACE_Black == 0 ~ "NonBlack_Women",
        SEX_Female == 0 & RACE_Black == 1 ~ "Black_NonWomen",
        SEX_Female == 0 & RACE_Black == 0 ~ "NonBlack_NonWomen"
      ),
      category = factor(category, levels = c("Black_Women", "NonBlack_Women", "Black_NonWomen", "NonBlack_NonWomen"))
    )
  return(data)
}

# Apply to all imputed datasets
imputed_datasets_with_categories <- lapply(imputed_datasets_with_dummies, create_four_category_outcome)

fit_multinomial_models <- function(imputed_datasets) {
  models_list <- lapply(imputed_datasets, function(data) {
    # Basic data prep
    data_model <- data
    data_model$R_NUMBER_OF_EMPLOYEES_ORDERED <- as.numeric(as.character(data_model$R_NUMBER_OF_EMPLOYEES_ORDERED))
    
    # Scale variables
    data_model$R_NUMBER_OF_EMPLOYEES_ORDERED <- scale(data_model$R_NUMBER_OF_EMPLOYEES_ORDERED)
    data_model$Age <- scale(data_model$Age)
    data_model$WOMEN_PERCENT <- scale(data_model$WOMEN_PERCENT)
    data_model$UNION_COVERAGE_PERCENT <- scale(data_model$UNION_COVERAGE_PERCENT)
    data_model$OSHA_VIOLATIONS <- scale(data_model$OSHA_VIOLATIONS)
    data_model$OSHA_AVG_PENALTY <- scale(data_model$OSHA_AVG_PENALTY)
    data_model$H_MEAN <- scale(data_model$H_MEAN)
    
    # Ensure category is a factor
    data_model$category <- factor(data_model$category,
                                levels = c("Black_Women", "NonBlack_Women",
                                         "Black_NonWomen", "NonBlack_NonWomen"))
    
    # Start with a simpler model
    model <- brm(
      category ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age + 
        WOMEN_PERCENT + UNION_COVERAGE_PERCENT + 
        OSHA_VIOLATIONS + OSHA_AVG_PENALTY + H_MEAN +
        (1|industry_code_merged),
      family = categorical(),
      data = data_model,
      cores = 1,
      chains = 2,
      iter = 2000,
      seed = 123,
      control = list(
        adapt_delta = 0.99,
        max_treedepth = 15
      )
    )
    return(model)
  })
  return(models_list)
}

# Fit models to imputed datasets
multinomial_models <- fit_multinomial_models(imputed_datasets_with_categories)

pool_multinomial_results <- function(models_list) {
  # Extract coefficients from each model
  coefs_list <- lapply(models_list, fixef)
  
  # Number of imputations
  m <- length(coefs_list)
  
  # Initialize a data frame to store pooled results
  coef_names <- rownames(coefs_list[[1]])
  pooled_results <- data.frame(term = coef_names)
  
  # For each coefficient, calculate pooled estimates
  for (coef in coef_names) {
    # Get estimates across imputations
    estimates <- sapply(coefs_list, function(x) x[coef, "Estimate"])
    
    # Within-imputation variance (average of variances)
    within_var <- mean(sapply(models_list, function(x) {
      vcov_matrix <- vcov(x)
      coef_idx <- which(rownames(vcov_matrix) == coef)
      return(vcov_matrix[coef_idx, coef_idx])
    }))
    
    # Between-imputation variance (variance of estimates)
    between_var <- var(estimates)
    
    # Total variance (Rubin's rules)
    total_var <- within_var + between_var * (1 + 1/m)
    
    # Pooled estimate (mean of estimates)
    pooled_estimate <- mean(estimates)
    
    # Pooled standard error (square root of total variance)
    pooled_se <- sqrt(total_var)
    
    # Store results
    pooled_results[pooled_results$term == coef, "estimate"] <- pooled_estimate
    pooled_results[pooled_results$term == coef, "std.error"] <- pooled_se
  }
  
  # Calculate z-values and p-values
  pooled_results$z.value <- pooled_results$estimate / pooled_results$std.error
  pooled_results$p.value <- 2 * (1 - pnorm(abs(pooled_results$z.value)))
  
  return(pooled_results)
}

# Use the new pooling function
pooled_results <- pool_multinomial_results(multinomial_models)


library(bayesplot)
library(patchwork)

create_diagnostic_grid <- function(models_list) {
  # Create list to store plots for each diagnostic type
  nuts_plots <- list()
  trace_plots <- list()
  density_plots <- list()
  neff_plots <- list()
  
  # Generate plots for each model
  for(i in seq_along(models_list)) {
    color_scheme_set("viridis")
    model <- models_list[[i]]
    
    # NUTS plot
    nuts_plots[[i]] <- create_nuts_plot(model) +
      ggtitle(paste("NUTS Energy Diagnostic -", "Imputation", i))
    
    # Trace plot
    trace_plots[[i]] <- create_trace_plot(model) +
      ggtitle(paste("Trace Plots -", "Imputation", i))
    
    # Density plot
    density_plots[[i]] <- create_density_plot(model) +
      ggtitle(paste("Density Overlays -", "Imputation", i))
    
    # Neff plot
    neff_plots[[i]] <- create_neff_plot(model) +
      ggtitle(paste("Effective Sample Size -", "Imputation", i))
  }
  
  # Combine plots by type
  nuts_grid <- wrap_plots(nuts_plots, ncol = 2) +
    plot_annotation(title = "NUTS Energy Diagnostics Across Imputations")
  
  trace_grid <- wrap_plots(trace_plots, ncol = 2) +
    plot_annotation(title = "Trace Plots Across Imputations")
  
  density_grid <- wrap_plots(density_plots, ncol = 2) +
    plot_annotation(title = "Density Overlays Across Imputations")
  
  neff_grid <- wrap_plots(neff_plots, ncol = 2) +
    plot_annotation(title = "Effective Sample Sizes Across Imputations")
  
  return(list(
    nuts = nuts_grid,
    traces = trace_grid,
    densities = density_grid,
    neff = neff_grid
  ))
}

# Modified individual plot functions for multinomial model
create_nuts_plot <- function(model) {
  np <- nuts_params(model)
  mcmc_nuts_energy(np) +
    theme_minimal()
}

create_trace_plot <- function(model) {
  posterior_draws <- as_draws_df(model, subset = sample(1:4000, 2000))
  
  # Using key parameters from your model
  key_pars <- c(
    "b_muNonBlackWomen_Intercept",
    "b_muNonBlackWomen_WOMEN_PERCENT",
    "b_muNonBlackWomen_WOMEN_PERCENT:UNION_COVERAGE_PERCENT"
  )
  
  mcmc_trace(posterior_draws,
             pars = key_pars,
             facet_args = list(ncol = 1)) +
    theme_minimal()
}

create_density_plot <- function(model) {
  posterior_draws <- as_draws_df(model, subset = sample(1:4000, 2000))
  
  key_pars <- c(
    "b_muNonBlackWomen_Intercept",
    "b_muNonBlackWomen_WOMEN_PERCENT",
    "b_muNonBlackWomen_WOMEN_PERCENT:UNION_COVERAGE_PERCENT"
  )
  
  mcmc_dens_overlay(posterior_draws,
                   pars = key_pars) +
    theme_minimal()
}

# The NUTS and neff plot functions can remain the same as they don't need specific parameter names

# Try creating diagnostics again
diagnostic_grids <- create_diagnostic_grid(multinomial_models)

# View individual grids
print(diagnostic_grids$nuts)
print(diagnostic_grids$traces)
print(diagnostic_grids$densities)
print(diagnostic_grids$neff)

# You can also get summary statistics
summary(multinomial_models[[1]])

# And look at the pooled results we created earlier
print(pooled_results)


fit_multinomial_model <- function(data) {
  # Basic data prep
  data_model <- data
  data_model$R_NUMBER_OF_EMPLOYEES_ORDERED <- as.numeric(as.character(data_model$R_NUMBER_OF_EMPLOYEES_ORDERED))
  
  # Scale variables
  data_model$R_NUMBER_OF_EMPLOYEES_ORDERED <- scale(data_model$R_NUMBER_OF_EMPLOYEES_ORDERED)
  data_model$Age <- scale(data_model$Age)
  data_model$WOMEN_PERCENT <- scale(data_model$WOMEN_PERCENT)
  data_model$UNION_COVERAGE_PERCENT <- scale(data_model$UNION_COVERAGE_PERCENT)
  data_model$OSHA_VIOLATIONS <- scale(data_model$OSHA_VIOLATIONS)
  data_model$OSHA_AVG_PENALTY <- scale(data_model$OSHA_AVG_PENALTY)
  data_model$H_MEAN <- scale(data_model$H_MEAN)
  
  # Ensure category is a factor
  data_model$category <- factor(data_model$category,
                              levels = c("Black_Women", "NonBlack_Women",
                                       "Black_NonWomen", "NonBlack_NonWomen"))
  
  # Fit multilevel multinomial model
  library(nnet)
  model <- multinom(
    category ~ R_NUMBER_OF_EMPLOYEES_ORDERED + Age + 
      WOMEN_PERCENT + UNION_COVERAGE_PERCENT + 
      OSHA_VIOLATIONS + OSHA_AVG_PENALTY + H_MEAN +
      (1|industry_code_merged),
    data = data_model,
    trace = FALSE
  )
  
  # Get model summary
  model_summary <- summary(model)
  
  # Extract coefficients and standard errors
  coefs <- coef(model)
  ses <- sqrt(diag(vcov(model)))
  
  # Calculate z-values and p-values
  z_values <- coefs / ses
  p_values <- 2 * (1 - pnorm(abs(z_values)))
  
  # Create results data frame
  results <- data.frame(
    term = names(coefs),
    estimate = coefs,
    std.error = ses,
    z.value = z_values,
    p.value = p_values
  )
  
  # Add random effects information
  ranef_summary <- ranef(model)
  
  return(list(
    model = model,
    fixed_effects = results,
    random_effects = ranef_summary
  ))
}

# Function to analyze results
analyze_results <- function(model_results) {
  # Print fixed effects
  cat("\nFixed Effects:\n")
  print(model_results$fixed_effects)
  
  # Print random effects summary
  cat("\nRandom Effects Summary:\n")
  print(summary(model_results$random_effects))
  
  # Calculate model fit statistics
  cat("\nModel Fit Statistics:\n")
  print(AIC(model_results$model))
  print(BIC(model_results$model))
  
  # Print likelihood ratio test
  cat("\nLikelihood Ratio Test:\n")
  print(anova(model_results$model))
}

# Run the analysis
model_results <- fit_multinomial_model(data)
analyze_results(model_results)