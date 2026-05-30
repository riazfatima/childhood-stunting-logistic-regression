############################################################
# CHILDHOOD STUNTING PREDICTION USING LOGISTIC REGRESSION
# Pakistan Punjab MICS6 Data
############################################################

############################################################
# 1. LOAD LIBRARIES
############################################################

library(haven)
library(dplyr)
library(caret)
library(pROC)

############################################################
# 2. LOAD DATA
############################################################

ch <- read_sav("data/ch.sav")
bh <- read_sav("data/bh.sav")

############################################################
# 3. SELECT REQUIRED VARIABLES
############################################################

child_data <- ch %>%
  select(
    HH1, HH2, LN, UF4,
    HAZ2,
    UB2,
    HL4,
    BMI,
    windex5,
    melevel,
    BD2
  )

############################################################
# 4. PREPARE BIRTH HISTORY DATA
############################################################

bh_clean <- bh %>%
  select(
    HH1,
    HH2,
    WM3,
    BH3,
    magebrt
  ) %>%
  rename(
    UF4 = WM3,
    LN = BH3
  )

############################################################
# 5. MERGE DATASETS
############################################################

merge_data <- child_data %>%
  left_join(
    bh_clean,
    by = c("HH1", "HH2", "UF4", "LN")
  ) %>%
  distinct(HH1, HH2, LN, .keep_all = TRUE)

############################################################
# 6. KEEP UNDER-5 CHILDREN
############################################################

merge_data <- merge_data %>%
  filter(UB2 < 60)

############################################################
# 7. CREATE STUNTING VARIABLE
############################################################

merge_data <- merge_data %>%
  mutate(
    stunted = case_when(
      HAZ2 < -6 | HAZ2 > 6 ~ NA_real_,
      HAZ2 < -2 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(!is.na(stunted))

merge_data$stunted <- factor(
  merge_data$stunted,
  levels = c(0, 1),
  labels = c("Non-Stunted", "Stunted")
)

############################################################
# 8. CREATE ANALYSIS VARIABLES
############################################################

merge_data <- merge_data %>%
  mutate(
    
    child_age_months = as.numeric(UB2),
    
    HL4 = factor(
      HL4,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    BD2 = factor(
      BD2,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    BMI = as.numeric(BMI),
    
    windex5 = factor(
      windex5,
      levels = 1:5,
      labels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
    ),
    
    melevel = factor(
      melevel,
      levels = c(0,1,2,3,4),
      labels = c(
        "None",
        "Primary",
        "Middle",
        "Secondary",
        "Higher"
      )
    )
  )

############################################################
# 9. HANDLE MISSING VALUES
############################################################

get_mode <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(ux, ux)))]
}

merge_data <- merge_data %>%
  mutate(
    child_age_months = ifelse(
      is.na(child_age_months),
      median(child_age_months, na.rm = TRUE),
      child_age_months
    ),
    
    BMI = ifelse(
      is.na(BMI),
      median(BMI, na.rm = TRUE),
      BMI
    )
  )

categorical_vars <- c(
  "HL4",
  "BD2",
  "windex5",
  "melevel"
)

merge_data[categorical_vars] <- lapply(
  merge_data[categorical_vars],
  function(x) {
    x[is.na(x)] <- get_mode(x)
    x
  }
)

############################################################
# 10. KEEP FINAL MODEL VARIABLES
############################################################

analysis_data <- merge_data %>%
  select(
    stunted,
    child_age_months,
    HL4,
    BD2,
    BMI,
    windex5,
    melevel
  )

############################################################
# 11. TRAIN / TEST SPLIT
############################################################

set.seed(123)

train_index <- createDataPartition(
  analysis_data$stunted,
  p = 0.70,
  list = FALSE
)

train_data <- analysis_data[train_index, ]
test_data  <- analysis_data[-train_index, ]

############################################################
# 12. TRAIN LOGISTIC REGRESSION MODEL
############################################################

logistic_model <- glm(
  stunted ~
    child_age_months +
    HL4 +
    BD2 +
    BMI +
    windex5 +
    melevel,
  data = train_data,
  family = binomial()
)

summary(logistic_model)

############################################################
# 13. ODDS RATIOS WITH 95% CI
############################################################

OR_table <- exp(
  cbind(
    OR = coef(logistic_model),
    confint.default(logistic_model)
  )
)

p_values <- summary(logistic_model)$coefficients[,4]

final_OR_table <- data.frame(
  Variable = rownames(OR_table),
  OR = round(OR_table[,1], 3),
  CI_Lower = round(OR_table[,2], 3),
  CI_Upper = round(OR_table[,3], 3),
  P_Value = round(p_values, 4)
)

print(final_OR_table)


############################################################
# 14. PREDICT ON TEST DATA
############################################################

predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

predicted_class <- ifelse(
  predicted_prob > 0.5,
  "Stunted",
  "Non-Stunted"
)

predicted_class <- factor(
  predicted_class,
  levels = levels(test_data$stunted)
)

############################################################
# 15. CONFUSION MATRIX
############################################################

confusion_matrix <- confusionMatrix(
  predicted_class,
  test_data$stunted,
  positive = "Stunted"
)

print(confusion_matrix)

############################################################
# 16. ROC CURVE AND AUC
############################################################

roc_obj <- roc(
  response = test_data$stunted,
  predictor = predicted_prob
)

auc_value <- auc(roc_obj)

cat("\nAUC =", round(auc_value, 4), "\n")

############################################################
# 17. SAVE ROC CURVE
############################################################


plot(
  roc_obj,
  main = paste(
    "ROC Curve (AUC =",
    round(auc_value, 3),
    ")"
  ),
  col = "blue",
  lwd = 3,
  print.auc = TRUE
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "gray"
)

dev.off()


############################################################
# 19. MODEL PERFORMANCE SUMMARY
############################################################

cat("\n=============================\n")
cat("MODEL PERFORMANCE SUMMARY\n")
cat("=============================\n")

cat(
  "\nAccuracy:",
  round(
    confusion_matrix$overall["Accuracy"],
    3
  )
)

cat(
  "\nSensitivity:",
  round(
    confusion_matrix$byClass["Sensitivity"],
    3
  )
)

cat(
  "\nSpecificity:",
  round(
    confusion_matrix$byClass["Specificity"],
    3
  )
)

cat(
  "\nAUC:",
  round(auc_value, 3)
)

cat("\n\nAnalysis Complete.\n")
