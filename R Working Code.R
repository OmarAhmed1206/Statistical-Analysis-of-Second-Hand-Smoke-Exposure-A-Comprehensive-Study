library(readr)
library(dplyr)
library(ggplot2)
library(DescTools)

data <- read_csv("second_hand_smoke_dataset_100.csv", show_col_types = FALSE)

cat("0. Data Preprocessing\n")
initial_rows <- nrow(data)
data <- data %>% distinct()
cat("Number of duplicate rows removed:", initial_rows - nrow(data), "\n")
cat("Final dataset size:", nrow(data), "rows\n")

data$Exposed <- factor(data$Exposed, levels = c("No", "Yes"))
data$Started_smoking <- factor(data$Started_smoking, levels = c("No", "Yes"))
data$Parent_smokes <- factor(data$Parent_smokes, levels = c("No", "Yes"))
data$Respiratory_symptoms <- factor(data$Respiratory_symptoms, levels = c("None", "Mild", "Moderate", "Severe"), ordered = TRUE)

cat("\n1. Exploratory Data Analysis (EDA)\n")
continuous_vars <- c("Headaches_per_month", "Hours_week_exposure", "Physical_activity_hours_per_week", "Stress_level_1_10")
for (var in continuous_vars) {
  cat("\nVariable:", var)
  cat("\n  Mean:", round(mean(data[[var]], na.rm = TRUE), 3))
  cat("\n  Median:", round(median(data[[var]], na.rm = TRUE), 3))
  cat("\n  Mode:", Mode(data[[var]])[1])
  cat("\n  SD:", round(sd(data[[var]], na.rm = TRUE), 3))
  cat("\n  IQR:", round(IQR(data[[var]], na.rm = TRUE), 3))
}

cat("\n\nFrequency Tables:\n")
cat("\nExposed:\n"); print(table(data$Exposed))
cat("\nRespiratory_symptoms:\n"); print(table(data$Respiratory_symptoms))
cat("\nStarted_smoking:\n"); print(table(data$Started_smoking))
cat("\nParent_smokes:\n"); print(table(data$Parent_smokes))

cat("\n2. Inferential Analysis: Relationships Studied\n")

cat("\nR1: Exposure vs Headaches per Month\n")
print(t.test(Headaches_per_month ~ Exposed, data = data))
cat("\nGroup Stats:\n"); print(data %>% group_by(Exposed) %>% summarise(Mean = mean(Headaches_per_month), IQR = IQR(Headaches_per_month)))

cat("\nR2a: Hours vs Respiratory Symptoms\n")
print(kruskal.test(Hours_week_exposure ~ Respiratory_symptoms, data = data))

cat("\nR2b: Hours vs Smoking Initiation\n")
print(t.test(Hours_week_exposure ~ Started_smoking, data = data))
cat("\nGroup Stats:\n"); print(data %>% group_by(Started_smoking) %>% summarise(Mean = mean(Hours_week_exposure), IQR = IQR(Hours_week_exposure)))

cat("\nR3a: Exposure vs Physical Activity\n")
print(t.test(Physical_activity_hours_per_week ~ Exposed, data = data))
cat("\nGroup Stats:\n"); print(data %>% group_by(Exposed) %>% summarise(Mean = mean(Physical_activity_hours_per_week), IQR = IQR(Physical_activity_hours_per_week)))

cat("\nR3b: Exposure vs Stress Level\n")
print(t.test(Stress_level_1_10 ~ Exposed, data = data))
cat("\nGroup Stats:\n"); print(data %>% group_by(Exposed) %>% summarise(Mean = mean(Stress_level_1_10), IQR = IQR(Stress_level_1_10)))

cat("\nR4: Parent Smoking vs Started Smoking\n")
r4_table <- table(data$Parent_smokes, data$Started_smoking)
cat("Contingency Table:\n"); print(r4_table)
print(chisq.test(r4_table))

cat("\n3. Regression Analysis\n")
reg_model <- lm(Headaches_per_month ~ Hours_week_exposure, data = data)
print(summary(reg_model))
cat("\n95% CI for Coefficients:\n"); print(confint(reg_model, level = 0.95))
cat("Interpretation: 95% confident true intercept and slope are within these ranges.\n")

new_data <- data.frame(Hours_week_exposure = c(0, 10))
cat("\nPredictions for 0 and 10 hours:\n"); print(predict(reg_model, newdata = new_data, interval = "predict", level = 0.95))

cat("\n4. Population Mean Inference\n")
ci_hpm <- t.test(data$Headaches_per_month, conf.level = 0.95)$conf.int
ci_hwe <- t.test(data$Hours_week_exposure, conf.level = 0.95)$conf.int
cat("95% CI for Headaches mean: [", round(ci_hpm[1], 3), ", ", round(ci_hpm[2], 3), "]\n", sep = "")
cat("95% CI for Exposure hours mean: [", round(ci_hwe[1], 3), ", ", round(ci_hwe[2], 3), "]\n", sep = "")

cat("\n5. Visualizations saved to FINAL_STAT_DIAGRAMS.pdf\n")

pdf("FINAL_STAT_DIAGRAMS1.pdf")
print(ggplot(data, aes(x = Headaches_per_month)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") + labs(title = "Histogram: Headaches per Month"))
print(ggplot(data, aes(x = Hours_week_exposure)) + geom_histogram(binwidth = 2, fill = "red", color = "black") + labs(title = "Histogram: Hours Week Exposure"))
print(ggplot(data, aes(x = Physical_activity_hours_per_week)) + geom_histogram(binwidth = 1, fill = "green", color = "black") + labs(title = "Histogram: Physical Activity Hours"))
print(ggplot(data, aes(x = Stress_level_1_10)) + geom_histogram(binwidth = 0.5, fill = "purple", color = "black") + labs(title = "Histogram: Stress Level"))
print(ggplot(data, aes(x = Exposed, y = Headaches_per_month)) + geom_boxplot() + labs(title = "R1: Headaches by Exposure Group"))
print(ggplot(data, aes(x = Respiratory_symptoms, y = Hours_week_exposure)) + geom_boxplot() + labs(title = "R2a: Exposure Hours by Respiratory Symptom Severity"))
print(ggplot(data, aes(x = Started_smoking, y = Hours_week_exposure)) + geom_boxplot() + labs(title = "R2b: Exposure Hours by Smoking Initiation"))
print(ggplot(data, aes(x = Exposed, y = Physical_activity_hours_per_week)) + geom_boxplot() + labs(title = "R3a: Physical Activity by Exposure Group"))
print(ggplot(data, aes(x = Exposed, y = Stress_level_1_10)) + geom_boxplot() + labs(title = "R3b: Stress Level by Exposure Group"))
print(ggplot(data, aes(x = Parent_smokes, fill = Started_smoking)) + geom_bar(position = "fill") + labs(title = "R4: Smoking Initiation by Parent Smoking Status", y = "Proportion"))
print(ggplot(data, aes(x = Hours_week_exposure, y = Headaches_per_month)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + labs(title = "Regression: Headaches vs Hours of Exposure", x = "Hours per Week Exposure", y = "Headaches per Month"))
dev.off()

cat("\nAll analyses complete.\n")
