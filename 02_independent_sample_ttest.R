###############################################################################
############################ T-Tests Analysis #################################

###############################################################################
####################### Author: Christos Dalamarinis ##########################

###############################################################################
### Purpose: Conduct Independent and Paired T-tests with Assumption Checks ###

# ============= Load Required Libraries ===============
library(psych)        #For descriptive statistics (describe, describeBy functions)
library(ggplot2)      #For creating publication-quality graphics and visualizations
library(car)          #For Levene's test of homogeneity of variance
library(pastecs)      #For comprehensive descriptive statistics (stat.desc function)
library(gridExtra)    #For arranging multiple plots in a grid layout
library(DescTools)    #For effect size calculations (EtaSq function)
library(Hmisc)        #For additional statistical functions and data manipulation
library(ggsignif)     #For adding significance brackets to ggplot2 visualizations

# ============= Load Data ==============================
# Read the sample dataset from CSV file
data <- read.csv("ttests_sample_data.csv") # Load your data here. By default, "read.csv()" treats the first row as column names "(header = TRUE)" and automatically starts reading FROM THE SECOND ROW ONWARDS. Missing values in the .csv file are recognized as "NA" in R.

# Convert group variable to factor with specified levels. Factors are variables we control or measure in order to determine their effect in the Dependent Variable.
data$group_numeric <- factor(data$group_numeric,levels = c(0:1), labels = c("controls", "treatment")) # ATTENTION: if your groups are assigned a "0" for control and "1" for treatment then:
data$sex <- factor(data$sex,levels = c(0,1), labels = c("males", "females")) # to indicate the that "0" corresponds to males  and "1" corresponds to females

# ============ Check Data Are Read Correctly ==========
head(data)
tail(data)
dim(data)
names(data)
summary(data)
View(data)

# ============= Descriptive Statistics ================
describe(data)
psych::describe(data) # a more comprehensive option
describeBy(data$score, group=data$group_numeric)
describeBy(data$score, group = data$sex)
describeBy(data$memory, group=data$group_numeric)
describeBy(data$reaction_time, group=data$group_numeric)
stat.desc(data$score, norm = T)
# The above is just an indication, additional descriptive tests can be added depending on the research question or goals.

# Since Descriptive analysis showed some missing values for "reaction_time", we have to identify them. Keep in mind that this procedure might be a bit different from dataset to dataset, depending on how the researcher has encoded missing values in his experiment code.
# Count missing values
sum(is.na(data$reaction_time))

# See which rows have missing values
which(is.na(data$reaction_time)) # Shows the ROW numbers with missing data.
data[is.na(data$reaction_time), c("subject", "reaction_time")] # Shows the subject IDs with missing values for a specific variable, use this in case your data starts at the second row.

# Now that we have identified the ID's of missing values we can either exclude them or keep them. 
# A common practice is to code with "999" the missing values and exclude them in this stage.
# However in our case here, missing values are coded as "NA" and R can handle them automatically since data are in .csv format. Therefore we take care of this in the Levene's test right away (see below).
# By default:
    ## Empty cells (,,) are read as NA.
    ## The literal text "NA" is also treated as NA.
    ## You can customize which strings count as missing with na.strings
# Otherwise:
    ##Hypothetical: if "reaction_time" used "999" for missing
data$reaction_time <- ifelse(data$reaction_time == 999, NA, data$reaction_time)


###############################################################################
################## PART 1: INDEPENDENT SAMPLES T-TEST #########################
###############################################################################

# Dataset: ttests_sample_data.csv
# Background: 200 healthy young adults were recruited to test whether a novel 4-week brain training program improves cognitive performance compared to standard activities.
# Design:  Participants were randomly assigned to either:
         ##Control group (n=100): Engaged in standard daily activities
         ##Treatment group (n=100): Completed the brain training program for 30 minutes daily

# Measurements: After 4 weeks, all participants completed three cognitive assessments:
    ##Primary outcome: Cognitive performance score (general intelligence test)
    ##Secondary outcomes: Memory score and reaction time

# Research Question: Does the brain training program significantly improve cognitive performance compared to the control condition?

# Analysis Outline:
  ##Normality
  ##Homogeneity of Variance
  ##Visual inspection - Histograms

# ============= Check Assumptions for Independent T-test ======

# --- 1. Normality Check ---
# Normality has to be checked within each group, not in the total data.
# Therefore, we need to make plots and run analyses the "control" and "treatment" group separately.
# We make separate data objects for each group.
control.group <- subset(data, data$group_numeric=="controls")
treatment.group <- subset(data, data$group_numeric=="treatment")

# We create a series of Histograms for the 2 groups
hist.control.group <- ggplot(control.group, aes(score))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill = "white", bins = 5)+ # we choose "density" here to overlay the red (normal) curve, it shows us if our data match a normal distribution. The y-axis values don't matter much as we simply look at the shape of the histogram and the curve.
  labs(x = "Cognitive Score (Control Group)", y = "Density")+
  stat_function(fun = dnorm, args = list(mean = mean(control.group$score, na.rm = T),sd = sd(control.group$score, na.rm = T)), colour = "red", linewidth = 1)

hist.treatment.group <- ggplot(treatment.group, aes(score))+
  geom_histogram(aes(y = after_stat(density)), color = "black", fill = "white", bins = 5)+
  labs(x = "Cognitive Score (Treatment Group)", y = "Density")+
  stat_function(fun = dnorm, args = list(mean = mean(treatment.group$score, na.rm = T), sd = sd(treatment.group$score, na.rm = T)), color = "red", linewidth = 1)

hist.all <- arrangeGrob(hist.control.group, hist.treatment.group, ncol = 2)
grid::grid.draw(hist.all)

# If the data don't seem normally distributed from the visual inspection we can run a formal test of normality.
stat.desc(control.group$score, norm = T) # "normtest.p" is non-significant therefore data are normally distributed
stat.desc(treatment.group$score, norm = T) # "normtest.p" is non-significant therefore data are normally distributed

# --- 2. Homogeneity of Variance Check ---
# Levene's test for equal variances between groups
leveneTest(data$score, data$group_numeric, center = mean)
leveneTest(data$memory, data$group_numeric, center = mean)
leveneTest(reaction_time ~ group_numeric, data = data, center = mean) # Levene's test with missing values

# In case the Homogeneity assumption is violated, and Levene's test is significant, run a Welch analysis
welch.score <- t.test(score ~ group, data=data, alternative="two.sided", var.equal=F, paired=F)
welch.score


# ============= Independent Samples T-Test ============
# Since assumptions are met we proceed with T-test analysis.
# T-test for score
tt.score <- t.test(score ~ group_numeric, data = data, alternative="two.sided", var.equal = T, paired = F)
tt.score #Print results

# T-test for memory
tt.memory <- t.test(memory ~ group_numeric, data = data, alternative="two.sided", var.equal = T, paired = F)
tt.memory

# T-test for reaction time
tt.reaction <- t.test(reaction_time ~ group_numeric, data = data, alternative="two.sided", var.equal = T, paired = F)
tt.reaction


# ============= Effect Size Calculation ===============
# Cohen's d for Independent samples T-test
# Step 1: Calculate means for each group
mean.control <- mean(control.group$score, na.rm = T)
mean.treatment <- mean(treatment.group$score, na.rm = T)

# Step 2: Calculate the standard deviation for each group
sd.control <- sd(control.group$score, na.rm = T)
sd.treatment <- sd(treatment.group$score, na.rm = T)

# Step 3: Get sample sizes
n.control <- length(na.omit(control.group$score))
n.treatment <- length(na.omit(treatment.group$score))

# Step 4: Calculate pooled standard deviation
sd.pooled <- sqrt(((n.control - 1) * sd.control^2 + (n.treatment - 1) * sd.treatment^2) / (n.control + n.treatment - 2))

# Step 5: Calculate Cohen's d
cohen.d <- (mean.treatment - mean.control) / sd.pooled
cohen.d

# Display results (optional)
cat("Control: M =", round(mean.control, 2), ", SD =", round(sd.control, 2), "\n")
cat("Treatment: M =", round(mean.treatment, 2), ", SD =", round(sd.treatment, 2), "\n")
cat("Cohen's.d =", round(cohen.d, 2))

# Alternative more simple way (with interpretation included):
library(DescTools)
CohenD(data$score[data$group_numeric == "treatment"], data$score[data$group_numeric == "controls"])
# Or:
cohen.d(score ~ group_numeric, data = data)



# ============= Visualization with Significance =======
# The code below is just an indication, alterations are possible to accomodate better visualizations for different kind of data or analysis
# Boxplot with significance bracket score
plot.score <- ggplot(data, aes(x = group_numeric, y = score))+
  geom_boxplot(aes(fill = group_numeric))+
  scale_fill_manual(values = c("lightskyblue1", "deepskyblue4"))+
  geom_signif(comparisons = list(c("controls", "treatment")),
              test = "t.test",
              map_signif_level = T,
              textsize = 4)+
  labs(x = "Group",
       y = "Cognitive Score",
       title = "Cognitive Performance: Cognitive Control vs. Treatment")+
  theme_minimal()+
  theme(legend.position = "none")
plot.score

# Boxplot for memory
plot.memory <- ggplot(data, aes(x = group_numeric, y = memory))+
  geom_boxplot(aes(fill = group_numeric))+
  scale_fill_manual(values = c("lightcoral", "green"))+
  geom_signif(comparisons = list(c("controls", "treatment")),
              test = "t.test",
              map_signif_level = T,
              textsize = 4)+
  labs(x = "Group",
       y = "Memory Score",
       title = "Memory Performance: Control vs. Treatment")+
  theme_minimal()+
  theme(legend.position = "none")
plot.memory

# Boxplot for reaction time
plot.reaction <- ggplot(data, aes(x = group_numeric, y = reaction_time))+
  geom_boxplot(aes(fill = group_numeric))+
  scale_fill_manual(values = c("lightgreen", "blue"))+
  geom_signif(comparisons = list(c("controls", "treatment")),
              test = "t.test",
              map_signif_level = T,
              textsize = 4,
              na.rm = T)+
  labs(x = "Group",
       y = "Reaction Time (ms)",
       title = "Reaction Time: Control vs. Treatment")+
  theme_minimal()+
  theme(legend.position = "none")
plot.reaction

# Violin plot showing distribution
violinplot <- ggplot(data, aes(x = group_numeric, y = score, fill = group_numeric)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("lightskyblue1", "deepskyblue4")) +
  geom_signif(comparisons = list(c("controls", "treatment")), 
              test = "t.test",
              map_signif_level = T) +
  labs(x = "Group", 
       y = "Cognitive Score",
       title = "Distribution of Cognitive Scores by Group") +
  theme_minimal() +
  theme(legend.position = "none")
violinplot

# ============= T-Test as Regression Model ============  
# Demonstrating equivalence of t-test and regression.
# The independent samples t-test is mathematically equivalent to linear regression with a binary predictor (group). This section demonstrates this equivalence.
# Regression model: score predicted by group
# "group_numeric" is a factor with 2 levels: "controls" (reference) and "treatment"
reg.score <- lm(score ~ group_numeric, data = data)
summary(reg.score)

# The regression equation is:
# score = b0 + b1*(group_numeric)
# Where:
#   b0 (Intercept) = mean of the reference group (controls)
#   b1 (Coefficient) = difference between treatment and control means
#   b0 + b1 = mean of the treatment group


# ============= Extract Key Statistics ====================
# Extract coefficients
reg.intercept <- coef(reg.score)[1]        # b0 = mean of controls
reg.slope <- coef(reg.score)[2]            # b1 = difference between means

# Extract t-statistic for the slope
reg.t.stat <- summary(reg.score)$coefficients[2, 3]

# Extract p-value for the slope
reg.p.value <- summary(reg.score)$coefficients[2, 4]

# Extract F-statistic from ANOVA
reg.f.stat <- summary(reg.score)$fstatistic[1]

# Extract R-squared (proportion of variance explained)
reg.r.squared <- summary(reg.score)$r.squared


# ============= Compare with T-Test Results ===============
# Run the t-test again to compare
#tt.score <- t.test(score ~ group_numeric, data = data, var.equal = TRUE, paired = FALSE)

# Display comparison
cat("\n========== EQUIVALENCE DEMONSTRATION ==========\n\n")

cat("--- MEANS ---\n")
cat("T-test: Control mean = ", round(tt.score$estimate[1], 3), "\n", sep = "")
cat("Regression: Intercept (b0) = ", round(reg.intercept, 3), "\n", sep = "")
cat("Are they equal? ", round(tt.score$estimate[1], 3) == round(reg.intercept, 3), "\n\n", sep = "")

cat("T-test: Treatment mean = ", round(tt.score$estimate[2], 3), "\n", sep = "")
cat("Regression: b0 + b1 = ", round(reg.intercept + reg.slope, 3), "\n", sep = "")
cat("Are they equal? ", round(tt.score$estimate[2], 3) == round(reg.intercept + reg.slope, 3), "\n\n", sep = "")

cat("--- MEAN DIFFERENCE ---\n")
cat("T-test: Difference = ", round(diff(tt.score$estimate), 3), "\n", sep = "")
cat("Regression: Slope (b1) = ", round(reg.slope, 3), "\n", sep = "")
cat("Are they equal? ", round(diff(tt.score$estimate), 3) == round(reg.slope, 3), "\n\n", sep = "")

cat("--- T-STATISTIC ---\n")
cat("T-test: t = ", round(tt.score$statistic, 3), "\n", sep = "")
cat("Regression: t = ", round(reg.t.stat, 3), "\n", sep = "")
cat("Are they equal? ", abs(round(tt.score$statistic, 3)) == abs(round(reg.t.stat, 3)), "\n\n", sep = "")   # ---------------------------------------------------------------
# NOTE:
# The sign (positive or negative) of the t-statistic (and mean difference) depends on which group is set as the reference level (first factor level).
# In t-test, R compares the first group (controls) to the second (treatment) and the sign will be negative if the second group's mean is higher.
# In regression, the coefficient for 'treatment' is positive if treatment > controls.
# This means the t-statistics may have opposite signs, but their absolute values
# (and statistical significance) are identical.
# When comparing, use abs() to check equivalence if needed.
# ---------------------------------------------------------------

cat("--- P-VALUE ---\n")
cat("T-test: p = ", round(tt.score$p.value, 4), "\n", sep = "")
cat("Regression: p = ", round(reg.p.value, 4), "\n", sep = "")
cat("Are they equal? ", round(tt.score$p.value, 4) == round(reg.p.value, 4), "\n\n", sep = "")


# ============= Verify Mathematical Relationship ==========
# Key relationship: t-statistic squared = F-statistic:
t.stat.squared <- reg.t.stat^2

# Display output
cat("--- RELATIONSHIP BETWEEN t AND F ---\n")
cat("t-statistic from regression = ", round(reg.t.stat, 3), "\n", sep = "")
cat("t-statistic squared = ", round(t.stat.squared, 3), "\n", sep = "")
cat("F-statistic from regression = ", round(reg.f.stat, 3), "\n", sep = "")
cat("Are t² and F equal? ", round(t.stat.squared, 3) == round(reg.f.stat, 3), "\n\n", sep = "") # If the values on both sides of == are equal, it returns TRUE. If they are different, it returns FALSE.


# ============= Visualize Regression Line =================

# Create visualization showing regression predictions
regression.plot <- ggplot(data, aes(x = as.numeric(group_numeric), y = score, color = group_numeric)) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = reg.intercept, color = "blue", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = reg.intercept + reg.slope, color = "red", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("lightskyblue1", "deepskyblue4")) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Controls", "Treatment")) +
  annotate("text", x = 0.7, y = reg.intercept + 2, 
           label = paste0("Controls: M = ", round(reg.intercept, 1)), 
           color = "blue", hjust = 0, size = 4) +
  annotate("text", x = 2.3, y = reg.intercept + reg.slope + 2, 
           label = paste0("Treatment: M = ", round(reg.intercept + reg.slope, 1)), 
           color = "red", hjust = 1, size = 4) +
  labs(x = "Group", 
       y = "Cognitive Score",
       title = "Linear Regression: Score ~ Group",
       subtitle = paste0("Slope (difference) = ", round(reg.slope, 2), 
                         ", t = ", round(reg.t.stat, 2), 
                         ", p = ", round(reg.p.value, 4))) +
  theme_minimal() +
  theme(legend.position = "none")
regression.plot

###############################################################################
########### NON-PARAMETRIC ALTERNATIVE: MANN-WHITNEY U TEST ###################
###############################################################################

# When assumptions of normality or homogeneity of variance are violated, use the Mann-Whitney U test (also called Wilcoxon rank-sum test).
# This test compares the distributions of two independent groups using ranks instead of raw scores, making it robust to non-normal data and outliers.

# ============= Mann-Whitney U Test (Wilcoxon Rank-Sum) =====
# First, since the non-parametric tests run with ranked data, we add rank variables to the dataset
data$rank_score <- rank(data$score, ties.method = "average", na.last = "keep")
data$rank_memory <- rank(data$memory, ties.method = "average", na.last = "keep")
data$rank_reaction <- rank(data$reaction_time, ties.method = "average", na.last = "keep")
# The above adds directly the rank variables to our dataset

# Now we can check it actually worked
head(data)

# Descriptive statistics of the rank variables for the two groups separately
by(data$rank_score, data$group_numeric, stat.desc)
by(data$rank_memory, data$group_numeric, stat.desc)
by(data$rank_reaction, data$group_numeric, stat.desc)

# Now we proceed with the actual Mann-Whitney U test
# Test for score variable
wilcox.score <- wilcox.test(score ~ group_numeric, data = data, alternative = "two.sided", exact = F, correct = T, paired = F)
wilcox.score

# Test for memory variable
wilcox.memory <- wilcox.test(memory ~ group_numeric, data = data, alternative = "two.sided", exact = F,  correct = T, paired = F)
wilcox.memory

# Test for reaction time (with missing values)
wilcox.reaction <- wilcox.test(reaction_time ~ group_numeric, data = data, alternative = "two.sided", exact = F, correct = T, paired = F)
wilcox.reaction


# ============= Effect Size for Mann-Whitney U ==============
# Calculate effect size r for Mann-Whitney U test
# r = Z / sqrt(N) where Z is the standardized test statistic

# --- For SCORE variable ---
W.score <- wilcox.score$statistic
W.expected <- n.control * (n.control + n.treatment + 1) / 2
SE.W <- sqrt(n.control * n.treatment * (n.control + n.treatment + 1) / 12)
Z.score <- (W.score - W.expected) / SE.W
r.effect.score <- abs(Z.score) / sqrt(n.control + n.treatment)
print(paste("Effect size r =", round(r.effect.score, 3)))

# --- For MEMORY variable ---
W.memory <- wilcox.memory$statistic
Z.memory <- (W.memory - W.expected) / SE.W
r.effect.memory <- abs(Z.memory) / sqrt(n.control + n.treatment)
print(paste("Effect size r =", round(r.effect.memory, 3)))


# --- For REACTION TIME variable ---
W.reaction <- wilcox.reaction$statistic
n.control.rt <- sum(data$group_numeric == "controls" & !is.na(data$reaction_time))
n.treatment.rt <- sum(data$group_numeric == "treatment" & !is.na(data$reaction_time))
W.expected.rt <- n.control.rt * (n.control.rt + n.treatment.rt + 1) / 2
SE.W.rt <- sqrt(n.control.rt * n.treatment.rt * (n.control.rt + n.treatment.rt + 1) / 12)
Z.reaction <- (W.reaction - W.expected.rt) / SE.W.rt
r.effect.reaction <- abs(Z.reaction) / sqrt(n.control.rt + n.treatment.rt)
print(paste("Effect size r =", round(r.effect.reaction, 3)))


# ============= Visualization of Ranks =====================

# Boxplot of ranks for score
ranked.boxplot <- ggplot(data, aes(x = group_numeric, y = rank_score, fill = group_numeric)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightskyblue1", "deepskyblue4")) +
  labs(x = "Group", 
       y = "Rank of Cognitive Score", 
       title = "Distribution of Ranks: Mann-Whitney U Test") +
  theme_minimal() +
  theme(legend.position = "none")
ranked.boxplot










