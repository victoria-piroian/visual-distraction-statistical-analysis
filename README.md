# Visual Distraction Statistical Analysis - Spot It Game

# Spot the Different Flowers 🌸
**Statistical Analysis of Visual Distraction on Accuracy and Response Time**

## Overview
This project examines how increasing levels of visual distraction affect human accuracy and response time during a visual comparison task. A custom “spot the difference” experiment was designed in which participants identified missing images between two flower cards under varying distraction conditions.

The project combines experimental design, statistical analysis, and probabilistic modeling using R. It was completed as a final statistics project at the University of Toronto, Faculty of Applied Science & Engineering.

---

## Research Question
How does visual distraction impact:
- Accuracy in identifying differences between images?
- Response time required to recall those differences?

---

## Experimental Design

### Participants
- 24 participants
- Ages 19–30
- Gender balanced
- Randomly assigned to one of three difficulty levels (8 participants per level)
- Each participant completed only one difficulty level

### Task
Participants viewed two image cards for 20 seconds and were then asked to recall which figures were missing from one image compared to the other.

### Independent Variable
Visual distraction level (categorical):
1. **Level 1 – Low distraction**  
   High contrast flowers on a white card with a black background.
2. **Level 2 – Medium distraction**  
   Flowers placed on patterned grey cards with reduced visibility.
3. **Level 3 – High distraction**  
   Dense, similarly colored flowers blending into complex backgrounds.

### Dependent Variables
- **Accuracy**:  
  Number of correct differences identified divided by the total number of differences.
- **Response Time**:  
  Time (in seconds) taken to recall all identified differences.

---

## Data Analysis
All analysis was performed in R using `ggplot2`, `readxl`, and base R functions.

Statistical methods include:
- Pearson correlation tests
- Linear regression
- ANOVA
- Binomial probability modeling
- Observed vs. predicted distribution comparisons
- Diagnostic and residual analysis

Accuracy values were normalized to a continuous scale between 0 and 1.

---

## Results
- Accuracy decreases as visual distraction increases  
  (ANOVA, p = 0.01773)
- Response time increases with higher distraction levels  
  (ANOVA, p < 1e-10)
- Participants consistently identified approximately 3–4 differences across all levels, suggesting a limit on short-term visual recall capacity

The results align with prior research indicating that increased visual complexity impairs both speed and accuracy of performance.

---

## How to Run

### Requirements
- R (version 4.0 or higher)
- RStudio (recommended)

### Install Dependencies
```r
install.packages(c("ggplot2", "readxl", "car"))
setwd("path/to/project")
source("code/analysis_main.R")
source("code/probability_models.R")
source("code/visualization.R")
```

### Author
Victoria Piroian
University of Toronto
Faculty of Applied Science & Engineering, 2022
