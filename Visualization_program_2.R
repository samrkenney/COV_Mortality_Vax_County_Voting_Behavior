County_Table <- read.csv("COVID_Mortality_Trump_Voters_1.csv", header = TRUE, sep = ",")

# The following code attempts to evaluate the strength and direction of any correlation between: Percent of a County's vote share for the GOP and
# COVID Mortality, Total Mortality, Vax Rate - all ages, 5+, 12+, 18+, and 65+, respectively. The code below evaluates our data for meeting
# the requirements of The Pearson's Correlation Coefficient and Spearman's Rank Correlation. 

#===== Outlier Removal: IQR method

# COVID Deaths
Q1_a1 <- quantile(County_Table$GOP_2020, 0.25)
Q3_a1 <- quantile(County_Table$GOP_2020, 0.75)
IQR_a1 <- IQR(County_Table$GOP_2020)
County_Table_a1 <- subset(County_Table,
                          County_Table$GOP_2020 > (Q1_a1 - 1.5 * IQR_a1) &
                          County_Table$GOP_2020 < (Q3_a1 + 1.5 * IQR_a1))

Q1_a2 <- quantile(County_Table_a1$COVID_Deaths_Per_10K, 0.25)
Q3_a2 <- quantile(County_Table_a1$COVID_Deaths_Per_10K, 0.75)
IQR_a2 <- IQR(County_Table_a1$COVID_Deaths_Per_10K)
County_Table_excl_COV_mort_outliers1 <- subset(County_Table_a1,
                                               County_Table_a1$COVID_Deaths_Per_10K > (Q1_a2 - 1.5 * IQR_a2) &
                                               County_Table_a1$COVID_Deaths_Per_10K < (Q3_a2 + 1.5 * IQR_a2)
                                               )[c("county_state", "GOP_2020", "COVID_Deaths_Per_10K")]

# All deaths
Q1_b2 <- quantile(County_Table_a1$All_Deaths_Per_10K, 0.25)
Q3_b2 <- quantile(County_Table_a1$All_Deaths_Per_10K, 0.75)
IQR_b2 <- IQR(County_Table_a1$All_Deaths_Per_10K)
County_Table_excl_total_mort_outliers1 <- subset(County_Table_a1,
                                                 County_Table_a1$All_Deaths_Per_10K > (Q1_b2 - 1.5 * IQR_b2) &
                                                 County_Table_a1$All_Deaths_Per_10K < (Q3_b2 + 1.5 * IQR_b2)
                                                 )[c("county_state", "GOP_2020", "All_Deaths_Per_10K")]

# Median age
Q1_c2 <- quantile(County_Table_a1$median_age, 0.25)
Q3_c2 <- quantile(County_Table_a1$median_age, 0.75)
IQR_c2 <- IQR(County_Table_a1$median_age)
County_Table_excl_med_age_outliers1 <- subset(County_Table_a1,
                                              County_Table_a1$median_age > (Q1_c2 - 1.5 * IQR_c2) &
                                              County_Table_a1$median_age < (Q3_c2 + 1.5 * IQR_c2)
                                              )[c("county_state", "GOP_2020", "median_age")]

# Vaccination All Ages
Q1_d2 <- quantile(County_Table_a1$Vax_Pct, 0.25)
Q3_d2 <- quantile(County_Table_a1$Vax_Pct, 0.75)
IQR_d2 <- IQR(County_Table_a1$Vax_Pct)
County_Table_excl_vax_all_outliers1 <- subset(County_Table_a1,
                                              County_Table_a1$Vax_Pct > (Q1_d2 - 1.5 * IQR_d2) &
                                              County_Table_a1$Vax_Pct < (Q3_d2 + 1.5 * IQR_d2)
                                              )[c("county_state", "GOP_2020", "Vax_Pct")]

# Vaccination Ages 5+
Q1_e2 <- quantile(County_Table_a1$Vax_5Plus_Pct, 0.25)
Q3_e2 <- quantile(County_Table_a1$Vax_5Plus_Pct, 0.75)
IQR_e2 <- IQR(County_Table_a1$Vax_5Plus_Pct)
County_Table_excl_vax_5plus_outliers1 <- subset(County_Table_a1,
                                                County_Table_a1$Vax_5Plus_Pct > (Q1_e2 - 1.5 * IQR_e2) &
                                                County_Table_a1$Vax_5Plus_Pct < (Q3_e2 + 1.5 * IQR_e2)
                                                )[c("county_state", "GOP_2020", "Vax_5Plus_Pct")]

# Vaccination Ages 12+
Q1_f2 <- quantile(County_Table_a1$Vax_12Plus_Pct, 0.25)
Q3_f2 <- quantile(County_Table_a1$Vax_12Plus_Pct, 0.75)
IQR_f2 <- IQR(County_Table_a1$Vax_12Plus_Pct)
County_Table_excl_vax_12plus_outliers1 <- subset(County_Table_a1,
                                                 County_Table_a1$Vax_12Plus_Pct > (Q1_f2 - 1.5 * IQR_f2) &
                                                 County_Table_a1$Vax_12Plus_Pct < (Q3_f2 + 1.5 * IQR_f2)
                                                 )[c("county_state", "GOP_2020", "Vax_12Plus_Pct")]

# Vaccination Ages 18+
Q1_g2 <- quantile(County_Table_a1$Vax_18Plus_Pct, 0.25)
Q3_g2 <- quantile(County_Table_a1$Vax_18Plus_Pct, 0.75)
IQR_g2 <- IQR(County_Table_a1$Vax_18Plus_Pct)
County_Table_excl_vax_18plus_outliers1 <- subset(County_Table_a1,
                                                 County_Table_a1$Vax_18Plus_Pct > (Q1_g2 - 1.5 * IQR_g2) &
                                                 County_Table_a1$Vax_18Plus_Pct < (Q3_g2 + 1.5 * IQR_g2)
                                                 )[c("county_state", "GOP_2020", "Vax_18Plus_Pct")]

# Vaccination Ages 65+
Q1_h2 <- quantile(County_Table_a1$Vax_65Plus_Pct, 0.25)
Q3_h2 <- quantile(County_Table_a1$Vax_65Plus_Pct, 0.75)
IQR_h2 <- IQR(County_Table_a1$Vax_65Plus_Pct)
County_Table_excl_vax_65plus_outliers1 <- subset(County_Table_a1,
                                                 County_Table_a1$Vax_65Plus_Pct > (Q1_h2 - 1.5 * IQR_h2) &
                                                 County_Table_a1$Vax_65Plus_Pct < (Q3_h2 + 1.5 * IQR_h2)
                                                 )[c("county_state", "GOP_2020", "Vax_65Plus_Pct")]

#===== Outlier Removal: z-score method

# add z-score columns to temp table for each variable
County_Table_A <- County_Table #temp table
County_Table_A$z_GOP_2020 <- (abs(County_Table_A$GOP_2020 - mean(County_Table_A$GOP_2020))/sd(County_Table_A$GOP_2020))
County_Table_A$z_COVID_Deaths <- (abs(County_Table_A$COVID_Deaths_Per_10K - mean(County_Table_A$COVID_Deaths_Per_10K))/sd(County_Table_A$COVID_Deaths_Per_10K))
County_Table_A$z_All_Deaths <- (abs(County_Table_A$All_Deaths_Per_10K - mean(County_Table_A$All_Deaths_Per_10K))/sd(County_Table_A$All_Deaths_Per_10K))
County_Table_A$z_med_age <- (abs(County_Table_A$median_age - mean(County_Table_A$median_age))/sd(County_Table_A$median_age))
County_Table_A$z_vax_all <- (abs(County_Table_A$Vax_Pct - mean(County_Table_A$Vax_Pct))/sd(County_Table_A$Vax_Pct))
County_Table_A$z_vax_5plus <- (abs(County_Table_A$Vax_5Plus_Pct - mean(County_Table_A$Vax_5Plus_Pct))/sd(County_Table_A$Vax_5Plus_Pct))
County_Table_A$z_vax_12plus <- (abs(County_Table_A$Vax_12Plus_Pct - mean(County_Table_A$Vax_12Plus_Pct))/sd(County_Table_A$Vax_12Plus_Pct))
County_Table_A$z_vax_18plus <- (abs(County_Table_A$Vax_18Plus_Pct - mean(County_Table_A$Vax_18Plus_Pct))/sd(County_Table_A$Vax_18Plus_Pct))
County_Table_A$z_vax_65plus <- (abs(County_Table_A$Vax_65Plus_Pct - mean(County_Table_A$Vax_65Plus_Pct))/sd(County_Table_A$Vax_65Plus_Pct))

# create new tables and filter out z-scores > 3
County_Table_B <- subset(County_Table_A,
                         County_Table_A$z_GOP_2020 < 3) #temp table

County_Table_excl_COV_mort_outliers2 <- subset(County_Table_B,
                                               County_Table_B$z_COVID_Deaths < 3
                                               )[c("county_state", "GOP_2020", "COVID_Deaths_Per_10K")]

County_Table_excl_total_mort_outliers2 <- subset(County_Table_B,
                                                 County_Table_B$z_All_Deaths < 3
                                                 )[c("county_state", "GOP_2020", "All_Deaths_Per_10K")]

County_Table_excl_med_age_outliers2 <- subset(County_Table_B,
                                              County_Table_B$z_med_age < 3
                                              )[c("county_state", "GOP_2020", "median_age")]

County_Table_excl_vax_all_outliers2 <- subset(County_Table_B,
                                              County_Table_B$z_vax_all < 3
                                              )[c("county_state", "GOP_2020", "Vax_Pct")]

County_Table_excl_vax_5plus_outliers2 <- subset(County_Table_B,
                                                County_Table_B$z_vax_5plus < 3
                                                )[c("county_state", "GOP_2020", "Vax_5Plus_Pct")]

County_Table_excl_vax_12plus_outliers2 <- subset(County_Table_B,
                                                 County_Table_B$z_vax_12plus < 3
                                                 )[c("county_state", "GOP_2020", "Vax_12Plus_Pct")]

County_Table_excl_vax_18plus_outliers2 <- subset(County_Table_B,
                                                 County_Table_B$z_vax_18plus < 3
                                                 )[c("county_state", "GOP_2020", "Vax_18Plus_Pct")]

County_Table_excl_vax_65plus_outliers2 <- subset(County_Table_B,
                                                 County_Table_B$z_vax_65plus < 3
                                                 )[c("county_state", "GOP_2020", "Vax_65Plus_Pct")]

#===== Test variables for Pearson's Correlation Coefficient requirements

# structure variables into list of vectors
IQR_vectors <- list(County_Table_a1$GOP_2020,
                    County_Table_excl_COV_mort_outliers1$COVID_Deaths_Per_10K,
                    County_Table_excl_total_mort_outliers1$All_Deaths_Per_10K,
                    County_Table_excl_med_age_outliers1$median_age,
                    County_Table_excl_vax_all_outliers1$Vax_Pct,
                    County_Table_excl_vax_5plus_outliers1$Vax_5Plus_Pct,
                    County_Table_excl_vax_12plus_outliers1$Vax_12Plus_Pct,
                    County_Table_excl_vax_18plus_outliers1$Vax_18Plus_Pct,
                    County_Table_excl_vax_65plus_outliers1$Vax_65Plus_Pct)

Z_vectors <- list(County_Table_B$GOP_2020,
                  County_Table_excl_COV_mort_outliers2$COVID_Deaths_Per_10K,
                  County_Table_excl_total_mort_outliers2$All_Deaths_Per_10K,
                  County_Table_excl_med_age_outliers2$median_age,
                  County_Table_excl_vax_all_outliers2$Vax_Pct,
                  County_Table_excl_vax_5plus_outliers2$Vax_5Plus_Pct,
                  County_Table_excl_vax_12plus_outliers2$Vax_12Plus_Pct,
                  County_Table_excl_vax_18plus_outliers2$Vax_18Plus_Pct,
                  County_Table_excl_vax_65plus_outliers2$Vax_65Plus_Pct)

vars <- c("GOP Percent County Vote",
          "COVID Mortality Per Capita",
          "Total Mortality Per Capita",
          "Median Age",
          "Vax Rate (all ages)",
          "Vax Rate (5+)",
          "Vax Rate (12+)",
          "Vax Rate (18+)",
          "Vax Rate (65+)")

# Pearson - Q-Q Normality visual test on data cleaned with IQR outlier-exclusion

par(mfrow = c(3, 3))
for (i in 1:9) {
  qqnorm(IQR_vectors[[i]], pch = 16, main = vars[i])
  qqline(IQR_vectors[[i]], col = "steelblue", lwd = 2)
}
mtext("Q-Q Normality Test for Data Cleaned by IQR Outlier Exclusion", side = 3, line = -1.2, outer = TRUE)

# all IQR-cleaned variables appear to fail Q-Q visual test for normality

# Pearson - Q-Q Normality visual test on data cleaned with Z-score outlier-exclusion

par(mfrow = c(3, 3))
for (i in 1:9) {
  qqnorm(Z_vectors[[i]], pch = 16, main = vars[i])
  qqline(Z_vectors[[i]], col = "steelblue", lwd = 2)
}
mtext("Q-Q Normality Test for Data Cleaned by Z-Score Outlier Exclusion", side = 3, line = -1.2, outer = TRUE)

# all Z-score-cleaned variables appear to fail Q-Q visual test for normality 

# Pearson - Shapiro-Wilk normality test on data cleaned with IQR outlier-exclusion

W <- NULL
P <- NULL

for (i in 1:9) {
  w <- shapiro.test(IQR_vectors[[i]])$statistic #not sure about the [1]
  w <- w[[1]]
  W <- append(W, w)
}

for (i in 1:9) {
  p <- shapiro.test(IQR_vectors[[i]])$p.value #not sure about the [1]
  P <- append(P, p)
}

Shapiro_Wilks_IQR <- data.frame(Variables = vars,
                                W_Statistic = W,
                                P_Value = P)
Shapiro_Wilks_IQR

# all IQR-cleaned variables fail the Shapiro-Wilks test for normality

# Pearson - Shapiro-Wilk normality test on data cleaned with Z-score outlier-exclusion

W <- NULL
P <- NULL

for (i in Z_vectors) {
  w <- shapiro.test(i)$statistic #not sure about the [1]
  w <- w[[1]]
  W <- append(W, w)
}

for (i in Z_vectors) {
  p <- shapiro.test(i)$p.value #not sure about the [1]
  P <- append(P, p)
}

Shapiro_Wilks_Z <- data.frame(Variables = vars,
                              W_Statistic = W,
                              P_Value = P)
Shapiro_Wilks_Z

# all Z-score-cleaned variables fail the Shapiro-Wilks test for normality

#===== Spearman Rank Correlation

# IQR-filtered variables
IQR_Dataframes <- list(County_Table_excl_COV_mort_outliers1,
                       County_Table_excl_total_mort_outliers1,
                       County_Table_excl_med_age_outliers1,
                       County_Table_excl_vax_all_outliers1,
                       County_Table_excl_vax_5plus_outliers1,
                       County_Table_excl_vax_12plus_outliers1,
                       County_Table_excl_vax_18plus_outliers1,
                       County_Table_excl_vax_65plus_outliers1)

RHO <- NULL

for (x in IQR_Dataframes) {
  rho <- cor.test(x[[2]], x[[3]],
                  method = "spearman",
                  exact = FALSE
                  )$estimate
  rho <- rho[[1]]
  RHO <- append(RHO, rho)
}

P <- NULL

for (x in IQR_Dataframes) {
  p <- cor.test(x[[2]], x[[3]],
                  method = "spearman",
                  exact = FALSE
  )$p.value
  P <- append(P, p)
}

Vars <- c("COVID Mortality Per Capita",
          "Total Mortality Per Capita",
          "Median Age",
          "Vax Rate (all ages)",
          "Vax Rate (5+)",
          "Vax Rate (12+)",
          "Vax Rate (18+)",
          "Vax Rate (65+)")

Spearman_IQR <- data.frame(GOP_Vote_Share = Vars,
                           Rho = RHO,
                           P_Values = P)

# Z-score-filtered variables

Z_Dataframes <- list(County_Table_excl_COV_mort_outliers2,
                     County_Table_excl_total_mort_outliers2,
                     County_Table_excl_med_age_outliers2,
                     County_Table_excl_vax_all_outliers2,
                     County_Table_excl_vax_5plus_outliers2,
                     County_Table_excl_vax_12plus_outliers2,
                     County_Table_excl_vax_18plus_outliers2,
                     County_Table_excl_vax_65plus_outliers2)

RHO <- NULL

for (x in Z_Dataframes) {
  rho <- cor.test(x[[2]], x[[3]],
                  method = "spearman",
                  exact = FALSE
  )$estimate
  rho <- rho[[1]]
  RHO <- append(RHO, rho)
}

P <- NULL

for (x in Z_Dataframes) {
  p <- cor.test(x[[2]], x[[3]],
                method = "spearman",
                exact = FALSE
  )$p.value
  P <- append(P, p)
}

Spearman_Z <- data.frame(GOP_Vote_Share = Vars,
                         Rho = RHO,
                         P_Values = P)

# Summarize Spearman Rank Correlation results

print("Spearman Correlation (IQR-cleaned variables)")
Spearman_IQR
print("Spearman Correlation (Z-score-cleaned variables)")
Spearman_Z

#===== Plot COVID Mortality in Trump-Voting Counties (IQR-cleaned variables)
par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table_excl_COV_mort_outliers1$GOP_2020,
     County_Table_excl_COV_mort_outliers1$COVID_Deaths_Per_10K,
     main = 'COVID Mortality in Trump-Voting Counties (IQR-cleaned)',
     xlab = '% of Trump vote',
     ylab = 'Deaths per 10K County Residents',
     ylim = c(0,max(County_Table_excl_total_mort_outliers2$All_Deaths_Per_10K)),
     pch = 19,
     cex = 0.05,
     col = "red")
points(County_Table_excl_total_mort_outliers1$GOP_2020,
       County_Table_excl_total_mort_outliers1$All_Deaths_Per_10K,
       pch = 19,
       cex = 0.05,
       col = "blue")
legend(x = "topleft",
       text.font = 1,
       legend = c('Deaths from all causes', 'Deaths from COVID-19', 'Age'),
       pch = c(19, 19, 19),
       cex = 0.5,
       col = c("blue", "red", "black"))
abline(lm(COVID_Deaths_Per_10K ~ GOP_2020, data = County_Table_excl_COV_mort_outliers1), col="red", lwd = 2)
abline(lm(All_Deaths_Per_10K ~ GOP_2020, data = County_Table_excl_total_mort_outliers1), col = "blue", lwd = 2)
par(new = TRUE)
plot(County_Table_excl_med_age_outliers1$GOP_2020,
     County_Table_excl_med_age_outliers1$median_age,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(30,55),
     pch = 19,
     cex = 0.05)
axis(side = 4, at = pretty(range(County_Table_excl_med_age_outliers1$median_age)))
mtext("Median County Resident Age", side = 4, line = 3)
abline(lm(median_age ~ GOP_2020, data = County_Table_excl_med_age_outliers1), col="black", lwd = 2)

# ==== Vaccination Rates across Trump-Voting Counties (IQR-cleaned variables)

par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table_excl_vax_all_outliers1$GOP_2020,
     County_Table_excl_vax_all_outliers1$Vax_Pct,
     main = 'COVID Vaccination in Trump-Voting Counties (IQR-cleaned)',
     ylim = c(min(County_Table_excl_vax_all_outliers1$Vax_Pct), max(County_Table_excl_vax_65plus_outliers1$Vax_65Plus_Pct)),
     xlab = '% of Trump vote',
     ylab = '% of County Residents Fully Vaccinated',
     pch = 19,
     cex = 0.1,
     col = "darkblue")
points(County_Table_excl_vax_5plus_outliers1$GOP_2020,
       County_Table_excl_vax_5plus_outliers1$Vax_5Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "purple")
points(County_Table_excl_vax_12plus_outliers1$GOP_2020,
       County_Table_excl_vax_12plus_outliers1$Vax_12Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkgreen")
points(County_Table_excl_vax_18plus_outliers1$GOP_2020,
       County_Table_excl_vax_18plus_outliers1$Vax_18Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkorange")
points(County_Table_excl_vax_65plus_outliers1$GOP_2020,
       County_Table_excl_vax_65plus_outliers1$Vax_65Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkred")
legend(x = "bottomleft",
       text.font = 1,
       legend = c('All ages', '5+', '12+', '18+', '65+'),
       pch = c(19, 19, 19, 19, 19),
       cex = 0.5,
       col = c("darkblue", "purple", "darkgreen", "darkorange", "darkred"))
abline(lm(Vax_Pct ~ GOP_2020, data = County_Table_excl_vax_all_outliers1), col="darkblue", lwd = 2)
abline(lm(Vax_5Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_5plus_outliers1), col = "purple", lwd = 2)
abline(lm(Vax_12Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_12plus_outliers1), col = "darkgreen", lwd = 2)
abline(lm(Vax_18Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_18plus_outliers1), col = "darkorange", lwd = 2)
abline(lm(Vax_65Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_65plus_outliers1), col = "darkred", lwd = 2)

#===== Plot COVID Mortality in Trump-Voting Counties (Z-score-cleaned variables)
par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table_excl_COV_mort_outliers2$GOP_2020,
     County_Table_excl_COV_mort_outliers2$COVID_Deaths_Per_10K,
     main = 'COVID Mortality in Trump-Voting Counties (Z score-cleaned)',
     xlab = '% of Trump vote',
     ylab = 'Deaths per 10K County Residents',
     ylim = c(0,max(County_Table_excl_total_mort_outliers2$All_Deaths_Per_10K)),
     pch = 19,
     cex = 0.05,
     col = "red")
points(County_Table_excl_total_mort_outliers2$GOP_2020,
       County_Table_excl_total_mort_outliers2$All_Deaths_Per_10K,
       pch = 19,
       cex = 0.05,
       col = "blue")
legend(x = "topleft",
       text.font = 1,
       legend = c('Deaths from all causes', 'Deaths from COVID-19', 'Age'),
       pch = c(19, 19, 19),
       cex = 0.5,
       col = c("blue", "red", "black"))
abline(lm(COVID_Deaths_Per_10K ~ GOP_2020, data = County_Table_excl_COV_mort_outliers2), col="red", lwd = 2)
abline(lm(All_Deaths_Per_10K ~ GOP_2020, data = County_Table_excl_total_mort_outliers2), col = "blue", lwd = 2)
par(new = TRUE)
plot(County_Table_excl_med_age_outliers2$GOP_2020,
     County_Table_excl_med_age_outliers2$median_age,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(30,55),
     pch = 19,
     cex = 0.05)
axis(side = 4, at = pretty(range(County_Table_excl_med_age_outliers2$median_age)))
mtext("Median County Resident Age", side = 4, line = 3)
abline(lm(median_age ~ GOP_2020, data = County_Table_excl_med_age_outliers2), col="black", lwd = 2)

# ==== Vaccination Rates across Trump-Voting Counties (Z-score-cleaned variables)

par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table_excl_vax_all_outliers2$GOP_2020,
     County_Table_excl_vax_all_outliers2$Vax_Pct,
     main = 'COVID Vaccination in Trump-Voting Counties (Z score-cleaned)',
     ylim = c(min(County_Table_excl_vax_all_outliers2$Vax_Pct), max(County_Table_excl_vax_65plus_outliers2$Vax_65Plus_Pct)),
     xlab = '% of Trump vote',
     ylab = '% of County Residents Fully Vaccinated',
     pch = 19,
     cex = 0.1,
     col = "darkblue")
points(County_Table_excl_vax_5plus_outliers2$GOP_2020,
       County_Table_excl_vax_5plus_outliers2$Vax_5Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "purple")
points(County_Table_excl_vax_12plus_outliers2$GOP_2020,
       County_Table_excl_vax_12plus_outliers2$Vax_12Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkgreen")
points(County_Table_excl_vax_18plus_outliers2$GOP_2020,
       County_Table_excl_vax_18plus_outliers2$Vax_18Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkorange")
points(County_Table_excl_vax_65plus_outliers2$GOP_2020,
       County_Table_excl_vax_65plus_outliers2$Vax_65Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkred")
legend(x = "bottomleft",
       text.font = 1,
       legend = c('All ages', '5+', '12+', '18+', '65+'),
       pch = c(19, 19, 19, 19, 19),
       cex = 0.5,
       col = c("darkblue", "purple", "darkgreen", "darkorange", "darkred"))
abline(lm(Vax_Pct ~ GOP_2020, data = County_Table_excl_vax_all_outliers2), col="darkblue", lwd = 2)
abline(lm(Vax_5Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_5plus_outliers2), col = "purple", lwd = 2)
abline(lm(Vax_12Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_12plus_outliers2), col = "darkgreen", lwd = 2)
abline(lm(Vax_18Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_18plus_outliers2), col = "darkorange", lwd = 2)
abline(lm(Vax_65Plus_Pct ~ GOP_2020, data = County_Table_excl_vax_65plus_outliers2), col = "darkred", lwd = 2)