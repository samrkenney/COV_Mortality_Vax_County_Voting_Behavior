County_Table <- read.csv("COVID_Mortality_Trump_Voters_1.csv", header = TRUE, sep = ",")
Decile_Table <- read.csv("COVID_Mortality_Trump_Voters_2.csv", header = TRUE, sep = ',')

#===== Plot COVID Mortality in Trump-Voting Counties

par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table$GOP_2020,
     County_Table$COVID_Deaths_Per_10K,
     main = 'COVID Mortality in Trump-Voting Counties',
     xlab = '% of Trump vote',
     ylab = 'Deaths per 10K County Residents',
     pch = 19,
     cex = 0.05,
     col = "red")
points(County_Table$GOP_2020,
       County_Table$All_Deaths_Per_10K,
       pch = 19,
       cex = 0.05,
       col = "blue")
legend(x = "topleft",
       text.font = 1,
       legend = c('Deaths from all causes', 'Deaths from COVID-19', 'Age'),
       pch = c(19, 19, 19),
       cex = 0.5,
       col = c("blue", "red", "black"))
abline(lm(COVID_Deaths_Per_10K ~ GOP_2020, data = County_Table), col="red")
abline(lm(All_Deaths_Per_10K ~ GOP_2020, data = County_Table), col = "blue")
par(new = TRUE)
plot(County_Table$GOP_2020,
     County_Table$median_age,
     axes = FALSE,
     xlab = "",
     ylab = "",
     pch = 19,
     cex = 0.05)
axis(side = 4, at = pretty(range(County_Table$median_age)))
mtext("Median County Resident Age", side = 4, line = 3)
abline(lm(median_age ~ GOP_2020, data = County_Table), col="black")

#===== Correlation measurements: COVID Mortality in Trump-Voting Counties

COVID_Death_Summary <- summary(lm(COVID_Deaths_Per_10K ~ GOP_2020, data = County_Table))
All_Death_Summary <- summary(lm(All_Deaths_Per_10K ~ GOP_2020, data = County_Table))
Age_Summary <- summary(lm(median_age ~ GOP_2020, data = County_Table))

intercept1 <- COVID_Death_Summary$coefficients[1,1]
slope1 <- COVID_Death_Summary$coefficients[2,1]
rsq1 <- COVID_Death_Summary$r.squared
rsq_adj1 <- COVID_Death_Summary$adj.r.squared
COVID_Deaths <- c(intercept1, slope1, rsq1, rsq_adj1)

intercept2 <- All_Death_Summary$coefficients[1,1]
slope2 <- All_Death_Summary$coefficients[2,1]
rsq2 <- All_Death_Summary$r.squared
rsq_adj2 <- All_Death_Summary$adj.r.squared
All_Deaths <- c(intercept2, slope2, rsq2, rsq_adj2)

intercept3 <- Age_Summary$coefficients[1,1]
slope3 <- Age_Summary$coefficients[2,1]
rsq3 <- Age_Summary$r.squared
rsq_adj3 <- Age_Summary$adj.r.squared
Age <- c(intercept3, slope3, rsq3, rsq_adj3)

metrics <- c("intercept",
             "slope",
             "r squared",
             "adjusted r squared")

COVID_Mortality_Summary <- data.frame(metrics,
                            COVID_Deaths,
                            All_Deaths,
                            Age)

# ==== Vaccination Rates across Trump-Voting Counties

par(mar = c(5, 4, 4, 4) + 0.3)
plot(County_Table$GOP_2020,
     County_Table$Vax_Pct,
     main = 'COVID Vaccination in Trump-Voting Counties',
     xlab = '% of Trump vote',
     ylab = '% of County Residents Fully Vaccinated',
     pch = 19,
     cex = 0.1,
     col = "darkblue")
points(County_Table$GOP_2020,
       County_Table$Vax_5Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "purple")
points(County_Table$GOP_2020,
       County_Table$Vax_12Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkgreen")
points(County_Table$GOP_2020,
       County_Table$Vax_18Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkorange")
points(County_Table$GOP_2020,
       County_Table$Vax_5Plus_Pct,
       pch = 19,
       cex = 0.1,
       col = "darkred")
legend(x = "bottomleft",
       text.font = 1,
       legend = c('All ages', '5+', '12+', '18+', '65+'),
       pch = c(19, 19, 19, 19, 19),
       cex = 0.5,
       col = c("darkblue", "purple", "darkgreen", "darkorange", "darkred"))
abline(lm(Vax_Pct ~ GOP_2020, data = County_Table), col="darkblue", lwd = 2)
abline(lm(Vax_5Plus_Pct ~ GOP_2020, data = County_Table), col = "purple", lwd = 2)
abline(lm(Vax_12Plus_Pct ~ GOP_2020, data = County_Table), col = "darkgreen", lwd = 2)
abline(lm(Vax_18Plus_Pct ~ GOP_2020, data = County_Table), col = "darkorange", lwd = 2)
abline(lm(Vax_65Plus_Pct ~ GOP_2020, data = County_Table), col = "darkred", lwd = 2)

#==== Correlation Measurements: Vaccination Rates Across Trump-Voting Counties

Vax_Summary <- summary(lm(Vax_Pct ~ GOP_2020, data = County_Table))
Vax5_Summary <- summary(lm(Vax_5Plus_Pct ~ GOP_2020, data = County_Table))
Vax12_Summary <- summary(lm(Vax_12Plus_Pct ~ GOP_2020, data = County_Table))
Vax18_Summary <- summary(lm(Vax_18Plus_Pct ~ GOP_2020, data = County_Table))
Vax65_Summary <- summary(lm(Vax_65Plus_Pct ~ GOP_2020, data = County_Table))

intercept1a <- Vax_Summary$coefficients[1,1]
slope1a <- Vax_Summary$coefficients[2,1]
rsq1a <- Vax_Summary$r.squared
rsq_adj1a <- Vax_Summary$adj.r.squared
All_Ages <- c(intercept1a, slope1a, rsq1a, rsq_adj1a)

intercept2a <- Vax5_Summary$coefficients[1,1]
slope2a <- Vax5_Summary$coefficients[2,1]
rsq2a <- Vax5_Summary$r.squared
rsq_adj2a <- Vax5_Summary$adj.r.squared
Plus_5 <- c(intercept2a, slope2a, rsq2a, rsq_adj2a)

intercept3a <- Vax12_Summary$coefficients[1,1]
slope3a <- Vax12_Summary$coefficients[2,1]
rsq3a <- Vax12_Summary$r.squared
rsq_adj3a <- Vax12_Summary$adj.r.squared
Plus_12 <- c(intercept3a, slope3a, rsq3a, rsq_adj3a)

intercept4a <- Vax18_Summary$coefficients[1,1]
slope4a <- Vax18_Summary$coefficients[2,1]
rsq4a <- Vax18_Summary$r.squared
rsq_adj4a <- Vax18_Summary$adj.r.squared
Plus_18 <- c(intercept4a, slope4a, rsq4a, rsq_adj4a)

intercept5a <- Vax65_Summary$coefficients[1,1]
slope5a <- Vax65_Summary$coefficients[2,1]
rsq5a <- Vax65_Summary$r.squared
rsq_adj5a <- Vax65_Summary$adj.r.squared
Plus_65 <- c(intercept5a, slope5a, rsq5a, rsq_adj5a)

metrics2 <- c("intercept",
             "slope",
             "r squared",
             "adjusted r squared")

COVID_Vaccination_Summary <- data.frame(metrics,
                         All_Ages,
                         Plus_5,
                         Plus_12,
                         Plus_18,
                         Plus_65)
# ===== Return summary tables: Trump-voting county correlation statistics
COVID_Mortality_Summary
COVID_Vaccination_Summary

# ===== Plot COVID Mortality by Trump-Voting County Deciles
Decile_Table <- Decile_Table[order(Decile_Table$Trump_Vote_Decile, decreasing = TRUE),]
Trump_Vote_Decile1 <- c('0-10',
                        '10-20',
                        '20-30',
                        '30-40',
                        '40-50',
                        '50-60',
                        '60-70',
                        '70-80',
                        '80-90',
                        '90-100')
Decile_Table$Trump_Vote_Decile <- Trump_Vote_Decile1

Decile_Table_1 <- data.frame(Decile_Table$Trump_Vote_Decile,
                             Decile_Table$COVID_mortality_Per_10K)
Decile_Table_2 <- data.frame(t(Decile_Table_1[-1]))
colnames(Decile_Table_2) <- Decile_Table_1[, 1]
Decile_Matrix_1 <- as.matrix(Decile_Table_2)

barplot(height = Decile_Matrix_1,
        beside = TRUE,
        ylim = c(0,50),
        main = "COVID Mortality by Trump-share of County Vote",
        xlab = "Share of County Vote for Trump (Decile)",
        ylab = "COVID Deaths per 10K County Residents",
        col = "darkblue")

heights <- Decile_Table$COVID_mortality_Per_10K + c(3,3,3,3,3,3,3,3,3,3)
label <- round(Decile_Table$COVID_mortality_Per_10K, digits = 2)

text(x= seq(from = 1.5, to = 19.5, by = 2),
     y = heights,
     labels = label,
     cex = 0.65
)

# ===== Plot All Mortality by Trump-Voting County Deciles
Decile_Table_3 <- data.frame(Decile_Table$Trump_Vote_Decile,
                             Decile_Table$All_mortality_Per_10K)
Decile_Table_4 <- data.frame(t(Decile_Table_3[-1]))
colnames(Decile_Table_4) <- Decile_Table_3[, 1]
Decile_Matrix_2 <- as.matrix(Decile_Table_4)

barplot(height = Decile_Matrix_2,
        beside = TRUE,
        ylim = c(0,450),
        main = "All Mortality by Trump-share of County Vote",
        xlab = "Share of County Vote for Trump (Decile)",
        ylab = "Deaths per 10K County Residents (all causes)",
        col = "darkblue")

heights <- Decile_Table$All_mortality_Per_10K + c(20,20,20,20,20,20,20,20,20,20)
label <- round(Decile_Table$All_mortality_Per_10K, digits = 2)

text(x= seq(from = 1.5, to = 19.5, by = 2),
     y = heights,
     labels = label,
     cex = 0.65
)

# ===== Plot COVID vaccination among different age groups by Trump-Voting County Deciles
Decile_Table_5 <- data.frame(Decile_Table$Trump_Vote_Decile,
                             Decile_Table$Vax_Pct,
                             Decile_Table$Vax_5Plus_Pct,
                             Decile_Table$Vax_12Plus_Pct,
                             Decile_Table$Vax_18Plus_Pct,
                             Decile_Table$Vax_65Plus_Pct)
Decile_Table_6 <- data.frame(t(Decile_Table_5[-1]))
colnames(Decile_Table_6) <- Decile_Table_5[, 1]
Decile_Matrix_3 <- as.matrix(Decile_Table_6)
barplot(height = Decile_Matrix_3,
        beside = TRUE,
        ylim = c(0,1),
        main = "Vaccination Rate by Trump-Share of County Vote and Age",
        xlab = "Share of County Vote for Trump (Decile)",
        ylab = "% of county residents vaccinated",
        col = c("darkblue", "purple", "darkgreen", "darkorange", "darkred")
)

legend("topright",
       text.font = 1,
       cex = 0.5,
       legend = c("All ages", "5+", "12+", "18+", "65+"),
       fill = c("darkblue", "purple", "darkgreen", "darkorange", "darkred"))
