intro_df <- read.csv("data/course_NWISdata_cleaned.csv", stringsAsFactors = FALSE, 
                     colClasses = c("character", rep(NA, 7)))


library(dplyr)
intro_df_est <- filter(intro_df, Flow_cd == "A e")
intro_df_est_QpH <- select(intro_df_est, Flow, DO)
intro_df_appr <-filter(intro_df, Flow_cd == "A") 
intro_df_appr_QpH <- select(intro_df_appr, Flow, DO)

plot(intro_df_appr_QpH$Flow, intro_df_appr_QpH$DO, pch=16, col='#FF5034')
points(intro_df_est_QpH$Flow, intro_df_est_QpH$DO, pch=16, col='skyblue')

#EXERCISE 1
# question 1
intro_df_subset = select(intro_df, site_no, DO, Wtemp)
site_1 = filter(intro_df_subset, site_no=="02203700")
site_2 = filter(intro_df_subset, site_no=="02336120")
plot(site_1$DO, site_1$Wtemp, pch=1)
points(site_2$DO, site_2$Wtemp, pch=6)
legend(x="topright", legend=c("Site 1", "Site 2"),
       pch=c(1, 6), title="Legend")

plot(intro_df$Flow, intro_df$Wtemp, pch=20)
#add a second y-axis
axis(side=4)


plot(intro_df$Flow, intro_df$Wtemp,  pch=20, log='x')
#format the second y-axis to have tick marks at every concentration (not just every 5) & no labels
axis(side=4, at=1:20, labels=FALSE)
#add a second x-axis
axis(side=3) #this axis is also logged


layout_matrix <- matrix(c(1:4), nrow=2, ncol=2, byrow=TRUE)
layout(layout_matrix)

#four boxplots:
plot1 <- boxplot(intro_df$Flow ~ intro_df$site_no, ylab="Discharge, cfs", main="Discharge")
plot2 <- boxplot(intro_df$Wtemp ~ intro_df$site_no, ylab="Temperature, deg C", main="Water Temp")
plot3 <- boxplot(intro_df$pH ~ intro_df$site_no, ylab="pH", main="pH")
plot4 <- boxplot(intro_df$DO ~ intro_df$site_no, ylab="D.O. Concentration, mg/L", main="Dissolved Oxygen")

dev.off()


#EXERCISE 2
# question 1
site_3 <- filter(intro_df, site_no=='02336410')
layout_matrix <- matrix(c(1, 2, 1, 0), 2, 2)
layout(layout_matrix)
plot_1 <- plot(site_3$Flow, site_3$DO, col="red", log="x")
axis(side=3)
axis(side=4)

plot_2 <- plot(site_3$Flow, site_3$pH, col="blue", log="x")
dev.off()
