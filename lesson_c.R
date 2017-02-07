intro_df <- read.csv('data/course_NWISdata.csv', stringsAsFactors = FALSE,
                     colClasses = c('character', rep(NA, 6)))


high_temp <- intro_df[intro_df["Wtemp_Inst"] > 15,]


estimated_q <- intro_df["Flow_Inst"][intro_df['Flow_Inst_cd'] == 'E']

library("dplyr")


dplyr_sel <- select(intro_df, site_no, dateTime, DO_Inst)


dplyr_high_temp <- filter(intro_df, Wtemp_Inst > 15)

dplyr_estmated_q <- filter(intro_df, Flow_Inst_cd == "E")


intro_df_newcolumn <- mutate(intro_df, DO_mgmL = DO_Inst/1000)

intro_df <- rename(intro_df,
                   Flow = Flow_Inst,
                   Flow_cd = Flow_Inst_cd,
                   Wtemp = Wtemp_Inst,
                   pH = pH_Inst,
                   DO = DO_Inst)

# EXERCISE 1
# question 1
no_flow_cd <- select(intro_df, -(Flow_cd))
# question 2
greater_than_10_cu_ft <- filter(no_flow_cd, Flow > 10)
# question 3
feet_to_meter_conversion <- 3.28 # ft/m
df <- mutate(greater_than_10_cu_ft, flow_cu_meters = Flow*(feet_to_meter_conversion**3))



#Let's first create a new small example data.frame
new_data <- data.frame(site_no=rep("00000001", 3), 
                       dateTime=c("2016-09-01 07:45:00", "2016-09-02 07:45:00", "2016-09-03 07:45:00"), 
                       Wtemp=c(14.0, 16.4, 16.0),
                       pH = c(7.8, 8.5, 8.3),
                       stringsAsFactors = FALSE)

bind_rows_df <- bind_rows(intro_df, new_data)


# read forgotten DO and discharge data
forgotten_data <- read.csv('data/forgottenData.csv', stringsAsFactors = FALSE, 
                           colClasses = c(rep("character",2),rep("numeric",3)))

left_join(new_data, forgotten_data, by=c("site_no", "dateTime"))


# EXERCISE 2
# could use sample_n() to select random rows, but we want everyone in the class to have the same values
# e.g. sample_n(intro_df, size=20)

#subset intro_df
rows2keep <- c(1634, 1123, 2970, 1052, 2527, 1431, 2437, 1877, 2718, 2357, 
               1290, 225, 479, 1678, 274, 1816, 418, 1777, 611, 2993)
intro_df_subset <- intro_df[rows2keep,]

# keep only flow for one dataframe
Q <- select(intro_df_subset, site_no, dateTime, Flow)

# select 8 values and only keep DO for second dataframe
DO <- intro_df_subset[c(1, 5, 7, 9, 12, 16, 17, 19),]
DO <- select(DO, site_no, dateTime, DO)
# question 2
DO_Q <- left_join(DO, Q, by=c('site_no', 'dateTime'))
# question 3
Q_DO <- right_join(DO, Q, by=c('site_no', 'dateTime'))
# question 4
Q_DO_NO_NA <- na.omit(Q_DO)


intro_df_grouped <- group_by(intro_df, site_no)
intro_df_summary <- summarize(intro_df_grouped, mean(Flow), mean(Wtemp))
intro_df_summary <- summarize(intro_df_grouped, mean(Flow, na.rm=TRUE), mean(Wtemp, na.rm=TRUE))


intro_df_2DO <- mutate(intro_df, DO_2 = runif(n=nrow(intro_df), min = 5.0, max = 18.0))
intro_df_2DO_byrow <- rowwise(intro_df_2DO)


intro_df_DO_max <- mutate(intro_df_2DO_byrow, max_DO = max(DO, DO_2))


# EXERCISE 3
# question 1
site_name_group <- group_by(intro_df, site_no)
intro_df_max_site_temp <- summarize(site_name_group, max(Wtemp, na.rm=TRUE))
# question 2
pH_group <- group_by(intro_df, pH)
intro_df_mean_pH_temp <- summarize(pH_group, mean(Wtemp, na.rm=TRUE))