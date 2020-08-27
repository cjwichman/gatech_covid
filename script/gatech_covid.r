library('rvest')
library('dplyr')
library('doBy')
library('ggplot2')


# this code pulls coronavirus data from Georgia Tech's health alerts webpage 
# (https://health.gatech.edu/coronavirus/health-alerts), complies the data 
# into a .csv, and plots daily cases.


rm(list=ls())
dir <- "~/Dropbox/gatech_covid"



# grab data from web
url <- "https://health.gatech.edu/coronavirus/health-alerts"
html.text <- html_text(html_nodes(read_html(url), "td"))




# grab total summary table
total <- data.frame(t(html.text[1:3]))
names(total) <- c("updated", "new_reports", "total_reported")
total




# grab "current month" summary
cases <- data.frame(matrix(html.text[4:length(html.text)], ncol=4, byrow=TRUE))
names(cases) <- c("date_reported", "position", "date_last_on_campus", "campus_impact")
# remove headers buried in the table
cases <- subset(cases, date_reported!="\n\t\t\t\tDate Reported\n\t\t\t\t")




# count number of cases (requires some manual cleaning)
table(cases$position)
cases$cases_count <- 1
cases$cases_count[cases$position == "Students" & cases$date_reported == "August 22, 2020"] <- 13
cases$cases_count[cases$position == "Students (3)"] <- 3
cases$cases_count[cases$position == "Students (6)"] <- 6
cases$cases_count[cases$position == "Various" & cases$date_reported == "August 22, 2020"] <- 12




# clean-up dates
cases$date_reported <- as.Date(cases$date_reported, format="%B %d, %Y")
cases$date_last_on_campus <- as.Date(cases$date_last_on_campus, format="%B %d, %Y")
  
sum(cases$cases_count) # 4 cases fewer than "total reported since March 2020 (302)





# collapse cases to daily totals
cases_bydate <- summaryBy(cases_count ~ date_reported, data=cases, FUN=sum) 
# fill-in missing dates with zeros
days <- data.frame(date=seq(min(cases_bydate$date_reported), 
                            max(cases_bydate$date_reported), 
                            by="days"))
cases_bydate <- merge(cases_bydate, days, 
                      by.x="date_reported", by.y="date", 
                      all.x=TRUE, all.y=TRUE)
cases_bydate$cases_count.sum[is.na(cases_bydate$cases_count.sum)] <- 0

# subset to august only
cases_bydate.august <- subset(cases_bydate, date_reported >= "2020-08-01")




# set date
date <- Sys.Date()




# plot daily cases for August 2020
ggplot(data=cases_bydate.august, 
       aes(x=date_reported, y=cases_count.sum)) +
  geom_line(color="gold", size=1.1) +
  geom_point(size=2) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-08-17")), linetype=2) +
  geom_text(x=as.numeric(as.Date("2020-08-17")), y=50, 
            label="First day of class ", hjust=1,
            family = "Courier") +
  ggtitle(paste0("Daily COVID-19 cases at Georgia Tech \n(as of ", date, ")")) +
  xlab("Date reported") +
  ylab("Number of daily reported cases") +
  theme_classic() + 
  theme(text=element_text(family="Courier", size=12),
        axis.text.x = element_text(angle = 30, vjust=1, hjust=1))
ggsave(filename = paste0(dir, "/daily_cases_august_", date, ".png"),
       width = 10, height = 6, units = "in")


# plot daily cases since reporting began
ggplot(data=cases_bydate, 
       aes(x=date_reported, y=cases_count.sum)) +
  geom_line(color="gold", size=1.1) +
  geom_point(size=2) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-08-17")), linetype=2) +
  geom_text(x=as.numeric(as.Date("2020-08-17")), y=50, 
            label="First day of class ", hjust=1,
            family = "Courier") +
  ggtitle(paste0("Daily COVID-19 cases at Georgia Tech \n(as of ", date, ")")) +
  xlab("Date reported") +
  ylab("Number of daily reported cases") +
  theme_classic() + 
  theme(text=element_text(family="Courier", size=12),
        axis.text.x = element_text(angle = 30, vjust=1, hjust=1))
ggsave(filename = paste0(dir, "/daily_cases_", date, ".png"),
       width = 10, height = 6, units = "in")



# save detailed data as .csv
write.csv2(cases, paste0(dir, "/gatech_cases_", date, ".csv"))
