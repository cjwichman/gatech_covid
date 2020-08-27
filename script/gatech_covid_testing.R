library('rvest')
library('dplyr')
library('doBy')
library('ggplot2')


# this code pulls coronavirus data from Georgia Tech's health alerts webpage 
# (https://health.gatech.edu/surveillance-testing-program-results), complies the data 
# into a .csv, and plots cumulative tests


rm(list=ls())
dir <- "~/Dropbox/gatech_covid"



# grab data from web
url <- "https://health.gatech.edu/surveillance-testing-program-results"
html.text <- html_text(html_nodes(read_html(url), "td"))



# grab cumulative summary table
total <- data.frame(matrix(unlist(html.text[1:4]), ncol=2, byrow=TRUE))
names(total) <- c("updated", "total_reported")
total



# grab tests data
tests <- data.frame(matrix(unlist(html.text[5:length(html.text)]), ncol=3, byrow=TRUE))
names(tests) <- c("date", "tests_positive", "tests_processed")
tests$tests_positive[tests$tests_positive == "As testing typically does not take place on Sundays, no surveillance samples were processed on 8/24."] <- 0
tests$tests_processed[tests$tests_processed == "As testing typically does not take place on Sundays, no surveillance samples were processed on 8/24."] <- 0

# remove characters and convert to numeric
tests$tests_positive <- gsub("\n", "", tests$tests_positive)
tests$tests_positive <- gsub(",", "", tests$tests_positive)
tests$tests_processed <- gsub("\n", "", tests$tests_processed)
tests$tests_processed <- gsub(",", "", tests$tests_processed)
tests$tests_positive <- as.numeric(tests$tests_positive)
tests$tests_processed <- as.numeric(tests$tests_processed)

# clean-up dates
tests$date <- gsub("\n", "", tests$date)
tests$date[tests$date == "August 9-14, 2020"] <- "August 14, 2020"
tests$date <- as.Date(tests$date, format="%B %d, %Y")


# calculate cumulative sum of tests & and no. positive
tests <- tests[order(tests$date),] # sort dates
tests$cumulative_tests_processed <- cumsum(tests$tests_processed)



# plot cumulative tests since reporting began
date <- Sys.Date()
ggplot(data=tests, 
       aes(x=date, y=cumulative_tests_processed)) +
  geom_line(color="blue", size=1.1) +
  geom_point(size=2) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-08-17")), linetype=2) +
  geom_text(x=as.numeric(as.Date("2020-08-17")), y=10500, 
            label="First day of class ", hjust=1,
            family = "Courier") +
  ggtitle(paste0("Cumulative no. of samples tested via Georgia Tech surveillance testing \n(as of ", date, ")")) +
  xlab("Date reported") +
  ylab("Individual samples tested") +
  theme_classic() + 
  theme(text=element_text(family="Courier", size=12),
        axis.text.x = element_text(angle = 30, vjust=1, hjust=1))
ggsave(filename = paste0(dir, "/figures/cumulative_tests_", date, ".png"),
       width = 10, height = 6, units = "in")




# save testing data as .csv
write.csv2(tests, paste0(dir, "/data/gatech_tests_", date, ".csv"))
