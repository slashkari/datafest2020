# Model Behavior
# Zoe Chen - zoechen17@gmail.com
# Rene Honarchian - rhonarchian@gmail.com
# Sean Lashkari - slash5324@hotmail.com
# Megan Li - meg.li494@gmail.com
# Zach Loran - zachloran@gmail.com

library(tidyverse)

# read in iowa liquor sales data by year
iowa_2016 <- read_csv("Iowa_Liquor_Sales_2016.csv")
iowa_2017 <- read_csv("Iowa_Liquor_Sales_2017.csv")
iowa_2018 <- read_csv("Iowa_Liquor_sales_2018.csv")
iowa_2019 <- read_csv("Iowa_Liquor_Sales_2019.csv")
iowa_2020 <- read_csv("Iowa_Liquor_Sales_2020.csv")

# convert dates to date class
iowa_2016$Date <- as.Date(iowa_2016$Date, "%m/%d/%y")
iowa_2017$Date <- as.Date(iowa_2017$Date, "%m/%d/%y")
iowa_2018$Date <- as.Date(iowa_2018$Date, "%m/%d/%y")
iowa_2019$Date <- as.Date(iowa_2019$Date, "%m/%d/%y")
iowa_2020$Date <- as.Date(iowa_2020$Date, "%m/%d/%y")

# add months column
iowa_2016 <- as_tibble(cbind(iowa_2016, Month = months(iowa_2016$Date)))
iowa_2017 <- as_tibble(cbind(iowa_2017, Month = months(iowa_2017$Date)))
iowa_2018 <- as_tibble(cbind(iowa_2018, Month = months(iowa_2018$Date)))
iowa_2019 <- as_tibble(cbind(iowa_2019, Month = months(iowa_2019$Date)))
iowa_2020 <- as_tibble(cbind(iowa_2020, Month = months(iowa_2020$Date)))

# summary stats by year
# 2016
sum_2016 <- iowa_2016 %>% group_by(Month) %>% summarise(mean_bottles_sold = mean(`Bottles Sold`),
                        standev_bottles_sold = sd(`Bottles Sold`),
                        mean_sales = mean(`Sale (Dollars)`),
                        sd_sales = sd(`Sale (Dollars)`),
                        mean_volume_sold = mean(`Volume Sold (Liters)`),
                        sd_volume_sold = sd(`Volume Sold (Liters)`))

# 2017
sum_2017 <- iowa_2017 %>% group_by(Month) %>% summarise(mean_bottles_sold = mean(`Bottles Sold`),
                        standev_bottles_sold = sd(`Bottles Sold`),
                        mean_sales = mean(`Sale (Dollars)`),
                        sd_sales = sd(`Sale (Dollars)`),
                        mean_volume_sold = mean(`Volume Sold (Liters)`),
                        sd_volume_sold = sd(`Volume Sold (Liters)`))

# 2018
sum_2018 <- iowa_2018 %>% group_by(Month) %>% summarise(mean_bottles_sold = mean(`Bottles Sold`),
                        standev_bottles_sold = sd(`Bottles Sold`),
                        mean_sales = mean(`Sale (Dollars)`),
                        sd_sales = sd(`Sale (Dollars)`),
                        mean_volume_sold = mean(`Volume Sold (Liters)`),
                        sd_volume_sold = sd(`Volume Sold (Liters)`))

# 2019
sum_2019 <- iowa_2019 %>% group_by(Month) %>% summarise(mean_bottles_sold = mean(`Bottles Sold`),
                        standev_bottles_sold = sd(`Bottles Sold`),
                        mean_sales = mean(`Sale (Dollars)`),
                        sd_sales = sd(`Sale (Dollars)`),
                        mean_volume_sold = mean(`Volume Sold (Liters)`),
                        sd_volume_sold = sd(`Volume Sold (Liters)`))

# 2020
sum_2020 <- iowa_2020 %>% group_by(Month) %>% summarise(mean_bottles_sold = mean(`Bottles Sold`),
                        standev_bottles_sold = sd(`Bottles Sold`),
                        mean_sales = mean(`Sale (Dollars)`),
                        sd_sales = sd(`Sale (Dollars)`),
                        mean_volume_sold = mean(`Volume Sold (Liters)`),
                        sd_volume_sold = sd(`Volume Sold (Liters)`))

# combine years and add year column
all_years <- rbind(sum_2016, sum_2017, sum_2018, sum_2019, sum_2020)
all_years <- all_years[, -c(3, 5, 7)]
all_years <- cbind(all_years, factor(rep(c(2016, 2017, 2018, 2019, 2020), each = 2)))
colnames(all_years) <- c("month", "mean_bottles_sold", "mean_sales", "mean_volume_sold_liters", "year")


# bargraph of average bottles sold by year split per month
ggplot(all_years, aes(x = month, y = mean_bottles_sold, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(x = "Month", y = "Average Number of Bottles Sold Per Day") +
  ggtitle("Average Number of Bottles Sold Per Day By Month")

ggsave(filename  = "bottles_sold.png")

# bargraph of average sales per day by month
ggplot(all_years, aes(x = month, y = mean_sales, fill = year)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Month", y = "Average Sales (Dollars) Per Day") +
  ggtitle("Average Daily Sales (Dollars) By Month")

ggsave(filename = "sales.png")

# ANOVA for bottles sold
df <- data.frame(bottles_sold = c(iowa_2016$`Bottles Sold`, iowa_2017$`Bottles Sold`,
                                  iowa_2018$`Bottles Sold`, iowa_2019$`Bottles Sold`,
                                  iowa_2020$`Bottles Sold`),
                 year = factor(c(rep(2016, nrow(iowa_2016)),
                                 rep(2017, nrow(iowa_2017)),
                                 rep(2018, nrow(iowa_2018)),
                                 rep(2019, nrow(iowa_2019)),
                                 rep(2020, nrow(iowa_2020)))))


anova(lm(bottles_sold ~ year, df))

# ANOVA for sales
df2 <- data.frame(sales = c(iowa_2016$`Sale (Dollars)`, iowa_2017$`Sale (Dollars)`,
                            iowa_2018$`Sale (Dollars)`, iowa_2019$`Sale (Dollars)`,
                            iowa_2020$`Sale (Dollars)`),
                  year = factor(c(rep(2016, nrow(iowa_2016)),
                                  rep(2017, nrow(iowa_2017)),
                                  rep(2018, nrow(iowa_2018)),
                                  rep(2019, nrow(iowa_2019)),
                                  rep(2020, nrow(iowa_2020)))))

anova(lm(sales ~ year, df2))

# lineplot of sales per year
# mean from each year 
sales <- c(128.0261, 131.8476, 135.692, 138.9683, 145.507)
df_sales <- data.frame(year = c("2016", "2017", "2018", "2019", "2020"), sales_in_dollars = sales)
ggplot(data=df_sales, aes(x=year, y=sales_in_dollars, group=1)) + geom_line() +
  geom_point()

ggsave(filename = "lineplot.png")

# iowa differences march
march_first <- read_csv("03-22-2020.csv")
march_last2 <- read_csv("03-31-2020.csv")

# iowa differences april
april_first <- read_csv("04-01-2020.csv")
april_last <- read_csv("04-30-2020.csv")

march_first <- march_first %>% filter(Province_State == "Iowa")
march_last <- march_last %>% filter(Province_State == "Iowa")
april_first <- april_first %>% filter(Province_State == "Iowa")
april_last <- april_last %>% filter(Province_State == "Iowa")

march_first <- march_first[, -c(1, 4, 12)]
march_last <- march_last[, -c(1, 4, 12)]
april_first <- april_first[, -c(1, 4, 12)]
april_last <- april_last[, -c(1, 4, 12)]


