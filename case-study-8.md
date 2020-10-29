---
title: "CASE STUDY 8"
author: "Ricky Warner"
date: "July 07, 2018"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---









```r
cost <- cost %>%
  mutate(time = ymd_hms(Time, tz = "UTC")) %>%
  mutate(time = with_tz(time, tz = "MST")) %>%
  mutate(month = month(time)) %>%
  mutate(day = wday(time, label = TRUE)) %>%
  mutate(week = week(time)) %>%
  mutate(daymonth = day(time))

negatives <- cost %>%
  filter(Amount < 0) %>%
  filter(Name != "Missing") %>%
  mutate(positive = Amount * -1)

positive <- cost %>%
  filter(Amount > 0) %>%
  filter(Name != "Missing")


week <- positive %>%
  group_by(Name, week) %>%
  summarise(total = sum(Amount))

day <- positive %>%
  group_by(Name, day) %>%
  summarise(total = sum(Amount))

month <- positive %>%
  group_by(Name, month) %>%
  summarise(total = sum(Amount))
```

## Returns 

An important aspect when comparing companies is ensuring that there are no negatives (no returns) as a company might have high return rate which would hurt profit margins. Also, accounting mistakes happen and based on the dataset, it appears that instead of actually using an accounting software to keep track of some expenditures, they simply show a negative withdrawal on their products. From the graph below, it appears that there is not any significant differences amongst the companies in terms of overall costs to their sales.


```r
negatives %>%
  ggplot(aes(time, positive, col = Name)) +
  geom_point() +
  facet_grid(. ~ Name, scales = "free_y") +
  theme_minimal() + 
  labs(x = "Month of the Year",
       y = "Total Return Amount (in dollars)",
       title = "Returns for Each Company") +
  theme(legend.position = "none")
```

![](case-study-8_files/figure-html/plot_data-1.png)<!-- -->
  
## Highest Sales

Another important aspect to look at is who is making the most "high purchaes" as this could be a sign that the company offers a valuable product that is high demand. From the graph below, it appears that LaBelle and SplashandDash had the highest "higher" purchases. It appears though that the other companies have more dense sales towards the lower end.

```r
 positive %>%
  filter(Amount < 150) %>%
  filter(day != "Sun" & day != "Sat") %>%
  ggplot() + 
  geom_jitter(aes(day, Amount, col = Name)) +
  labs(x = "Day of the Week",
       y = "Total",
       title = "Profits by Day") + 
  facet_wrap(~ Name) + theme_minimal() +
  theme(legend.position = "none")
```

![](case-study-8_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Weekly Performance 

Another important factor that we need to consider in evaluation of companies is overal trend. While their sales might be strong, there might be evidence of lowering profits which isn't good for the overall company. From the graph below, it appears that Frozone, LaBelle, and Tacento show the strongest signs of growth and theirfore would be a safer inverstment. The others appear to have negative trends which doesn't look good.


```r
 week %>%
  ggplot() + 
  geom_line(aes(week, total, col = Name)) +
   geom_point(aes(week, total, col = Name)) +
  labs(x = "Week of the Year",
       y = "Total",
       title = "Profits by Week (with average line)") +
  facet_wrap(~Name) + 
   geom_smooth(aes(week, total, col = Name), method = "lm", se = F) +
   scale_x_discrete(limits = seq(20,28,2)) +
   theme_minimal()
```

![](case-study-8_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Monthly Performance 

From the graph below, it shows that almost all companies have or are currently experiencing either a decline in sales or a slowing of sales. The one company that still appears to keep a positive path is LaBelle as there growth is still positive.

```r
 month %>%
  ggplot() + 
  geom_line(aes(month, total, col = Name)) +
  labs(x = "Month of the Year",
       y = "Total",
       title = "Profits by Month") +
   scale_x_discrete(limits = seq(5,7,1)) + 
   theme_minimal() +
   facet_wrap(~Name)
```

![](case-study-8_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
 
## Daily Sales
An interesting note is that all of the companies appear to have better sales on Fridays over any other day. The only exception is SplashandDash which appears to have Thursday as their most impressive day. 

```r
positive %>%
  filter(day != "Sun" & day != "Sat") %>%
  group_by(day, Name) %>%
  summarize(total = sum(Amount)) %>%
  ggplot() +
  geom_bar(aes(day, total, fill = day), stat = "identity") + theme_minimal() +
  labs(x = "Day of the Week",
       y = "Total Amount Earned (in dollars)",
       title = "Total Profits for Each Company") + facet_wrap(~Name)
```

![](case-study-8_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Overall Money Earned

It appears that the best company in terms of overall profit for the semester is HotDiggity followed by LaBelle. So, the best companies to invest in are HotDiggity and LaBelle. 


```r
positive %>%
  group_by(Name) %>%
  summarize(total = sum(Amount)) %>%
  ggplot() + 
  geom_bar(aes(Name, total, fill = Name), stat = "identity") +
  theme_minimal() + 
  labs(x = "Name of Company",
       y = "Total Earned (in dollars)",
       title = "Total Profits for Entire") +
   theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
```

![](case-study-8_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
