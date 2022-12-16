# load tidyverse for reading, wrangling and plotting data
library(tidyverse) 
# load afex for ANOVA functions
library(afex) 
# load emmeans for pairwise comparisons
library(emmeans) 
# load psych for data summarize 
library(psych) 
# load skimr package for data summarize
library(skimr)
# load ggpubr package for plotting
library(ggpubr)
# load car package for leveneTest
library(car)

#load data
Q1_data = read_csv('./Raw_Data/assignment_2_dataset_1.csv')

# check if the data is correctly loaded
head(Q1_data)

# rename the value from condition_a, condition_b to normally and degraded.
# here we use mutate() function to recreate the column.
# case_when() is to select the case that fits different "condition".
Q1_data = Q1_data %>%
    mutate(
        condition = case_when(
            condition == "condition_a" ~ "normally",
            condition == "condition_b" ~ "degraded"
        )
    )

# code condition as a factor,using mutate function and factor function to replace the old column.
Q1_data = Q1_data %>%
    mutate(condition = factor(condition))

# double check if the data is coded correctly.
head(Q1_data)

# summarize data using base R functions
Q1_data %>%
    # we want to view the data in different conditions.
    group_by(condition) %>%
    # here we calculate mean and sd for the data, using summarize()
    summarise(mean = mean(response_time),
              sd = sd(response_time))

# according to this package's documentation, we use `describeBy()` function to compare data in different groups.
# The first argument sent is the data frame, and the second one is the group list.
describeBy(Q1_data$response_time, Q1_data$condition)

Q1_data %>%
    group_by(condition) %>%
    # here we use skim() function to generate summarize.
    skim() %>%
    # as we are only interested in the response_time variable, we use filter() to select it and compare between groups.
    filter(skim_variable == "response_time")

# Seed is set here to ensure reproducibility.
set.seed(11082245)
# use ggplot to demonstrate the distribution of data.
Q1_data %>%
    #here we set x axis as condition, y axis as response time, and we coloured the data according to the different conditions.
    ggplot(aes(x = condition, y = response_time, colour = condition)) +
    # here we add a layer of violin plot using settings and parameters in ggplot()
    geom_violin() +
    # let the dots jitter a little bit to have a better look.
    geom_jitter(width = .1) +
    # the figure is clear enough and we do not need a guides.
    guides(colour = FALSE) +
    # here we use stat_summary() function to add another layer.
    # "mean_cl_boot" is a built in function to acquire confidence limits for the mean.
    stat_summary(fun.data = "mean_cl_boot", colour = "black") +
    # also, a theme and text size is set here for a better view.
    theme_minimal() +
    theme(text = element_text(size = 13)) 

ggplot(data = Q1_data, aes(sample = response_time, colour = factor(condition))) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap( ~ condition) +
    guides(colour = FALSE) +
    labs(y = "Response Time", x = "Normal Theoretical Quantiles") +
    theme_minimal() +
    theme(text = element_text(size = 13))

tapply(Q1_data$response_time, Q1_data$condition, shapiro.test)

leveneTest(response_time ~ condition, data = Q1_data)

Q1_model_1 = aov_4(response_time ~ condition + (1 | participant),data = Q1_data)

summary(Q1_model_1)
# by using emmeans(), we could compare the data group by the condition factor.
emmeans(Q1_model_1, pairwise ~ condition)

Q1_data %>%
    ggplot(aes(x = condition, y = response_time, colour = condition)) +
    geom_violin() +
    geom_jitter(width = .1) +
    # here we add ANOVA result to the plot
    # note this anova function may not get the same result.
    stat_compare_means(method = "anova") +
    # here we draw the line to compare different groups
    stat_compare_means(comparisons = list(c("degraded", "normally")),
                       symnum.args = list(
                           # use * to illustrate the p value
                           cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
                           symbols = c("****", "***", "**", "*", "ns")
                       )) +
    guides(colour = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 13)) +
    labs(y = "Response Time", x = "Conditions")

t.test(Q1_data$response_time ~ Q1_data$condition)

Q2_data = read_csv('./Raw_Data/assignment_2_dataset_2.csv')
head(Q2_data)

Q2_data = Q2_data %>%
    mutate(
        condition = case_when(
            condition == "condition_a" ~ "normally",
            condition == "condition_b" ~ "degraded"
        )
    )
head(Q2_data)

Q2_data = Q2_data %>%
    mutate(condition = factor(condition))
head(Q2_data)

Q2_data %>%
    group_by(condition) %>%
    skim() %>%
    filter(skim_variable != "participant")

Q2_data %>%
    group_by(caffeine) %>%
    skim() %>%
    filter(skim_variable == "response_time")

# we set colour = condition in order to distinguish the relation between caffeine consumption and conditions. 
ggplot(Q2_data, aes(x = caffeine, y = response_time, colour = condition)) +
    geom_point(size = 3, alpha = .9) +
    labs(x = "Caffeine Consumption (cups of coffee)",
         y = "Response Time") +
    theme_minimal() +
    theme(text = element_text(size = 11)) 

Q2_model = aov_4(response_time ~ caffeine + condition +(1 | participant),data = Q2_data,factorize = FALSE)

anova(Q2_model)
# here we use 'pairwise' to compare different conditions
emmeans(Q2_model, pairwise ~ condition)

Q2_data <- Q2_data %>%
    mutate(condition = fct_relevel(condition,
                                   c("normally", "degraded")))
# here we use contrasts() to check the contrasts
contrasts(Q2_data$condition)

Q2_model_lm = lm(response_time ~ condition + caffeine, data = Q2_data)
Q2_model_lm

Resopnse_Time_normally = 998.564 + 12.791 * 0 + 2.469 * mean(Q2_data$caffeine)
Resopnse_Time_normally

# for ANOVA result: degraded = 1018
Resopnse_Time_degraded = 998.564 + 12.791 * 1 + 2.469 * mean(Q2_data$caffeine)
Resopnse_Time_degraded

Q2_model_lm_1 = lm(response_time ~ condition - 1 + caffeine, data = Q2_data)
Q2_model_lm_1

Q2_model_lm_2 = lm(response_time ~ 0 + condition + caffeine, data = Q2_data)
Q2_model_lm_2

# for ANOVA result: normally = 1005
Response_Time_0_1 = 998.564 * 1 + 1011.354 * 0 + 2.469 * mean(Q2_data$caffeine)
Response_Time_0_1

# for ANOVA result: degraded = 1018
Response_Time_0_2 = 998.564 * 0 + 1011.354 * 1 + 2.469 * mean(Q2_data$caffeine)
Response_Time_0_2

Q3_data_raw = read_csv('./Raw_Data/assignment_2_dataset_3.csv')
head(Q3_data_raw)

Q3_data_converted <- Q3_data_raw %>%
    # pivot function can convert wider data frame to longer data frame
    pivot_longer(
        # here we set columns that need to be converted
        cols = c(
            positiveprime_positivetarget,
            positiveprime_negativetarget,
            negativeprime_positivetarget,
            negativeprime_negativetarget
        ),
        # we send two column name here to indicate the function to convert them into two columns
        names_to = c("prime", "target"),
        # here we tell the function to separate conditions at the '_' symbol
        names_sep = "_" ,
        # here we tell the program where to save the value
        values_to = "rt",
        # set the transformed variable to be factor
        names_transform = "factor"
    )

# manually check the data
head(Q3_data_converted)

Q3_data_converted %>%
    # in this case, we use two grouping variables
    group_by(prime, target) %>%
    skim() %>%
    filter(skim_variable == "rt")

Q3_data_converted %>%
    # here we set x axis to be prime and target pair, which helps us compare the data
    ggplot(aes(
        x = prime:target,
        y = rt,
        colour = prime:target
    )) +
    geom_violin() +
    geom_jitter(width = .1, alpha = .25) +
    guides(colour = FALSE) +
    stat_summary(fun.data = "mean_cl_boot", colour = "black") +
    theme_minimal() +
    # because the tag is too long, we need to rotate it a little bit.
    theme(text = element_text(size = 13),
          axis.text.x = element_text(
              angle = 35,
              vjust = 0.5,
              hjust = 0.5
          )) +
    labs(x = "Prime X Target", y = "RT")

Q3_model = aov_4(rt ~ prime * target + (1 + prime * target | participant), data = Q3_data_converted)
Q3_model

# we use this function to get a more specific result
anova(Q3_model)

# here we use
emmeans(Q3_model, pairwise ~ prime * target, adjust = "Bonferroni")

Q3_data_converted %>%
    # here we set x axis to be prime and target pair, which helps us compare data
    ggplot(aes(
        x = prime:target,
        y = rt,
        colour = prime:target
    )) +
    geom_violin() +
    geom_jitter(width = .1, alpha = .25) +
    guides(colour = FALSE) +
    stat_summary(fun.data = "mean_cl_boot", colour = "black") +
    theme_minimal() +
    # because the tag is too long, we need to rotate it a little bit.
    theme(text = element_text(size = 13),
          axis.text.x = element_text(
              angle = 35,
              vjust = 0.5,
              hjust = 0.5
          )) +
    labs(x = "Prime X Target", y = "RT") +
    # here we draw the line to compare groups
    stat_compare_means(comparisons = list(
        c(
            "negativeprime:negativetarget",
            "positiveprime:negativetarget"
        ),
        c(
            "positiveprime:positivetarget",
            "negativeprime:positivetarget"
        )
    ),
    symnum.args = list(
        # use * to illustrate the p value
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
        symbols = c("****", "***", "**", "*", "ns")
    ))

