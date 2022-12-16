# load packages required
library(tidyverse) # load tidyverse for reading, wrangling and plotting data
library(afex) # load for ANOVA functions
library(emmeans) # load for pairwise comparisons
library(psych) # load this package for data summarize and 
library(skimr)# load this package for data summarize
#load data
Q1_data = read_csv('./Raw_Data/assignment_2_dataset_1.csv')

# check if the data is correctly loaded
head(Q1_data)

# code condition as a factor
Q1_data = Q1_data %>% 
    mutate(condition = factor(condition))
head(Q1_data)

#rewrite consition variable

# summarize the data
Q1_data %>% 
    group_by(condition) %>% 
    summarise(mean = mean(response_time), sd = sd(response_time))

# 
describeBy(Q1_data$response_time, Q1_data$condition)


Q1_data %>% 
    # 
    group_by(condition) %>% 
    # 
    skim() %>% 
    # 
    filter(skim_variable == "response_time")

describeBy(Q2_data$response_time, Q2_data$condition,Q2_data$caffeine)

#data visualization
set.seed(11082245)
Q1_data %>% 
    ggplot(aes(x = condition, y = response_time, colour = condition)) +
    geom_violin() +
    geom_jitter(width = .1) +
    guides(colour = FALSE) +
    stat_summary(fun.data = "mean_cl_boot", colour = "black") +
    theme_minimal() +
    theme(text = element_text(size = 13)) 

#正态和方差检验

# building ANOVA model
Q1_model_1 = aov_4(response_time ~ condition + (1 | participant),data = Q1_data)

summary(Q1_model_1)

emmeans(Q1_model_1, pairwise~condition)
# emmeans plot

t.test(Q1_data$response_time ~ Q1_data$condition)


# Question 2
# 
Q2_data = read_csv('./Raw_Data/assignment_2_dataset_2.csv')

#
head(Q2_data)

#
Q2_data = Q2_data %>% 
    mutate(condition = factor(condition))
head(Q2_data)

Q2_data %>% 
    # 
    group_by(condition) %>% 
    # 
    skim() %>% 
    # 
    filter(skim_variable != "participant")

describeBy(Q2_data$response_time, c(Q2_data$condition,Q2_data$caffeine))

tapply(Q1_data$response_time,Q1_data$condition,skim)

ggplot(Q2_data, aes(x = caffeine, y = response_time,  colour = condition)) + 
    geom_point(size = 3, alpha = .9) +
    labs(x = "Caffeine Consumption (cups of cafe)", 
         y = "Response Time") +
    theme_minimal() +
    theme(text = element_text(size = 11)) 

#Other methord to check data

# 
Q2_model = aov_4(response_time ~ caffeine + condition +(1 | participant),data = Q2_data,factorize = FALSE)

anova(Q2_model)

emmeans(Q2_model, pairwise ~ condition)

Q2_data <- Q2_data %>%
    mutate(Condition = fct_relevel(condition,
                                   c("condition_a","condition_b")))

contrasts(Q2_data$condition)
# 
Q2_model_lm = lm(response_time ~ caffeine + condition-1, data = Q2_data)

Q2_model_lm = lm(response_time ~ 0 + caffeine + condition, data = Q2_data)

Q2_model_lm = lm(response_time ~ caffeine + condition, data = Q2_data)

Q2_model_lm

summary(Q2_model)

responsetime = 2.469*(mean(Q2_data$caffeine)) +998.564*(0) + 1011.354*(1)


# 
Q3_data_raw = read_csv('./Raw_Data/assignment_2_dataset_3.csv')

head(Q3_data)

# convert data
Q3_data_converted <- gather(
    data = Q3_data_raw,
    key = "conditions",
    value = "rt",
    2:5
)

# another way to convert
Q3_data_converted <- Q3_data_raw %>%
    pivot_longer(
        cols = c(positiveprime_positivetarget, positiveprime_negativetarget, negativeprime_positivetarget, negativeprime_negativetarget),
        names_to = "conditions",
        values_to = "rt"
    )

# summarize the data

# visualization

# building model
Q3_model = aov_4(rt ~ conditions +(1+conditions|participant),data = Q3_data_converted)

Q3_model

# 
anova(Q3_model)

emmeans(Q3_model,pairwise ~ conditions,adjust = "Bonferroni")

#Bonferroni reason





Q1_data %>%
    ggplot(aes(x = condition, y = response_time, colour = condition)) +
    geom_violin() +
    geom_jitter(width = .1) +
    stat_compare_means(method = "anova") + 
    stat_compare_means(comparisons = list(c("degraded", "normally")),symnum.args = list(
        # use * to illustrate the p value
        cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
        symbols = c("****", "***", "**", "*", "ns")
    )) +
    guides(colour = FALSE)+
    theme_minimal() +
    theme(text = element_text(size = 13))
    