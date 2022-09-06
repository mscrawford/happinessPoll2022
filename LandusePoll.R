library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("import_LandusePoll22_2022-09-05_11-37.r")

ds <- as_tibble(ds) %>%
    select(-"SERIAL", -"REF", -"QUESTNNR", -"MODE", -"STARTED",
           -"TIME001", -"TIME002", -"TIME003", -"TIME004", -"TIME005", -"TIME006", -"TIME_SUM",
           -"MAILSENT", -"LASTDATA", -"FINISHED", -"Q_VIEWER", -"LASTPAGE", -"MAXPAGE",
           -"MISSING", -"MISSREL", -"TIME_RSI", -"DEG_TIME") %>%
    filter(!is.na(A001))





# Socio-demographics ----------------------------------------------------------------------------------------------

ds.t <- ds %>%
    select(starts_with("A"))

# "What is your current position at PIK?"
A001 <- ds.t %>%
    group_by(A001) %>%
    summarise(n()); A001

# "On average, how many hours per week are you currently working?"
A002 <- ggplot(ds.t) +
    geom_histogram(aes(x = A002),
                   stat = "count") +
    labs(x = "Number of hours",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); A002

# "What is your gender?"
A003 <- ds.t %>%
    group_by(A003) %>%
    summarise(n()); A003

# "Do you have kids?"
A004 <- ds.t %>%
    group_by(A004) %>%
    summarise(n()); A004

# "When doing home office, do you have to take care of others (kids, relatives, etc.)?"
A005 <- ds.t %>%
    group_by(A005) %>%
    summarise(n()); A005

# "Are you the first one in your family degree (parents, grandparents, siblings) with a university degree?"
A006 <- ds.t %>%
    group_by(A006) %>%
    summarise(n()); A006

# Are you the first one in your family, who is obtaining/has obtained a doctoral degree?"
A007 <- ds.t %>%
    group_by(A007) %>%
    summarise(n()); A007

# "Do you take all allotted vacation days in a year? If not how many generally remain?"
A009 <- ggplot(ds.t) +
    geom_histogram(aes(x = A009),
                   stat = "count") +
    labs(x = "Number of days",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); A009

# Anything to add?
A008_01 <- ds.t %>%
    group_by(A008_01) %>%
    summarise(n()); A008_01





# Questions of COVID ----------------------------------------------------------------------------------------------

ds.t <- ds %>%
    select(starts_with("B") | starts_with("C"))

# So far, I have had COVID-19 __ times
B001 <- ggplot(ds.t) +
    geom_histogram(aes(x = B001),
                   stat = "count") +
    labs(x = "Number of times",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); B001

# I have or have had long covid
B002 <- ds.t %>%
    group_by(B002) %>%
    summarise(n()); B002

# "I would welcome a deeper integration of in-person meetings once again into our group’s workplace culture"
C001 <- ggplot(ds.t) +
    geom_histogram(aes(x = C001),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); C001

# "I tend to feel _____ than I perceive my peers to be, in relation to the COVID-19 pandemic and meeting in person"
C002 <- ggplot(ds.t) +
    geom_histogram(aes(x = C002),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); C002

# How comfortable do you feel with ...
C003 <- ds.t %>%
    select(starts_with("C003")) %>%
    mutate_each(funs(as.numeric)) %>%
    rename("Sharing offices" = C003_01,
           "Meetings (< 3)" = C003_02,
           "Meetings (3 - 10)" = C003_03,
           "Meetings (> 10)" = C003_04,
           "Prolonged group activities" = C003_05) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    filter(!is.na(value))

C003$type <- factor(C003$type, levels = c("Sharing offices", "Meetings (< 3)", "Meetings (3 - 10)", "Meetings (> 10)", "Prolonged group activities"))

C003.p <- ggplot(C003) +
    geom_boxplot(aes(x = type,
                    y = value)) +
    labs(x = "Situation",
         y = "Sentiment") +
    theme_bw(20) +
    theme(); C003.p

# "Even if the prevalence of COVID-19 pandemic increases over the fall and winter (while its severity remains
# roughly the same), if the government and PIK administration do not introduce new guidelines, I would continue
# coming to work"
C004 <- ggplot(ds.t) +
    geom_histogram(aes(x = C004),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C004

# "After the COVID-19 pandemic has subsided, I plan to work in the office"
C005 <- ggplot(ds.t) +
    geom_histogram(aes(x = C005),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C005

# "Do you think that it would be beneficial for the Land-use group to have either “core hours” or “core days” for in person work?"
C006 <- ggplot(ds.t) +
    geom_histogram(aes(x = C006),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C006

# "Compared to when you work in the office, while doing home office, do you accomplish"
C007 <- ggplot(ds.t) +
    geom_histogram(aes(x = C007),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C007

# C008 - Please assess the following statements
# "I have adequate access to my supervisors, in terms of regular interactions and when acute problems arise."
C008_01 <- ggplot(ds.t) +
    geom_histogram(aes(x = C008_01),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C008_01

# "I receive help from colleagues when I need assistance in solving a work-related issue."
C008_02 <- ggplot(ds.t) +
    geom_histogram(aes(x = C008_02),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C008_02

# "I am receiving sufficient support and flexibility from the PIK administration in dealing with on-going Covid issues as they arise."
C008_03 <- ggplot(ds.t) +
    geom_histogram(aes(x = C008_03),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C008_03

# "I am receiving sufficient support and flexibility from the Land-use group in dealing with on-going Covid issues as they arise."
C008_04 <- ggplot(ds.t) +
    geom_histogram(aes(x = C008_04),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(14) +
    theme(aspect.ratio = 1); C008_04

# "Are there any developments/outcomes that have arisen during the COVID-19 pandemic – since last year’s poll – that you would like to adopt long-term (e.g. remote conferences)?"
C009_01 <- ds.t %>%
    group_by(C009_01) %>%
    summarise(n()); C009_01

# "Are there any other topics that you would like to be discussed in terms of the COVID-19 pandemic and workplace culture during the LU retreat?"
C010_01 <- ds.t %>%
    group_by(C010_01) %>%
    summarise(n()); C010_01
