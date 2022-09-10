library(tidyverse)
library(MetBrewer)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("import_LandusePoll22_2022-09-05_11-37.r")

ds <- as_tibble(ds) %>%
    select(-"SERIAL", -"REF", -"QUESTNNR", -"MODE", -"STARTED",
           -"TIME001", -"TIME002", -"TIME003", -"TIME004", -"TIME005", -"TIME006", -"TIME_SUM",
           -"MAILSENT", -"LASTDATA", -"FINISHED", -"Q_VIEWER", -"LASTPAGE", -"MAXPAGE",
           -"MISSING", -"MISSREL", -"TIME_RSI", -"DEG_TIME") %>%
    filter(!is.na(A001))

# Socio-demographics ----------------------------------------------------------------------------------------------

ds.a <- ds %>%
    select(starts_with("A")) %>%
    rename("Position"     = A001,
           "Weekly hours" = A002,
           "Sex"          = A003,
           "Kids"         = A004,
           "Care"         = A005,
           "University"   = A006,
           "Doctorate"    = A007,
           "Comments"     = A008_01,
           "Vacation"     = A009)

levels(ds.a$Position) <- c("Junior", "Senior", "NA")

# "What is your current position at PIK?"
ds.a %>%
    group_by(Position, Sex) %>%
    summarise(Sum = n())

a.p4 <- ggplot(ds.a) +
    geom_bar(aes(x = `Position`),
             stat = "count") +
    labs(x = "Position",
         y = "Frequency") +
    facet_grid(cols = vars(Sex)) +
    theme_classic(12) +
    theme(aspect.ratio = 1,
          strip.background = element_blank()); a.p4

cowplot::save_plot(filename = "plots/Position.png", plot = a.p4, ncol = 2, base_asp = 1)

# "On average, how many hours per week are you currently working?"
a.p1 <- ggplot(ds.a) +
    geom_bar(aes(x = `Weekly hours`),
             stat = "count") +
    labs(x = "Number of hours",
         y = "Frequency") +
    theme_classic(12) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1),
          aspect.ratio = 1); a.p1

cowplot::save_plot(filename = "plots/WeeklyHours.png", plot = a.p1, base_asp = 1)

# "What is your gender?"
ds.a %>%
    group_by(Sex) %>%
    summarise(sum = n())

# "Do you have kids?"
ds.a %>%
    group_by(Kids) %>%
    summarise(sum = n())

# "When doing home office, do you have to take care of others (kids, relatives, etc.)?"
ds.a %>%
    group_by(Care) %>%
    summarise(sum = n())

ds.a.degree <- ds.a %>%
    select(Sex, University, Doctorate) %>%
    mutate_each(funs(as.character)) %>%
    pivot_longer(cols = -Sex, names_to = "degree_type", values_to = "value") %>%
    filter(value != "No answer") %>%
    group_by(Sex, degree_type, value) %>%
    count() %>%
    group_by(Sex, degree_type) %>%
    mutate(total = sum(n)) %>%
    filter(value == "Yes")

ds.a.degree$degree_type <- factor(ds.a.degree$degree_type, levels = c("University", "Doctorate"))

a.p3 <- ggplot(ds.a.degree) +
    geom_col(aes(x = degree_type,
                 y = (n/total) * 100,
                 fill = Sex),
             position = "dodge") +
    labs(x = "Degree",
         y = "First in family to obtain a degree (%)",
         color = "") +
    expand_limits(y = c(0, 100)) +
    scale_fill_manual(values = met.brewer("Hokusai2", 2)) +
    theme_classic(12) +
    theme(aspect.ratio = 1,
          legend.title = element_blank(),
          strip.background = element_blank()); a.p3

cowplot::save_plot(filename = "plots/Degree.png", plot = a.p3)

# "Do you take all allotted vacation days in a year? If not how many generally remain?"
a.p2 <- ggplot(ds.a %>%
                   filter(!is.na(Vacation)) %>%
                   mutate(Vacation = fct_recode(Vacation, "10+" = "More than 10"))) +
    geom_histogram(aes(x = Vacation),
                   stat = "count") +
    facet_grid(cols = vars(Sex)) +
    labs(x = "Number of vacation days remaining",
         y = "Frequency",
         color = "") +
    theme_classic(12) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust = 1),
          strip.background = element_blank(),
          aspect.ratio = 1); a.p2

cowplot::save_plot(filename = "plots/VacationDays.png", plot = a.p2, ncol = 2, base_asp = 1)

# Anything to add?
ds.a %>%
    group_by(Comments) %>%
    summarise(n())


# Questions of COVID ----------------------------------------------------------------------------------------------

ds.bc <- ds %>%
    select(starts_with("B") | starts_with("C") | "A001" | "A003" | "A004" | "A005") %>%
    rename("Position"               = A001,
           "Sex"                    = A003,
           "Kids"                   = A004,
           "Care"                   = A005,
           "Covid frequency"       = B001,
           "Long Covid"             = B002,
           "Deeper integration"     = C001,
           "Cautious"               = C002,
           "Comfort.Shared office"  = C003_01,
           "Comfort.LessThanThree"  = C003_02,
           "Comfort.ThreeToTen"     = C003_03,
           "Comfort.MoreThan10"     = C003_04,
           "Comfort.Prolonged"      = C003_05,
           "ThisWinter"             = C004,
           "AfterCovid"             = C005,
           "CoreHours"              = C006,
           "Home Productivity"      = C007,
           "Support.Supervisors"    = C008_01,
           "Support.Colleagues"     = C008_02,
           "Support.PIK"            = C008_03,
           "Support.LU"             = C008_04,
           "DevelopmentsToKeep"     = C009_01,
           "Comments"               = C010_01)

levels(ds.bc$Position) <- c("Junior", "Senior", "NA")

###
# So far, I have had COVID-19 __ times
p <- ggplot(ds.bc) +
    geom_bar(aes(x = `Covid frequency`),
             stat = "count") +
    labs(x = "Covid count",
         y = "Frequency") +
    theme_classic(12) +
    theme(aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/CovidFrequency.png", plot = p, ncol = 1, base_asp = 1)

###
# I have or have had long covid
ds.bc %>%
    group_by(`Long Covid`) %>%
    summarise(Sum = n())

###
# Deeper integration
# "I would welcome a deeper integration of in-person meetings once again into our group’s workplace culture"
levels(ds.bc$`Deeper integration`) <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree", "NA")

## NOTE:
# Importantly, those who care for others were overwhelmingly in favor of deeper integration; though fewer
# strongly agreed all except one (indifferent) agreed. Perhaps they're running from responsibilities at home.

p <- ggplot(ds.bc) +
    geom_bar(aes(x = `Deeper integration`),
             stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    scale_fill_discrete(drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    theme_classic(10) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust = 1),
          axis.title.x = element_blank(),
          strip.background = element_blank(),
          aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/deeperIntegration.png", plot = p, base_asp = 1)

###
# Cautious
# "I tend to feel _____ than I perceive my peers to be, in relation to the COVID-19 pandemic and meeting in person"

ds.bc.frequency <- ds.bc %>%
    filter(!`Covid frequency` %in% c("I'd rather not say", "[NA] Not answered"),
           !`Cautious`        %in% c("Much more cautious", "[NA] Not answered")) %>%
    mutate(`Covid frequency` = as.numeric(`Covid frequency`)) %>%
    droplevels()

# Quick and dirty regression on the categorical variable of cautiousness showed that people
# who were less cautious tended to have Covid more times than those who were more cautious or as cautious.
# Poor P-values and relatively low R-squared.
m <- lm(data = ds.bc.frequency, formula = `Covid frequency` ~ Cautious); m
summary(m)

p <- ggplot(ds.bc) +
    geom_histogram(aes(x = Cautious),
                   stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_bw(20) +
    theme(aspect.ratio = 1); p

###
# Covid frequency
p <- ggplot(ds.bc) +
    geom_bar(aes(x = `Covid frequency`),
             stat = "count") +
    labs(y = "Frequency") +
    theme_classic(12) +
    theme(aspect.ratio = 1); p

cowplot::save_plot(filename = "Covid.png", plot = p, base_asp = 1)

###
# Meeting sizes - how comfortable do you feel with ...
comfort <- ds.bc %>%
    select(starts_with("Comfort")) %>%
    mutate_each(funs(as.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(type = as.factor(type))

levels(comfort$type) <- c("Meetings (< 3)", "Meetings (> 10)", "Prolonged group activities", "Sharing offices", "Meetings (3 - 10)")
# comfort$type <- factor(comfort$type, levels = c("Sharing offices", "Meetings (< 3)", "Meetings (3 - 10)", "Meetings (> 10)", "Prolonged group activities"))
comfort$type <- factor(comfort$type, levels = c("Prolonged group activities", "Meetings (> 10)",  "Meetings (3 - 10)", "Meetings (< 3)", "Sharing offices"))

p <- ggplot(comfort) +
    # geom_boxplot(aes(x = type,
                     # y = value)) +
    geom_violin(aes(x = type,
                     y = value)) +
    labs(x = "Situation",
         y = "Sentiment") +
    coord_flip() +
    theme_classic(12); p

cowplot::save_plot(filename = "plots/ComfortMeetings.png", plot = p, base_asp = 1.5)

###
# This winter

# "Even if the prevalence of COVID-19 pandemic increases over the fall and winter (while its severity remains
# roughly the same), if the government and PIK administration do not introduce new guidelines, I would continue
# coming to work"
p <- ggplot(ds.bc) +
    geom_bar(aes(x = ThisWinter),
             stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_classic(10) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust = 1),
          axis.title.x = element_blank(),
          aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/ThisWinter.png", p, base_asp = 1)

###
# After Covid

# "After the COVID-19 pandemic has subsided, I plan to work in the office"
p <- ggplot(ds.bc) +
    geom_bar(aes(x = AfterCovid),
             stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_classic(10) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust = 1),
          axis.title.x = element_blank(),
          aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/AfterCovid.png", p, base_asp = 1)

###
# Core hours

# "Do you think that it would be beneficial for the Land-use group to have either “core hours” or “core days” for in person work?"
p <- ggplot(ds.bc) +
    geom_bar(aes(x = CoreHours),
             stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_classic(14) +
    theme(aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/CoreHours.png", p, base_asp = 1)

###
# Home productivity

ds.bc$`Home Productivity` <- factor(ds.bc$`Home Productivity`, levels = c("Less", "About the Same", "More"))

p <- ggplot(ds.bc) +
    geom_bar(aes(x = `Home Productivity`),
             stat = "count") +
    labs(x = "Sentiment",
         y = "Frequency") +
    theme_classic(14) +
    theme(aspect.ratio = 1); p

cowplot::save_plot(filename = "plots/HomeProductivity.png", p, base_asp = 1)

###
# Support
support <- ds.bc %>%
    select(starts_with("Support")) %>%
    rename("Supervisors" = "Support.Supervisors",
           "Colleagues"  = "Support.Colleagues",
           "PIK"         = "Support.PIK",
           "Land-use"    = "Support.LU") %>%
    mutate_all(funs(as.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

p <- ggplot(support) +
    geom_bar(aes(x = value)) +
    facet_wrap(facets = vars(variable)) +
    expand_limits(x = c(1, 5)) +
    labs(x = "Satisfaction",
         y = "Frequency") +
    theme_classic(16); p

cowplot::save_plot(filename = "plots/Satisfaction.png", p, base_asp = 1, ncol = 2, nrow = 2)

###
# "Are there any developments/outcomes that have arisen during the COVID-19 pandemic
ds.bc %>%
    select(DevelopmentsToKeep) %>%
    filter(!is.na(.)) %>%
    View()

# "Are there any other topics that you would like to be discussed in terms of the COVID-19 pandemic and workplace culture during the LU retreat?"
ds.bc %>%
    select(Comments) %>%
    filter(!is.na(.)) %>%
    View()



# Questions of Discrimination ----------------------------------------------------------------------------------------------

# "In the past year, have you felt discriminated against or harassed, or witnessed discrimination or harassment,
# in the workplace? This could be, e.g., in the land-use group, at PIK, conferences or workshops,
# or during the publication process.This will of co:"

D001 <- ds %>%
    select(starts_with("D001_")) %>%
    mutate_each(funs(as.numeric)) %>%
    rename(
        "Gender" = D001_01,
        "Age" = D001_02,
        "Racial/ethnic background"= D001_03,
        "Sexual orientation"= D001_04,
        "Appearance"= D001_05,
        "Religious Background" = D001_06,
        "Class Background" = D001_07,
        "Marital status" = D001_08,
        "Family situation" = D001_09,
        "Language" = D001_10,
        "Other, please specify" = D001_11) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    group_by(type) %>%
    summarise(value = sum(value)) %>%
    arrange(desc(value))

D001.p <- ggplot(D001) +
    geom_col(aes(x = reorder(type, value),
                 y =  value), fill = "red",colour = "red", alpha = 0.4) +
    labs(x = "Type of Discrimination",
         y = "Count") +
    theme_minimal(20) +
    scale_y_continuous(breaks=seq(0,10,2)) +
    coord_flip() ; D001.p
#multiple discrimination ?

#(Optional) Remaining anonymous, please be more specific about the occurrence so that we can discuss as a group, especially noting where this discrimination occurred
D002 <- ds %>%
    select(starts_with("D002_")) %>%
    filter(!is.na(D002_01))


################# E HAPPINESS POLL
#2 collabs?

ds.e1 <- ds %>%
    select("A001", starts_with("E001")) %>%
    rename(
        "Position" = A001,
        "Overall.happiness" = E001_01,
        "Day.to.day.work" = E001_02,
        "Personal.work.progress"= E001_03,
        "Job.security.and.perspectives"= E001_04,
        "Commuting.time"= E001_05,
        "Creativity" = E001_06,
        "Relevance.Doing.good" = E001_07,
        "Team" = E001_08,
        "Working.time.flexibility" = E001_09,
        "Working.atmosphere" = E001_10,
        "Collaboration.qty" = E001_11,
        "Collaboration" = E001_12,
        "Work.life.balance" = E001_13,
        "Curiosity" = E001_14) %>%
    mutate(across(!Position, as.numeric)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "type", values_to = "value")

levels(ds.e1$Position) <- c("Junior", "Senior", "No answer")

ds.e1.p1 <- ggplot(filter(ds.e1, type %in% c("Overall.happiness")), aes(x=Position, y = value, fill = Position)) +
    geom_violin() +
    stat_summary(fun=mean, geom="point", size=2, shape = 18)+
    facet_wrap(~type) +
    #geom_boxplot(width=0.1) +
    #coord_flip() +
    theme_classic(base_size = 24) +
    theme(legend.position = "none") +
    ylab("Happiness") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    expand_limits(y = c(0,5))     ; ds.e1.p



ds.e1.p2 <- ggplot(filter(ds.e1, type %in% c("Commuting.time", "Work.life.balance",
                                             "Job.security.and.perspectives", "Working.time.flexibility")),
                   aes(x=Position, y = value, fill = Position)) +
    geom_violin() +
    stat_summary(fun=mean, geom="point", size=2, shape = 18)+
    facet_wrap(~type) +
    #geom_boxplot(width=0.1) +
    #coord_flip() +
    theme_classic(base_size = 24) +
    theme(legend.position = "none") +
    ylab("Happiness") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    expand_limits(y = c(0,5))     ; ds.e1.p2

ds.e1.p3 <- ggplot(filter(ds.e1, type %in% c("Collaboration",
                                             "Team", "Working.atmosphere")),
                   aes(x=Position, y = value, fill = Position)) +
    geom_violin() +
    stat_summary(fun=mean, geom="point", size=2, shape = 18)+
    facet_wrap(~type, nrow = 2) +
    #geom_boxplot(width=0.1) +
    #coord_flip() +
    theme_classic(base_size = 24) +
    theme(legend.position = "none") +
    ylab("Happiness") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    expand_limits(y = c(0,5))     ; ds.e1.p3


ds.e1.p4 <- ggplot(filter(ds.e1, type %in% c("Day.to.day.work",
                                             "Relevance.Doing.good", "Personal.work.progress", "Creativity", "Curiosity")),
                   aes(x=Position, y = value, fill = Position)) +
    geom_violin() +
    stat_summary(fun=mean, geom="point", size=2, shape = 18)+
    facet_wrap(~type, nrow = 2) +
    #geom_boxplot(width=0.1) +
    #coord_flip() +
    theme_classic(base_size = 24) +
    theme(legend.position = "none") +
    ylab("Happiness") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    expand_limits(y = c(0,5))     ; ds.e1.p4

ds.e <- ds %>%
    select(starts_with("E001")) %>%
    mutate_each(funs(as.numeric)) %>%
    rename(
        "Overall.happiness" = E001_01,
        "Day.to.day.work" = E001_02,
        "Personal.work.progress"= E001_03,
        "Job.security.and.perspectives"= E001_04,
        "Commuting.time"= E001_05,
        "Creativity" = E001_06,
        "Relevance.Doing.good" = E001_07,
        "Team" = E001_08,
        "Working.time.flexibility" = E001_09,
        "Working.atmosphere" = E001_10,
        "Collaboration.qty" = E001_11,
        "Collaboration" = E001_12,
        "Work.life.balance" = E001_13,
        "Curiosity" = E001_14) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    group_by(type) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(Year = 2022)

ds.e <- ds.e[,-2]


hist <- read.csv("TimeSeries2016_2021.csv")

hs <- rbind(hist, ds.e) %>%
    pivot_longer(cols = 2:last_col(), names_to = "type", values_to = "average")

write.csv(hs, file = "TimeSeries2016_2022.csv")


colorsTS <- c(
    "mediumorchid2",
    "blue4",
    "red4",
    "orchid4",
    "red1",
    "orange1",
    "palegreen2",
    "lightskyblue",
    "green4",
    "deeppink1",
    "black",
    "deepskyblue3",
    "green2"
)

ggplot(filter(hs, type == "Overall.happiness"),
       aes(x = Year, y = average, color = type)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = colorsTS[7]) +
    expand_limits(y = c(0,5)) +
    theme_minimal(base_size = 24) +
    ylab("Average Happiness")


ggplot(filter(hs, type %in% c("Overall.happiness", "Commuting.time", "Work.life.balance",
                              "Job.security.and.perspectives", "Working.time.flexibility")),
       aes(x = Year, y = average, color = type)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = colorsTS[c(2,6,7,12,13)]) +
    expand_limits(y = c(0,5)) +
    theme_minimal(base_size = 24) +
    geom_point(data = filter(hs, type == "Work.life.balance"))+
    ylab("Average Happiness")


ggplot(filter(hs, type %in% c("Overall.happiness", "Collaboration",
                              "Team", "Working.atmosphere")),
       aes(x = Year, y = average, color = type)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = colorsTS[c(10,7,3,11)]) +
    expand_limits(y = 0) +
    theme_minimal(base_size = 24) +
    ylab("Average Happiness")

ggplot(filter(hs, type %in% c("Overall.happiness", "Day.to.day.work",
                              "Relevance.Doing.good", "Personal.work.progress", "Creativity", "Curiosity")),
       aes(x = Year, y = average, color = type)) +
    geom_line(size = 1.5) +
    scale_color_manual(values = colorsTS[c(5,1,9,7, 4,8)]) +
    expand_limits(y = c(0,5)) +
    theme_minimal(base_size = 24) +
    ylab("Average Happiness")


########### E02 What do you value most/which factor contributes most to
#                your workplace happiness? (multiple answers)


ds.e2 <- ds %>%
    select(starts_with("E002_")) %>%
    rename("Flexible working hours" = E002_01,
           "Interactions with colleagues/collaboration" = E002_02,
           "Decision independence/freedom w.r.t. topic" = E002_03,
           "Topic research" = E002_04,
           "Creativity" = E002_05,
           "Team/colleagues" = E002_06) %>%
    mutate_each(funs(as.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    group_by(type) %>%
    summarise(value = sum(value))

ds.e2.p <- ggplot(ds.e2, aes(x = reorder(type, value), y = value, fill = type)) +
    geom_col() +
    coord_flip() +
    xlab("Contributor") + ylab("Count") +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none") +
    scale_fill_manual(values =  met.brewer("VanGogh2", 6)); ds.e2.p

#######
#### Meetings ########
######


#How many hours per week do you spend on meetings
ds.f1 <- ds %>%
    select("A001", starts_with("F001_")) %>%
    rename("Position" = A001,
           "Landuse" = F001_01,
           "MAgPIE/LUCHS" = F001_02,
           "Post-Doc" = F001_03,
           "Supervision weeklies" = F001_04,
           "RD seminars" = F001_05,
           "Other Seminars (LPJmL, Phd, etc)" = F001_06,
           "Project Meetings" = F001_07,
           "Technical meetings (hackathons, modelling, etc)" = F001_08,
           "Administrative meetings" = F001_09,
           "Public-facing (press etc.)" = F001_10,
           "Other meetings" = F001_11) %>%
    mutate(across(!Position, gsub, pattern = "[^0-9.-]", replacement = ""))
#maually clean up weird ones
ds.f1$Landuse <- as.numeric(ds.f1$Landuse)
ds.f1$`Supervision weeklies` <- as.numeric(ds.f1$`Supervision weeklies`)
ds.f1[14,"Supervision weeklies"] <- 5.5
ds.f1[14,2] <- 0.5
ds.f1 <- ds.f1 %>%
    mutate(across(!Position, as.numeric)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "type", values_to = "time") %>%
    replace_na(list(time = 0)) %>%
    group_by(type, Position) %>%
    summarise(time = mean(time))

levels(ds.f1$Position) <- c("Junior", "Senior", "No answer")


ds.f1.p <- ggplot(ds.f1, aes(x = reorder(type, -time), y = time, fill = Position)) +
    geom_bar(position="dodge", stat="identity")+
    # coord_flip() +
    theme_minimal(base_size = 24) +
    #theme(legend.position = "none") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    xlab(NULL) + ylab("Degree of Necessity") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1)) +
    xlab("Meeting") + ylab("Average Hours per Week"); ds.f1.p



##### Who benefits from each meeting type?

ds.f2 <- ds %>%
    select(starts_with("F002_"), -contains("CN")) %>%
    mutate_each(funs(as.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>%
    separate(col = type, into = c("drop", "meeting", "subject"), sep = "_") %>%
    mutate(meeting = case_when(
        meeting == "01" ~ "Landuse",
        meeting == "02" ~ "LUCHS/MAgPIE",
        meeting == "03" ~ "Post-Doc",
        meeting == "04" ~ "Supervision Weeklies",
        meeting == "05" ~ "RD Seminars",
        meeting == "06" ~ "Other Seminars",
        meeting == "07" ~ "Project Meetings",
        meeting == "08" ~ "Technical meetings",
        meeting == "09" ~ "Administrative meetings",
        meeting == "10" ~ "Public-facing (press etc.)",
        meeting == "11" ~ "Other meetings"),
        subject = case_when(
            subject == "1" ~ "Myself",
            subject == "2" ~ "The Group",
            subject == "3" ~ "The RD",
            subject == "4" ~ "PIK",
            subject == "5" ~ "Decision makers",
            subject == "6" ~ "General Public")) %>%
    select(-drop) %>%
    group_by(meeting, subject) %>%
    summarise(value = sum(value)) %>%
    mutate(meeting = as.factor(meeting), subject = as.factor(subject))

ds.f2$meeting <- factor(ds.f2$meeting,levels = (c("Landuse",
                                                  "LUCHS/MAgPIE",
                                                  "Post-Doc",
                                                  "Supervision Weeklies",
                                                  "RD Seminars",
                                                  "Other Seminars",
                                                  "Project Meetings",
                                                  "Technical meetings",
                                                  "Administrative meetings",
                                                  "Public-facing (press etc.)",
                                                  "Other meetings")))

ds.f2$subject <- factor(ds.f2$subject,levels = rev(c("Myself",
                                                     "The Group",
                                                     "The RD",
                                                     "PIK",
                                                     "Decision makers",
                                                     "General Public")))


ds.f2.p <- ggplot(ds.f2, aes(x= subject, y = value, fill = subject)) +
    geom_col(alpha = 0.8) +
    coord_flip()+
    facet_wrap(~meeting) +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none") +
    scale_fill_manual(values =  met.brewer("Troy", 6)) +
    xlab("Who benefits?") + ylab("Vote count"); ds.f2.p

### are such meetings always necessary?


ds.f3 <- ds %>%
    select(starts_with("F003_")) %>%
    rename("Landuse" = F003_01,
           "MAgPIE/LUCHS" = F003_02,
           "Post-Doc" = F003_03,
           "Supervision weeklies" = F003_04,
           "RD seminars" = F003_05,
           "Other Seminars (LPJmL, Phd, etc)" = F003_06,
           "Project Meetings" = F003_07,
           "Technical meetings (hackathons, modelling, etc)" = F003_08,
           "Administrative meetings" = F003_09,
           "Public-facing (press etc.)" = F003_10,
           "Other" = F003_11) %>%
    mutate_each(funs(as.numeric)) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "count") %>%
    replace_na(list(count = 0)) %>%
    group_by(type) %>%
    summarise(count = mean(count))

ds.f3.p <- ggplot(ds.f3, aes(x = reorder(type, count), y = count, fill = type)) +
    geom_col() +
    coord_flip() +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none") +
    scale_fill_manual(values =  met.brewer("VanGogh2", 11))+
    xlab("Meeting") + ylab("Degree of Necessity"); ds.f3.p

### Current Career needs? ######


ds.f4 <- ds %>%
    select("A001", starts_with("F004_")) %>%
    rename("Position" = A001,
           "Read papers" = F004_01,
           "Learn methods" = F004_02,
           "Develop research questions" = F004_03,
           "Listen to other's presentations" = F004_04,
           "Present own work at seminar/conferences" = F004_05,
           "Publish first-author papers" = F004_06,
           "Project Meetings" = F004_07,
           "Develop own research line" = F004_08,
           "Write proposals" = F004_09,
           "Supervise Bachelor/Masters" = F004_10,
           "Supervise PhD" = F004_11,
           "Supervise PostDoc" = F004_12,
           "Teach" = F004_13,
           "Establish external collaborations" = F004_14,
           "Institutional duties" = F004_15) %>%
    mutate(across(!Position, as.numeric)) %>%
    pivot_longer(cols = 2:last_col(), names_to = "type", values_to = "value") %>%
    replace_na(list(value = 0)) %>%
    group_by(Position, type) %>%
    summarise(value = mean(value))

levels(ds.f4$Position) <- c("Junior", "Senior", "No answer")


ds.f4.p <- ggplot(ds.f4, aes(x = reorder(type, -value), y = value, fill = Position)) +
    geom_bar(position="dodge", stat="identity")+
    # coord_flip() +
    theme_minimal(base_size = 24) +
    #theme(legend.position = "none") +
    scale_fill_manual(values =  met.brewer("Hokusai2", 2))+
    xlab(NULL) + ylab("Degree of Necessity") +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1)); ds.f4.p

ds.f5 <- ds %>%
    select(starts_with("F005_"))

unique(data.frame(ds.f5))
