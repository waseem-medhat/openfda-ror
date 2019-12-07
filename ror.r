library(httr)
library(magrittr)
library(ggplot2)

# both drug and event -> a ----

q_both <- paste0(
  "https://api.fda.gov/drug/event.json",
  "?search=",
  "patient.drug.openfda.substance_name:SOFOSBUVIR",
  "+AND+",
  "patient.reaction.reactionmeddrapt:HEPATITIS+B+REACTIVATION",
  "+AND+",
  "receiptdate:[1989-06-30+TO+2019-12-05]",
  "+AND+",
  "occurcountry:eg",
  "&count=receiptdate"
)

r_both <- GET(q_both)

lapply(content(r_both)$results, function(x) x$count) %>% 
  as.numeric() %>% 
  sum() -> a

# drug only -> (a + b) ----

q_sofo <- paste0(
  "https://api.fda.gov/drug/event.json",
  "?search=",
  "patient.drug.openfda.substance_name:SOFOSBUVIR",
  "+AND+",
  "receiptdate:[1989-06-30+TO+2019-12-05]",
  "+AND+",
  "occurcountry:eg",
  "&count=receiptdate"
)

r_sofo <- GET(q_sofo)

lapply(content(r_sofo)$results, function(x) x$count) %>% 
  as.numeric() %>% 
  sum() -> a_plus_b

b <- a_plus_b - a

# event only -> (a + c) ----

q_ae <- paste0(
  "https://api.fda.gov/drug/event.json",
  "?search=",
  "patient.reaction.reactionmeddrapt:HEPATITIS+B+REACTIVATION",
  "+AND+",
  "receiptdate:[1989-06-30+TO+2019-12-05]",
  "+AND+",
  "occurcountry:eg",
  "&count=receiptdate"
)

r_ae <- GET(q_ae)

lapply(content(r_ae)$results, function(x) x$count) %>% 
  as.numeric() %>% 
  sum() -> a_plus_c

c <- a_plus_c - a

# all data -> (a + b + c + d) ----

q_all <- paste0(
  "https://api.fda.gov/drug/event.json",
  "?search=",
  "receiptdate:[1989-06-30+TO+2019-12-05]",
  "+AND+",
  "occurcountry:eg",
  "&count=receiptdate"
)

r_all <- GET(q_all)

lapply(content(r_all)$results, function(x) x$count) %>% 
  as.numeric() %>% 
  sum() -> gt

d <- gt - sum(a, b, c)

# analysis and visualization ----

fisher_obj <- fisher.test(matrix(c(a, b, c, d), nrow = 2))
fisher_obj$estimate # ROR
fisher_obj$conf.int

# error bar
ggplot(NULL) +
  geom_errorbar(
    aes(
      x = "",
      ymin = fisher_obj$conf.int[1],
      ymax = fisher_obj$conf.int[2]
    ),
    width = 0.5
  ) +
  geom_point(
    aes(
      x = "",
      y = fisher_obj$estimate
    ),
    size = 4,
    shape = 22,
    fill = "white"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylim(-10, 170) +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    title = "Association of Sofosbuvir with Hepatitis B Reactivation",
    subtitle = "Reporting Odds Ratio and its Confidence Interval"
  )
