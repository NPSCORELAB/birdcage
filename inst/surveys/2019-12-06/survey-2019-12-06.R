library(tidyverse)

data_path <- "inst/surveys/2019-12-06/birdcage-questionnaire (Responses) - Form Responses 1.csv"

df <- data_path %>% 
  read_csv() %>% 
  mutate(Timestamp = as.Date(Timestamp, format = "%m/%d/%Y")) %>% 
  mutate(survey_id = row_number())

num_cols <- names(df)[map_lgl(df, ~ is.numeric(.x))] %>% 
  discard(~ .x == "survey_id")

levs <- c("Very Unsatisfied",
          "Less than Satisfied",
          "Satisfied",
          "More than Satisfied",
          "Very Satisfied")

init <- df %>% 
  pivot_longer(cols = num_cols) %>% 
  select(survey_id, timestamp = Timestamp, 
         question = name, response = value)

clean_df <- init %>% 
  mutate(question = case_when(
    question == "How likely are you to use this tool?" ~ "Likelihood of Using",
    question == "Documentation" ~ "Documentation Quality",
    question == "Completeness of Documentation" ~ "Documentation Coverage",
    question == "Accessibility to Product Support" ~ "Access to Support",
    TRUE ~ question
  )) %>% 
  mutate(response = case_when(
    response == 1 ~ levs[[1]],
    response == 2 ~ levs[[2]],
    response == 3 ~ levs[[3]],
    response == 4 ~ levs[[4]],
    response == 5 ~ levs[[5]]
  ) %>% 
    factor(levels = rev(levs))
  ) %>% 
  group_by(question, response) %>% 
  count() %>% 
  group_by(question) %>% 
  mutate(percent = n / nrow(df)) %>% 
  ungroup() %>% 
  arrange(desc(percent))

core_logo <- grid::rasterGrob(
  png::readPNG("inst/logos/corelogo.png")
)

clean_df %>% 
  ggplot() +
  geom_bar(aes(x = question, y = percent, fill = response),
           stat = "identity") +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::percent, expand = c(0.02, 0, 0 ,0)) +
  scale_fill_brewer(palette = "Spectral", direction = -1, drop = FALSE) +
  guides(fill = guide_legend(title = NULL, reverse = TRUE)) +
  labs(x = NULL, y = "% of Participants",
       title = "Questionnaire",
       caption = sprintf("Date Submitted: %s", df$Timestamp[[1L]])) +
  theme_minimal(base_family = "serif") +
  theme(legend.position = "top", legend.text = element_text(size = 6),
        plot.margin = unit(c(3, 1, 1, 2), "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  annotation_custom(core_logo,
                    xmin = 15.5, xmax = 17,
                    ymin = 0.75)

ggsave("man/figures/survey.png",
       dpi = 1000)

