## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(scales)
library(signs)
library(dplyr)
library(ggplot2)
library(ggrepel)

## ----basics-------------------------------------------------------------------
theme_set(theme_gray())
theme_update(
  panel.grid.minor = element_blank(),
  axis.text.y      = element_blank(),
  axis.ticks.y     = element_blank()
)

p <- 
  ggplot(sleep) +
  aes(group, extra) +
  geom_point() +
  xlab("Drug") +
  ylab("Extra Sleep (hours)")

label_hours <- function(mapping) {
  geom_text_repel(
    mapping,
    nudge_x       = -.1,
    direction     = "y",
    segment.size  = .4,
    segment.color = "grey75",
    hjust         = "right"
  )
}

p +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra, accuracy = .1),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

## ----percentages--------------------------------------------------------------
p +
  ylab("Extra Sleep (% over 8 hours)") +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra / 8, accuracy = .1, format = scales::percent),
        group == 2 ~ percent(extra / 8, accuracy = .1)
      )
    )
  )

## ----commas-------------------------------------------------------------------
p +
  ylab("Extra Sleep (hours / year)") +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra * 365, format = scales::comma),
        group == 2 ~ comma(extra * 365)
      )
    )
  )

## ----matching-by-position-----------------------------------------------------
x <- seq(-4, 4)

number(x, 1) # first argument is accuracy
signs(x, 1)  # first argument is accuracy

## ----add-plusses--------------------------------------------------------------
p +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra, accuracy = .1, add_plusses = TRUE),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

## ----trim-leading-zeros-------------------------------------------------------
p +
  ylim(-.8, .8) +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra, accuracy = .1, trim_leading_zeros = TRUE),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


## ----axis-labels--------------------------------------------------------------
theme_update(
  axis.text.y = element_text(hjust = 1)
)

p +
  scale_y_continuous(
    limits = c(-.8, .8),
    breaks = seq(-.8, .8, by = .2),
    labels = signs_format(
      accuracy           = .1,
      add_plusses        = TRUE,
      trim_leading_zeros = TRUE
    )
  ) +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(
          extra,
          accuracy           = .1,
          add_plusses        = TRUE,
          trim_leading_zeros = TRUE
        ),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

## ----plus-or-minus------------------------------------------------------------
p +
  scale_y_continuous(
    limits = c(-4, 6),
    breaks = seq(-4, 6, by = 1),
    labels = signs_format(
      add_plusses = TRUE,
      label_at_zero = "symbol"
    )
  ) +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(
          extra,
          accuracy = .1,
          add_plusses = TRUE
        ),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

## ----zero-blank---------------------------------------------------------------
p +
  scale_y_continuous(
    limits = c(-4, 6),
    breaks = seq(-4, 6, by = 1),
    labels = signs_format(
      add_plusses = TRUE,
      label_at_zero = "blank"
    )
  ) +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(
          extra,
          accuracy = .1,
          add_plusses = TRUE
        ),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

## ----setting-options-globally-------------------------------------------------
options(
  signs.format             = scales::number,
  signs.add.plusses        = TRUE,
  signs.trim.leading.zeros = TRUE,
  signs.label.at.zero      = "none"
)

p +
  scale_y_continuous(
    limits = c(-.8, .8),
    breaks = seq(-.8, .8, by = .2),
    labels = signs_format(accuracy = .1, label_at_zero = "blank")
  ) +
  label_hours(
    aes(
      label = case_when(
        group == 1 ~ signs(extra, accuracy = .1),
        group == 2 ~ number(extra, accuracy = .1)
      )
    )
  )

