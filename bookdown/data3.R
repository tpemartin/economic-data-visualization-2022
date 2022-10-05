# Public link does not require auth
googlesheets4::gs4_deauth()
googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1lkts4hLkrAFAobONFXiEjgDnuUmXKci6YF--vg1pC1s/edit?usp=sharing",
  sheet = "data 3"
) -> data3
head(data3)
library(ggplot2)
ggplot(data = data3) +
  geom_point(
    mapping = aes(y = Characteristic, x = `Total Effect`, fill = Country, size = Population),
    shape = 21,
    alpha = 0.5
  ) +
  geom_point(
    mapping = aes(y = Characteristic, x = `Total Effect`, color = Country)
  ) -> gg0
gg0
ggplot(data = data.frame(x = 0, xend = c(11, 22, 17, 5), y = c(1, 2, 3, 4), yend = c(1, 2, 3, 4))) +
  geom_segment(
    aes(
      x = x, xend = xend, y = y, yend = yend
    )
  )
library(dplyr)
data3 |>
  group_by(Characteristic) |>
  summarise(
    max = max(`Total Effect`)
  ) |>
  ungroup() -> data3max
print(data3max)s
ggplot() +
  geom_segment(
    data = data3max,
    mapping = aes(x = 0, xend = max, y = Characteristic, yend = Characteristic)
  )

library(ggplot2)
gg0 +
  geom_segment(
    data = data3max,
    mapping = aes(x = 0, xend = max, y = Characteristic, yend = Characteristic)
  )
ggplot() +
  geom_col(
    data = data3max,
    aes(
      x = Characteristic,
      y = max
    ),
    width = 0.01
  )
