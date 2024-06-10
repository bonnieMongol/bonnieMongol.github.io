![](https://i.gifer.com/8Etj.gif)

---
title: "Assignment 3 v2"
output: html_document
date: "2024-06-10"
---

## R Markdown

```{r setup, include = FALSE }

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

```

```{r load Libraries, include = FALSE }

library(tidyverse)
library(tidytext)
library(wordcloud)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(naniar)
library(scales)
library(janitor)
library(DT)

```


### To run time series on Netflix, Inc. data


### Import Data

```{r Import Data}

NetFlix <- read_csv("/Users/bogi/Downloads/netflix_titles.csv") %>% 
  mutate(date_added = mdy(date_added)) %>% clean_names()

glimpse(NetFlix)

```

### Data Screening

#### Missing data

```{r Missing data 1}

gg_miss_which(NetFlix)

```

## Missing the variables

```{r Missing data 2}

gg_miss_upset(NetFlix)

```


```{r Head}

NetFlix %>% head()

```



### Comparison movies and TV shows on Netflix

```{r Movies VS TV shows}

NetFlix %>% count(type, sort = T) %>%
  
  mutate(prop = paste0(round(n / sum(n) * 100, 0), "%")) %>%
  ggplot(aes(x = "", y = prop, fill = type)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "steelblue",
    size = 1
  ) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(y = prop, label = prop),
    position = position_stack(vjust = 0.5),
    size = 6,
    col = "white",
    fontface = "bold"
  ) +
  scale_fill_manual (values = c('#e41a1c', '#377eb8')) +
  theme_void() +
  labs(
    title = "Are Movies on Netflix more than TV shows?",
    subtitle = "Pie Plot, proportion of Movies to TV shows",
    caption = "Kaggle: Netflix Movies and TV Shows",
    fill = ""
  )

```

The result shows Movies are more than TV shows. 

### Years Difference between release year and added year!


```{r years Difference 1}

NetFlix <-  NetFlix %>% 
  mutate(year_diff = year(date_added)-release_year) 

NetFlix %>% count(year_diff, sort = F)

```

Checking example years below

-   10 items added before release year, 1 year

-   1 item added before release year, 2 year

-   1 added before release year, 3 year


```{r years Difference 2}


datatable(
  NetFlix %>% select(-cast, -description) %>%
    filter(year_diff < 0) %>%
    arrange(year_diff),
  caption = NULL,
  options = list(dom = 't')
)

```
#### Years Difference distridution

```{r}

NetFlix %>% select(year_diff) %>%
  filter(!is.na(year_diff)) %>%
  plot_ly(x = ~ year_diff,
          type = "histogram",
          marker = list(line = list(color = "darkgray",
                                    width = 1))) %>%
  layout(
    title = "Year difference between release_year and date_added",
    yaxis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "difference (Years)",
                 zeroline = FALSE)
  )

```
Checking example of 90 years difference

```{r}

datatable(NetFlix %>% select(title, type, release_year, date_added, year_diff) %>%
  filter(year_diff > 60) %>% 
    arrange(desc(year_diff)),
  caption = NULL,
  options = list(dom = 't')
)

```



### Rating by Type

```{r Rating by Type}

NetFlix %>% select(rating, type) %>%
  filter(!is.na(rating)) %>%
  mutate(rating = fct_lump(rating, 5)) %>%
  group_by(rating, type) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ type ,
    y = ~ Count,
    type = "bar",
    color = ~ rating,
    text = ~ Count,
    textposition = 'outside',
    textfont = list(color = '#000000', size = 12)
  ) %>%
  layout(yaxis = list(categoryorder = "array",
                      categoryarray = ~ Count)) %>%
  layout(
    title = "Rating by Type",
    yaxis = list(title = "Type"),
    xaxis = list(title = "Count"),
    legend = list(title = list(text = '<b> Rating </b>'))
  )

```
### Distribution by Countries Top 10

```{r Distribution by Countries Top 10}

NetFlix %>% select(country) %>%
  filter(!is.na(country)) %>%
  mutate(country = fct_lump(country, 10)) %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ Count ,
    y = ~ country,
    type = "bar",
    orientation = 'h'
  ) %>%
  layout(yaxis = list(categoryorder = "array", categoryarray = ~ Count)) %>%
  layout(
    title = "Items distribution by Country",
    yaxis = list(title = "Country"),
    xaxis = list(title = "Count")
  )

```



```{r Distribution by Countries Top 50}

NetFlix %>% select(country) %>%
  filter(!is.na(country)) %>%
  mutate(country = fct_lump(country, 45)) %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ Count ,
    y = ~ country,
    type = "bar",
    orientation = 'h'
  ) %>%
  layout(yaxis = list(categoryorder = "array", categoryarray = ~ Count)) %>%
  layout(
    title = "Items distribution by Country",
    yaxis = list(title = "Country"),
    xaxis = list(title = "Count")
  )

```

```{r Lebanon, fig.height = 9, fig.width = 9}

    datatable(NetFlix %>% 
                select(-cast, -description) %>% 
      filter(!is.na(country),
             country == "Lebanon"),
      caption = NULL,
      options = list(dom = 't'))

```


### Dataset split to check the durations

```{r Dataset split}

movies <- NetFlix %>% select(country, type, duration, rating, title) %>%
  filter(type == "Movie") %>%
  drop_na() %>% 
  mutate(duration_min = parse_number(duration))

tv_show <- NetFlix %>% select(country, type, duration, rating, title) %>%
  filter(type == "TV Show") %>% 
  drop_na() %>% 
  mutate(duration_season = parse_number(duration))

```

### Movies Durations

```{r Movies Durations}

movies %>%
  plot_ly(
    x = ~ duration_min,
    type = "histogram",
    nbinsx = 40,
    marker = list(
      color = "drakblue",
      line = list(color = "black",
                  width = 1)
    )
  ) %>%
  layout(
    title = "Duration distrbution",
    yaxis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "Duration (min)",
                 zeroline = FALSE)
  ) 

```
```{r}

datatable(movies %>% select(title, duration_min) %>% 
            filter(duration_min >200) %>% arrange(desc(duration_min)),
      caption = NULL,
      options = list(dom = 't'))

```


### TV-Show Durations

```{r TV-Show Durations}

tv_show %>% select(duration_season) %>%
  count(duration_season, sort = TRUE) %>%
  ggplot(aes(
    x = as.factor(duration_season),
    y = n,
    label = n
  )) +
  geom_col(aes(fill = duration_season)) +
  geom_text(vjust = -0.5, size = 3, col = "darkgreen") +
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Season duration",
       y = "Count",
    title = "Season distrbution",
    subtitle = "Column Plot, Season distrbution",
    caption = "Kaggle: Netflix Movies and TV Shows",
    fill = ""
  )

```

```{r}

datatable(tv_show %>% select(title, duration_season) %>% 
            filter(duration_season >15) %>% arrange(desc(duration_season)),
      caption = NULL,
      options = list(dom = 't'))

```

### Time series


```{r Time series }

ggplotly(
  NetFlix %>% select(date_added) %>%
    filter(!is.na(date_added)) %>%
    mutate(year_added = year(date_added)) %>%
    group_by(year_added) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ggplot(aes(
      x = year_added,
      y = Count,
      label = Count
    )) +
    geom_line(size = 1, col = "darkred", alpha = 0.5) +
    geom_col(alpha = 0.6, fill = "steelblue") +
    geom_text(vjust = -0.7, size = 3) +
    theme_light() +
    scale_y_continuous(label = comma) +
    labs(
      x = "Year Added",
      y = "Count",
      title = "Number of Items added per year",
      subtitle = "Column and line Plot, Nunber of Items added per year",
      caption = "Kaggle: Netflix Movies and TV Shows"
    )
)

```


```{r}


desc_words_m <- NetFlix %>% select(type, show_id, description) %>%
  filter(type == "Movie") %>% 
    unnest_tokens(word, description) %>%
    anti_join(stop_words)

count_word <- desc_words_m %>%
   count(word, sort = TRUE)


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 50,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

```

### Most frequent words in description variable For TV-Shows (word cloud)

```{r}


desc_words_tv <- NetFlix %>% select(type, show_id, description) %>%
  filter(type == "TV Show") %>% 
    unnest_tokens(word, description) %>%
    anti_join(stop_words)

count_word <- desc_words_tv %>%
   count(word, sort = TRUE)


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 30,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

```
