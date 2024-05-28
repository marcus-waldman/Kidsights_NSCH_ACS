weighted_bar<-function(dat, var){

  theme_min_rotex = theme_minimal()
  theme_min_rotex$axis.text.x = element_text(angle=90, hjust=1)

  dat = dat %>% dplyr::rename("y" = var)

  plt<- pollster::crosstab(dat, x = year, y = y, weight = fwc, format = "long") %>%
    ggplot(aes(y, pct, fill = as.ordered(year))) +
    geom_bar(stat = "identity", position = "dodge", show.legend = F) +
    theme_min_rotex +
    labs(title = var, x = NULL, y= "%")

  return(plt)


}
