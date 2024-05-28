weighted_ecdf<-function(dat, var){

  theme_min_rotex = theme_minimal()
  theme_min_rotex$axis.text.x = element_text(angle=90, hjust=1)

  dat = dat %>% dplyr::rename("y" = var) %>% dplyr::mutate(year = as.ordered(year))

  aggdat = dat %>%
    dplyr::group_by(year) %>%
    dplyr::reframe(qtile = seq(.01,.99,by=.01), est = spatstat.geom::weighted.quantile(y, fwc, probs = seq(.01,.99, by = .01)))

  plt<-
    ggplot() +
    geom_line(data = aggdat, aes(x=est, y = qtile, col = year))+
    theme_min_rotex +
    labs(title = var, x = NULL, y= "%")

  return(plt)


}
