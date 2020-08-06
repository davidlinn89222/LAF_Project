INLA_model_selection <- function(formula, sp)
{
  library(INLA)
  
  # Do the first inla using original formula (input)
  res.bym.init <- 
    inla(formula,
         family = "poisson",
         data = sp@data,
         offset = log(population+1),
         control.inla = list(strategy = "simplified.laplace", int.strategy = "eb"),
         control.compute = list(dic = TRUE, waic = TRUE),
         control.predictor = list(compute = TRUE)
    )
  
  
  # first row using original formula to fit INLA
  res <- bind_cols(
    "takenWhich" = "original",
    "DIC" = res.bym.init$dic$dic, 
    "WAIC" = res.bym.init$waic$waic)
  
  # CI table used to check which is not significant
  CI_df <- res.bym.init$summary.fixed[, c("0.025quant", "0.975quant")]
  
  # name of X in original formula
  names_X <- rownames(CI_df)
  
  # name of nonsiginifcant variable
  names_notSig <- vector()
  
  for (i in 1:nrow(CI_df)) {
    lower_num <- CI_df[i, 1]
    upper_num <- CI_df[i, 2]
    
    if (0 >= lower_num && 0 <= upper_num) {
      names_notSig <- append(names_notSig, names_X[i])
    } 
  }
  
  print(names_notSig)
  names_notSig <- str_replace(names_notSig, pattern = "1", "")
  print(names_notSig)
  
  # name_notSig
  for (i in 1:length(names_notSig)) {
    
    # update formula
    formula.update <- update(formula, paste("~ . -",names_notSig[i]))
    
    # fit INLA for each new formula
    res.bym.loop <- 
      inla(formula.update,
           family = "poisson",
           data = sp@data,
           offset = log(population+1),
           control.inla = list(strategy = "simplified.laplace", int.strategy = "eb"),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(compute = TRUE)
      )
    
    temp <- bind_cols(
      "takenWhich" = names_notSig[i],
      "DIC" = res.bym.loop$dic$dic, 
      "WAIC" = res.bym.loop$waic$waic)
    
    res <- bind_rows(res, temp)
    
    print(paste(i, "finished!", sep = " "))
    
  }
  return(res)
}


# -----------------------------------------------------------------------------------------------------

ver1_template <- function(sf, vec, text) {
  ggplot(data = city_border_sf) + # 城市之間的邊界
    geom_sf()+ # 畫出上面的邊界
    geom_sf(data = temp, fill = NA)+ # 灰色區域label的部分（先建底）
    geom_sf(data = main_sf_un, color = "black")+ # 研究區域的邊界（透過union二級行政區）
    geom_sf(data = sf, aes(fill = vec),  color = "NA") + # Choroplet map想放的變數
    scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red") +  # 定義調色盤
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "aliceblue")) + # 外圍的經緯度
    coord_sf(xlim = c(121.2826, 121.8081), ylim = c(24.8617, 25.24436), expand = T) +   xlab("") + ylab("") +
    geom_label(data = temp, aes(lon, lat, label = TOWNENG), size = 3, fontface = "bold", 
               nudge_y = temp$nudge_y)
}





