cols = rev(c("grey50", "#de0100", "#0015bc"))

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}

cols_trans = c(t_col("#0015bc", percent = 80), t_col("#de0100", percent = 80))
n1 = 30
n2 = 40
n = n1*n2
for_indep = 23
for_trump = 561
for_biden = 616


set.seed(16)
my_sample = sample(1:n)

library("gifski")
png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path, units="px", width=1600*1.4, height=1600*1.13, res=300)
par(ask = FALSE, mar = c(3,0.75,0.75,0.75))
nb = 61
alpha = 0

for (k in 1:nb){
  plot(NA, xlim = c(0, 85), ylim = c(-7, 40), ann = FALSE, axes = FALSE)
  counter = counter2 = counter3 = counter4 = 0
  text(15 + 5, 40, "Population", cex = 1.25)
  lines(c(27,42)-3, c(34,34))
  ymax_biden = for_biden/n*6 + 34
  ymax_trump = for_trump/n*6 + 34
  ymax_indep = for_indep/n*6 + 34

  rect(28-3, 34, 31-3, ymax_biden, col = cols[1])
  rect(33-3, 34, 36-3, ymax_trump, col = cols[2])
  rect(38-3, 34, 41-3, ymax_indep, col = cols[3])

  text(29.5 -3, 32.6, "B")
  text(34.5 -3, 32.6, "T")
  text(39.5 -3, 32.6, "I")
  rect(29.5 - 1 - 3, 32.6 - 1, 29 + 1.5 - 3, 32 + 1.5, border = cols[1], lwd = 2)
  rect(4-2, 36.5, 6-2, 38, col = cols[1])
  rect(4-2, 34, 6-2, 35.5, col = cols[2])
  rect(4-2, 31.5, 6-2, 33, col = cols[3])

  text(15 - 11, 35.5/2 + 37/2 + 1,         paste("Biden: ",round(100*for_biden/n,1), "%", sep = ""), pos = 4, cex = 0.9)
  text(15 - 11, 35.5/2 + 37/2 - 2.5 + 1,   paste("Trump: ",round(100*for_trump/n,1), "%", sep = ""), pos = 4, cex = 0.9)
  text(15 - 11, 35.5/2 + 37/2 - 2.5 - 1.5, paste("Indep: ",round(100*for_indep/n,1), "%", sep = ""), pos = 4, cex = 0.9)

  for (i in 1:30){
    for (j in 1:40){
      counter = counter + 1
      if (counter <= for_biden){
        points(j,i, col = cols[1], pch = 16, cex = 0.9)
      }else{
        if (counter <= (for_biden + for_trump)){
          points(j,i, col = cols[2], pch = 16, cex = 0.9)
        }else{
          points(j,i, col = cols[3], pch = 16, cex = 0.9)
        }
      }
    }
  }


  if (k > 1){
    for (l in 1:(k-1)){
      for (h in 1:10){
        counter2 = counter2 + 1
        pos = my_sample[10*(l-1) + h]
        mod = pos %% n2
        ypos = floor(pos/n2) + 1
        if (ypos > n1){
          ypos = n1
        }
        points(mod + 1, ypos, col = "white", pch = 16, cex = 0.8)
      }
    }

    for (g in 1:counter2){
      if (my_sample[g] <= for_biden){
        counter3 = counter3 + 1
        points((g-1) %% (n2/2) + 55, floor((g-1)/(n2/2)) + 1, col = cols[1], pch = 16, cex = 0.9)
      }else{
        if (my_sample[g] <= (for_biden + for_trump)){
          counter4 = counter4 + 1
          points((g-1) %% (n2/2) + 55, floor((g-1)/(n2/2)) + 1, col = cols[2], pch = 16, cex = 0.9)
        }else{
          points((g-1) %% (n2/2) + 55, floor((g-1)/(n2/2)) + 1, col = cols[3], pch = 16, cex = 0.9)
        }
      }
    }

    delta = 43 - 2
    text(15 + delta + 5, 40, paste("Sample (", round(counter2, 1), " voters)", sep = ""), cex = 1.25)
    lines(c(27,42) + delta, c(34,34))
    ymax1 = (counter3/counter2)*6 + 34
    ymax2 = (counter4/counter2)*6 + 34
    ymax3 = ((counter2 - counter3 - counter4)/counter2)*6 + 34

    rect(28 + delta, 34, 31 + delta, ymax1, col = cols[1])
    rect(33 + delta, 34, 36 + delta, ymax2, col = cols[2])
    rect(38 + delta, 34, 41 + delta, ymax3, col = cols[3])

    text(29.5 + delta, 32.6, "B")
    text(34.5 + delta, 32.6, "T")
    text(39.5 + delta, 32.6, "I")

    rect(4 + delta, 36.5, 6 + delta, 38, col = cols[1])
    rect(4 + delta, 34, 6 + delta, 35.5, col = cols[2])
    rect(4 + delta, 31.5, 6 + delta, 33, col = cols[3])

    text(15 + delta -1 - 8, 35.5/2 + 37/2 + 1,         paste("Biden: ",round(100*(counter3/counter2),1), "%", sep = ""), pos = 4, cex = 0.9)
    text(15 + delta -1 - 8, 35.5/2 + 37/2 - 2.5 + 1,   paste("Trump: ",round(100*(counter4/counter2),1), "%", sep = ""), pos = 4, cex = 0.9)
    text(15 + delta -1 - 8, 35.5/2 + 37/2 - 2.5 - 1.5, paste("Indep: ",round(100*((counter2 - counter3 - counter4)/counter2),1), "%", sep = ""), pos = 4, cex = 0.9)

    if (counter3/counter2 > 0.5){
      if (counter3/counter2 - qnorm(1-alpha/2)*sqrt(counter3/counter2*(1-counter3/counter2)/counter2) > 0.5){
        rect(29.5 + delta - 1, 32.6 -1, 29 + delta + 1.5, 32 + 1.5, border = cols[1], lwd = 2)
      }
    }else{
      if (counter3/counter2 < 0.5){
        if (counter3/counter2 + qnorm(1-alpha/2)*sqrt(counter3/counter2*(1-counter3/counter2)/counter2) < 0.5){
          rect(34.5 + delta-1, 32.6 -1, 34 + delta + 1.5, 32 + 1.5, border = cols[2], lwd = 2)
        }
      }
    }
  }

  if (k > 1){
  prop_min = 55 - 10
  prop_max = 74 + 10
  prop_y = -2.5
  delta_y_prop = 0.5
  lines(c(prop_min, prop_max), c(prop_y, prop_y))
  d = prop_max - prop_min

  for (u in 0:10){
    lines(c(prop_min, prop_min) + u/10*d, c(prop_y - delta_y_prop,prop_y+delta_y_prop))
    text(prop_min + u/10*d, -5, u/10*100, cex = 0.9)
  }

  lines(c(prop_min,prop_min) + 5/10*d, c(prop_y - 1.5*delta_y_prop,prop_y+1.5*delta_y_prop), lwd = 2)
  text(prop_min, -0.5, "Trump", cex = 0.9)
  text(prop_min + d, -0.5, "Biden", cex = 0.9)

  text(65, -7, "Estimated proportion of votes for Biden (%)", cex = 0.7)

  if (counter3/counter2 >= 0.5){
    points(prop_min + counter3/counter2*d, prop_y, col = cols[1], cex = 2, pch = 16)
    if (alpha == 0){
      rect(prop_min + d, prop_y - 1,
           prop_min,
           prop_y + 1, col = cols_trans[1], border = "NA")
    }else{
      rect(prop_min + counter3/counter2*d + qnorm(1-alpha/2)*d*sqrt(counter3/counter2*(1-counter3/counter2)/counter2), prop_y - 1,
           prop_min + counter3/counter2*d - qnorm(1-alpha/2)*d*sqrt(counter3/counter2*(1-counter3/counter2)/counter2),
           prop_y + 1, col = cols_trans[1], border = "NA")
    }

  }else{
    if (alpha == 0){
      points(prop_min + counter3/counter2*d, prop_y, col = cols[2], cex = 2, pch = 16)
      rect(prop_min + 1*d, prop_y - 1,
           prop_min,
           prop_y + 1, col = cols_trans[2], border = "NA")
    }else{
      points(prop_min + counter3/counter2*d, prop_y, col = cols[2], cex = 2, pch = 16)
      rect(prop_min + counter3/counter2*d + qnorm(1-alpha/2)*d*sqrt(counter3/counter2*(1-counter3/counter2)/counter2), prop_y - 1,
           prop_min + counter3/counter2*d - qnorm(1-alpha/2)*d*sqrt(counter3/counter2*(1-counter3/counter2)/counter2),
           prop_y + 1, col = cols_trans[2], border = "NA")

    }
  }
}
}

dev.off()


png_files <- sprintf(png_path, 1:nb)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, "GIF/sample_ci0.gif", delay = 0.5, loop = TRUE, progress = TRUE)
unlink(png_files)
utils::browseURL(gif_file)
