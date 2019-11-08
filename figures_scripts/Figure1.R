library(cowplot)
library(ggplot2)
library(latex2exp)

testobs <- c(150,160,162,164,168,170,172,176,178,188)

curve_intersect <- function(curve1, curve2, empirical=TRUE, domain=NULL) {
  if (!empirical & missing(domain)) {
    stop("'domain' must be provided with non-empirical curves")
  }
  
  if (!empirical & (length(domain) != 2 | !is.numeric(domain))) {
    stop("'domain' must be a two-value numeric vector, like c(0, 10)")
  }
  
  if (empirical) {
    # Approximate the functional form of both curves
    curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
    curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
    
    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x),
                       c(min(curve1$x), max(curve1$x)))$root
    
    # Find where point_x is in curve 2
    point_y <- curve2_f(point_x)
  } else {
    # Calculate the intersection of curve 1 and curve 2 along the x-axis
    # within the given domain
    point_x <- uniroot(function(x) curve1(x) - curve2(x), domain)$root
    
    # Find where point_x is in curve 2
    point_y <- curve2(point_x)
  }
  
  return(list(x = point_x, y = point_y))
}

create_cdf_ends <- function(observations){
  weib <- fitdistrplus::fitdist(observations, distr = "weibull", method = "mle")
  cdf0 <- as.numeric(weib$estimate['scale']*(-log(1-0.01))^(1/weib$estimate['shape']))
  cdf100 <- as.numeric(weib$estimate['scale']*(-log(1-0.99))^(1/weib$estimate['shape']))
  
  added_vec <- sort(append(observations, values = c(cdf0, cdf100)), decreasing = FALSE)
  cdfadded <- 1 - exp(-(added_vec/weib$estimate['scale'])^weib$estimate['shape'])
  return(added_vec)
}

create_predict_df <- function(observations){
  
  added_vec <- create_cdf_ends(observations)
  vec_start <- min(added_vec)
  vec_end <- max(added_vec)
  new_vec <- seq(from = vec_start, to = vec_end, by = 0.5)
  
  weib <- fitdistrplus::fitdist(observations, distr = "weibull", method = "mle")
  cdfadded <- 1 - exp(-(new_vec/weib$estimate['scale'])^weib$estimate['shape'])
  
  cdf_df <- data.frame(x = new_vec, y = cdfadded)
  ends <- data.frame(x = c(min(added_vec - 1), max(added_vec+ 1)), y = c(-0.001,1.001))
  cdf_df <- rbind(cdf_df, ends)
  cdf_df <- cdf_df[order(cdf_df$x, decreasing = FALSE),]
  
  return(cdf_df)
  
}



cdf_df <- create_predict_df(testobs)
weib <- fitdistrplus::fitdist(testobs, distr = "weibull", method = "mle")
cdf_origpts <- 1 - exp(-(testobs/weib$estimate['scale'])^weib$estimate['shape'])

# Step 1 Plot CDF

step1 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts)) + 
  labs(x = "Observation Dates", y = "Percent") + 
  scale_y_continuous(expand = c(0,0), breaks = c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 2 Estimate theta.hat orig for 90 percentile

nintydf <- data.frame(x = cdf_df$x, y = 0.9)
curve_intersect(nintydf, cdf_df) # intersects at x = 181.9951, y = 0.9

step2 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts)) + 
  geom_segment(aes(x = 130, xend=181.9951,y=0.9,yend=0.9), linetype = 2) +
  geom_segment(aes(x = 181.9951, xend=181.9951,y=0,yend=0.9), linetype = 2) +
  annotate("text", x = 176.2, y = 0.08, size = 4, label = TeX("$\\hat{\\Theta}_{original}$")) +
  labs(x = "Observation Dates", y = "Percent") + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,0.25,0.5,0.75,0.9,1)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 3 create new observation dates
set.seed(33)
sim_vector <- runif(n = length(testobs),min = 0, max = 1)

# new sim vector dates are 0.44594048 0.39465031 0.48372887 0.91887596 0.84388144 
# 0.51734962 0.43712500 0.34319822 0.01551696 0.11799

curve_intersect(data.frame(x = cdf_df$x, y = 0.44594), cdf_df)# 168.4662
curve_intersect(data.frame(x = cdf_df$x, y = 0.3946), cdf_df) # 166.9187
curve_intersect(data.frame(x = cdf_df$x, y = 0.4837), cdf_df) #169.5494
curve_intersect(data.frame(x = cdf_df$x, y = 0.9189), cdf_df) # 182.897
curve_intersect(data.frame(x = cdf_df$x, y = 0.8439), cdf_df) # 179.7867
curve_intersect(data.frame(x = cdf_df$x, y = 0.5173), cdf_df) # 170.4846
curve_intersect(data.frame(x = cdf_df$x, y = 0.4371), cdf_df) # 168.2077
curve_intersect(data.frame(x = cdf_df$x, y = 0.3432), cdf_df) # 165.2498
curve_intersect(data.frame(x = cdf_df$x, y = 0.01552), cdf_df) # 137.0981
curve_intersect(data.frame(x = cdf_df$x, y = 0.11799), cdf_df) # 154.2974

newpts <- data.frame(
  x = c(168.466,166.91,169.54,182.89,179.78,170.48,168.20,165.24,137.098,154.297),
  y = c(0.44594,0.3946,0.4837,0.9189,0.8439,0.5173,0.4371,0.3432,0.01552,0.11799))

step3 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts), alpha = 0.75) +
  
  geom_segment(aes(x = 130, xend=154.2974, y=0.11799,yend=0.11799), linetype = 2, color = "blue") +
  geom_segment(aes(x = 154.2974, xend=154.2974,y=0,yend=0.11799), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=137.0981, y=0.01552,yend=0.01552), linetype = 2, color = "blue") +
  geom_segment(aes(x = 137.0981, xend=137.0981,y=0,yend=0.01552), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=165.2498, y=0.3432,yend=0.3432), linetype = 2, color = "blue") +
  geom_segment(aes(x = 165.2498, xend=165.2498,y=0,yend=0.3432), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=168.2077, y=0.4371,yend=0.4371), linetype = 2, color = "blue") +
  geom_segment(aes(x = 168.2077, xend=168.2077,y=0,yend=0.4371), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=170.4846, y=0.5173,yend=0.5173), linetype = 2, color = "blue") +
  geom_segment(aes(x = 170.4846, xend=170.4846,y=0,yend=0.5173), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=179.7867, y=0.8439,yend=0.8439), linetype = 2, color = "blue") +
  geom_segment(aes(x = 179.7867, xend=179.7867,y=0,yend=0.8439), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=168.4662, y=0.44594,yend=0.44594), linetype = 2, color = "blue") +
  geom_segment(aes(x = 168.4662, xend=168.4662,y=0,yend=0.44594), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=166.9187, y=0.3946,yend=0.3946), linetype = 2, color = "blue") +
  geom_segment(aes(x = 166.9187, xend=166.9187,y=0,yend=0.3946), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=169.5494, y=0.4837,yend=0.4837), linetype = 2, color = "blue") +
  geom_segment(aes(x = 169.5494, xend=169.5494,y=0,yend=0.4837), linetype = 2, color = "blue") +
  geom_segment(aes(x = 130, xend=182.897, y=.9189,yend=.9189), linetype = 2, color = "blue") +
  geom_segment(aes(x = 182.897, xend=182.897,y=0,yend=.9189), linetype = 2, color = "blue") +
  
  geom_point(data = newpts, mapping = aes(x = x, y = y), color = "blue", alpha = 0.75) +
  
  labs(x = "Observation Dates", y = "Percent") + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 4 create new cdf
newpts <- c(168.466,166.91,169.54,182.89,179.78,170.48,168.20,165.24,137.098,154.297)
newcdf <-  create_predict_df(newpts)
weib <- fitdistrplus::fitdist(newpts, distr = "weibull", method = "mle")
cdf_newpts <- 1 - exp(-(newpts/weib$estimate['scale'])^weib$estimate['shape'])

nintydf2 <- data.frame(x = newcdf$x, y = 0.9)
curve_intersect(nintydf2, newcdf) # intersects at x = 179.5915, y = 0.9

step4 <- ggplot() + 
  geom_line(newcdf, mapping = aes(x = x, y = y), color = "blue") +
  geom_point(mapping = aes(x = newpts, y = cdf_newpts), color = "blue") + 
  geom_segment(aes(x = 130, xend=179.5915,y=0.9,yend=0.9), linetype = 2, color = "blue") +
  geom_segment(aes(x = 179.5915, xend=179.5915,y=0,yend=0.9), linetype = 2, color = "blue") +
  labs(x = "Observation Dates", y = "Percent") + 
  annotate("text", x = 177.4, y = 0.08, size = 4, label = TeX("$\\hat{\\Theta}_{i}$")) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,0.25,0.5,0.75,0.9,1)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

############ LATEX EQUATION ##############


step5 <- ggplot() + 
  annotate("text", x = 4, y = 35.5, size = 6, label = TeX("$\\bar{\\Theta } = 
                                                          \\hat{\\Theta} _{original} - 
                                                          \\hat{Bias}$, where")) +
  annotate("text", x = 4, y = 33, size = 6, label = TeX("$\\hat{Bias} = (\\frac{1}{B}\\sum_{i = 1}^{B}\\hat{\\Theta }_{i}) 
                                                        - \\hat{\\Theta }_{original}$, so")) + 
  annotate("text", x = 4, y = 30.5, size = 6, label = TeX("$\\bar{\\Theta } = 2 \\hat{\\Theta}_{original} - \\frac{1}{B}
                                                          \\sum_{i = 1}^{B}\\hat{\\Theta}_{i}$")) + 
  scale_y_continuous(limits = c(29,37)) + 
  labs(x = "", y = "") +
  theme_void()




cp <- plot_grid(step1, step2, step5, step3, step4,
                labels = c("Step 1", "Step 2", "Equation", "Step 3", "Step 4"
                ), vjust = 1.5, hjust = -1.25)

ggsave("Figure_outputs/Fig1.png", cp, dpi = 300, width = 9, height = 6)
