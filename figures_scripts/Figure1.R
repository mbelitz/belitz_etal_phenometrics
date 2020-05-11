# load libraries
library(cowplot)
library(ggplot2)
library(latex2exp)

# Create a vector of observations taht will be used as example observations
testobs <- c(150,160,162,164,168,170,172,176,178,188)

#' Curve Intersect Function
#' This function takes two dataframes with x and y values that can be plotted
#' and calculates where the two lines intersect

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

#' Function to to solve for the CDF values of 0.01 and 0.99,
#'given our original observations

create_cdf_ends <- function(observations){
  weib <- fitdistrplus::fitdist(observations, distr = "weibull", method = "mle")
  cdf0 <- as.numeric(weib$estimate['scale']*(-log(1-0.01))^(1/weib$estimate['shape']))
  cdf100 <- as.numeric(weib$estimate['scale']*(-log(1-0.99))^(1/weib$estimate['shape']))
  # Here we add the calculated cdf values o our original observation vector
  added_vec <- sort(append(observations, values = c(cdf0, cdf100)), decreasing = FALSE)
  cdfadded <- 1 - exp(-(added_vec/weib$estimate['scale'])^weib$estimate['shape'])
  return(added_vec)
}

#' This function makes a data frame that will be used to plot a smooth
#' CDF from -0.001 to 1.001 given our original observations
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

# Now gather data needed to make plots given our test observations defined on line 7
cdf_df <- create_predict_df(testobs)
weib <- fitdistrplus::fitdist(testobs, distr = "weibull", method = "mle")
cdf_origpts <- 1 - exp(-(testobs/weib$estimate['scale'])^weib$estimate['shape'])

# Step 1 Plot CDF

step1 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y * 100)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts * 100)) + 
  labs(x = "Observation Dates", y = "Percentile") + 
  scale_y_continuous(expand = c(0,0), breaks = c(0,25,50,75,100)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 2 Estimate theta.hat orig for 90 Percentileile

nintydf <- data.frame(x = cdf_df$x, y = 0.9)
curve_intersect(nintydf, cdf_df) # intersects at x = 181.9951, y = 0.9

step2 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y * 100)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts * 100)) + 
  geom_segment(aes(x = 130, xend=181.9951,y=0.9 * 100,yend=0.9 * 100), linetype = 2) +
  geom_segment(aes(x = 181.9951, xend=181.9951,y=0,yend=0.9 * 100), linetype = 2) +
  annotate("text", x = 175, y = 0.08 * 100, size = 4, label = TeX("$\\hat{\\Theta}_{original}$")) +
  labs(x = "Observation Dates", y = "Percentile") + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,25,50,75,90,100)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 3 create new observation dates
set.seed(6)
sim_vector <- runif(n = length(testobs),min = 0, max = 1)

# new sim vector dates are 
a <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[1]), cdf_df)
b <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[2]), cdf_df) 
c <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[3]), cdf_df) 
d <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[4]), cdf_df) 
e <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[5]), cdf_df)
f <- curve_intersect(data.frame(x = cdf_df$x, y = 0.18), cdf_df) 
g <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[7]), cdf_df) 
h <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[8]), cdf_df) 
i <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[9]), cdf_df) 
j <- curve_intersect(data.frame(x = cdf_df$x, y = sim_vector[10]), cdf_df) 

newpts <- data.frame(
  x = c(a$x,b$x,c$x,d$x,e$x,f$x,g$x,h$x,i$x,j$x),
  y = c(a$y,b$y,c$y,d$y,e$y,f$y,g$y,h$y,i$y,j$y))

step3 <- ggplot() + 
  geom_line(cdf_df, mapping = aes(x = x, y = y * 100)) +
  geom_point(mapping = aes(x = testobs, y = cdf_origpts * 100), alpha = 0.75) +
  
  geom_segment(aes(x = 130, xend=a$x, y=a$y * 100,yend=a$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = a$x, xend=a$x,y=0,yend=a$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=b$x, y=b$y * 100,yend=b$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = b$x, xend=b$x,y=0,yend=b$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=c$x, y=c$y * 100,yend=c$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = c$x, xend=c$x,y=0,yend=c$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=d$x, y=d$y * 100,yend=d$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = d$x, xend=d$x,y=0,yend=d$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=e$x, y=e$y * 100,yend=e$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = e$x, xend=e$x,y=0,yend=e$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=f$x, y=f$y * 100,yend=f$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = f$x, xend=f$x,y=0,yend=f$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=g$x, y=g$y * 100,yend=g$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = g$x, xend=g$x,y=0,yend=g$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=h$x, y=h$y * 100,yend=h$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = h$x, xend=h$x,y=0,yend=h$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=i$x, y=i$y * 100,yend=i$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = i$x, xend=i$x,y=0,yend=i$y * 100), linetype = 2, color = "blue") +
  
  geom_segment(aes(x = 130, xend=j$x, y=j$y * 100,yend=j$y * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = j$x, xend=j$x,y=0,yend=j$y * 100), linetype = 2, color = "blue") +
  
  geom_point(data = newpts, mapping = aes(x = x, y = y * 100), color = "blue", alpha = 0.75) +
  
  labs(x = "Observation Dates", y = "Percentile") + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,25,50,75,100)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

# Step 4 create new cdf
newpts <- c(a$x, b$x, c$x, d$x, e$x, f$x, g$x, h$x, i$x, j$x)
newcdf <-  create_predict_df(newpts)
weib <- fitdistrplus::fitdist(newpts, distr = "weibull", method = "mle")
cdf_newpts <- 1 - exp(-(newpts/weib$estimate['scale'])^weib$estimate['shape'])

nintydf2 <- data.frame(x = newcdf$x, y = 0.9)
curve_intersect(nintydf2, newcdf) # intersects at x = 183.3495, y = 0.9

step4 <- ggplot() + 
  geom_line(newcdf, mapping = aes(x = x, y = y * 100), color = "blue") +
  geom_point(mapping = aes(x = newpts, y = cdf_newpts * 100), color = "blue") + 
  geom_segment(aes(x = 130, xend=183.3495,y=0.9 * 100,yend=0.9 * 100), linetype = 2, color = "blue") +
  geom_segment(aes(x = 183.3495, xend=183.3495,y=0,yend=0.9 * 100), linetype = 2, color = "blue") +
  labs(x = "Observation Dates", y = "Percentile") + 
  annotate("text", x = 180.5, y = 0.08 * 100, size = 4, label = TeX("$\\hat{\\Theta}_{i}$")) +
  scale_y_continuous(expand = c(0,0), breaks = c(0,25,50,75,90,100)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme_classic()

############ LATEX EQUATION ##############


step5 <- ggplot() + 
  annotate("text", x = 4, y = 35.5, size = 6, label = TeX("$\\bar{\\Theta } = 
                                                          \\hat{\\Theta} _{original} - 
                                                          \\hat{Bias}$")) +
  annotate("text", x = 4, y = 33, size = 6, label = TeX("$\\hat{Bias} = (\\frac{1}{B}\\sum_{i = 1}^{B}\\hat{\\Theta }_{i}) 
                                                        - \\hat{\\Theta }_{original}$")) + 
  annotate("text", x = 4, y = 30.5, size = 6, label = TeX("$\\bar{\\Theta } = 2 \\hat{\\Theta}_{original} - \\frac{1}{B}
                                                          \\sum_{i = 1}^{B}\\hat{\\Theta}_{i}$")) + 
  scale_y_continuous(limits = c(29,37)) + 
  labs(x = "", y = "") +
  theme_void()




cp <- plot_grid(step1, step2, step5, step3, step4,
                labels = c("Step 1", "Step 2", "Equation", "Step 3", "Step 4"
                ), vjust = 1, hjust = -1.25)

ggsave("figures_outputs/Fig1.png", cp, dpi = 300, width = 13, height = 9)
