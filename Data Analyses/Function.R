# Save as Function.R

# Math Formula
lhs <- function(x) {
  log10(0.5*x + sqrt(0.25*(x^2) + 1)) 
}
antilog <- function(y) {
  (10^(2*y)-1)/(10^y)
}
ev <- function(y) {
  1 / (100 * y)
}
# Zero-Bounded Demand Curve Function
zbm_fct <- function(fr,a,b) {log10(0.5*exp(b) + sqrt(0.25*(exp(b)^2) + 1)) * (exp((-exp(a) / (log10(0.5*exp(b) + sqrt(0.25*(exp(b)^2) + 1))))*exp(b)*fr))}
zbm_grad <- deriv(body(zbm_fct)[[2]], 
                  namevec = c("a","b"), 
                  function.arg = zbm_fct)
# ggplot2 
mattheme <- theme(text = element_text(size = 14, family = "serif", color = "black", face = "bold"), 
                  axis.text.y = element_text(colour = "black", size = 12, face = "bold", family = "serif"), 
                  axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 0, family = "serif"), 
                  axis.title.x = element_text(size = 16,vjust = -0.75, family = "serif"), 
                  axis.title.y = element_text(size = 16, family = "serif"), 
                  panel.border = element_blank(),
                  axis.line.x = element_line(), 
                  axis.line.y = element_line(), 
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "serif"), 
                  panel.background = element_rect(fill = "white", linetype = 1, color = "white", size = 1), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), 
                  plot.background = element_rect(fill = "white"),
                  strip.background =element_rect(fill="white", color = "white", size = 1),
                  strip.text = element_text(size=14, family = "serif"),
                  legend.key = element_rect(fill = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  legend.text=element_text(size = 10,family = "serif", color = "black", face = "bold"),
                  legend.position = "bottom",
                  legend.margin=margin(t=0))