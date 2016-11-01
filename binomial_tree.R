library(data.table)
library(ggplot2)
library(magrittr)

get_edges <- function(n_steps) {
  res <- lapply(0:n_steps, function(t) {
    x_start <- t - 1
    x_end <- t
    y_end <- seq(from = t, to = -t, by = -2)
    
    y_start <- y_end - 1
    
    y_start <- y_start[-length(y_start)]
    
    if (t > 0) {
      values <- data.table(xs = x_start,
                           xe = x_end,
                           ys = rep(y_start, length(y_end)), 
                           ye = rep(y_end, length(y_start)))[abs(ys - ye) < 2]
    } else {
      nu <- numeric(0)
      values <- data.table(xs = nu,
                           xe = nu,
                           ys = nu,
                           ye = nu)
    }
    
    return(values)
  })
  return(rbindlist(res))
}
get_nodes <- function(n_steps) {
  res <- lapply(0:n_steps, function(t) {
    x <- t
    y <- seq(from = t, to = -t, by = -2)
    
    return(data.table(x = x, y = y))
  })
  res2 <- rbindlist(res)
  
  return(res2)
}
create_tree <- function(n_steps, type = "call", s_0 = 100, tick = 10, k = 100, rf = 0.1) {
  
  edges <- get_edges(n_steps)
  nodes <- get_nodes(n_steps)
  
  
  nodes[, s_t := y*tick + s_0]
  
  if (type == "call") {
    nodes[x == max(x), c_t := ifelse(s_t > k, s_t - k, 0)]
  } else if (type == "put") {
    nodes[x == max(x), c_t := ifelse(s_t < k, k - s_t, 0)]
  } else {
    stop("Unknown type, only 'call'/'put' allowed!")
  }
 
  nodes[, ':=' (delta = NA, b = NA)]
  
  for (lvl in (max(nodes$x) - 1):0) {
    tmp <- nodes[x == lvl]
    
    nodes_up <- nodes[x == lvl + 1][, .(y = y - 1, c_u = c_t, s_u = s_t)]
    nodes_down <- nodes[x == lvl + 1][, .(y = y + 1, c_d = c_t, s_d = s_t)]
    
    tmp <- merge(tmp, nodes_up, by = "y", all.x = T)
    tmp <- merge(tmp, nodes_down, by = "y", all.x = T)
    tmp[, delta := (c_u - c_d) / (s_u - s_d)]
    tmp[, b := (c_d - s_d * delta) / (1 + rf)]
    tmp[, c_t := s_t * delta + b]
    nodes <- rbindlist(list(tmp[, .(x, y, s_t, c_t, delta, b)],
                            nodes[x != lvl,]))
  }
  
  nodes[, label := paste("S[t] = ", s_t, 
                         "\nDelta = ", round(delta, 4),
                         "\nB = ", round(b, 4),
                         "\nC[t] = ", round(c_t, 4))]
  
  ggplot() + 
    theme_void() +
    geom_segment(data = edges, aes(x = xs, xend = xe, y = ys, yend = ye)) + 
    geom_label(data = nodes, 
               aes(x = x, y = y, label = label), fill = "white") + # parse = T
    geom_label(data = data.table(x = 0:max(nodes$x), y = max(nodes$y) + 1, 
                                 lab = paste0("t = ", 0:max(nodes$x))),
               aes(x = x, y = y, label = lab), fill = "white")
  
}

create_tree(n_steps = 10, type = "call", s_0 = 100, tick = 10, k = 100, rf = 0.1)
create_tree(n_steps = 3, type = "put", s_0 = 100, tick = 5, k = 100, rf = 0.1)
create_tree(n_steps = 3, type = "call", s_0 = 100, tick = 5, k = 100, rf = 0.01)

