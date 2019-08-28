three_mix <- data.frame(
  subject = rep(1:8, each = 3),
  A = rep(paste0("A", 1:3), 8),
  B = rep(paste0("B", 1:2), each = 12),
  C = rep(rep(paste0("C", 1:2), each = 6), 2),
  stringsAsFactors = FALSE)

three_way <- data.frame(
  subject = 1:24,
  A = rep(paste0("A", 1:3), 8),
  B = rep(paste0("B", 1:2), each = 12),
  C = rep(rep(paste0("C", 1:2), each = 6), 2),
  stringsAsFactors = FALSE)

two_within <- data.frame(
  subject = rep(1:6, each = 4),
  A = rep(rep(paste0("A", 1:2), each = 2), 6),
  B = rep(paste0("B", 1:2), 12),
  stringsAsFactors = FALSE)
