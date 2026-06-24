pre <- read.csv("Data/coco_ut2_presurvey.csv", sep = ";")
post <- read.csv("Data/coco_ut2_postsurvey.csv", sep = ";")

d <- merge(
  pre[, c("id", "political_orientation")],
  post[, c("id", "political_orientation_t2")],
  by = "id"
)

d$pre <- as.numeric(d$political_orientation)
d$post <- as.numeric(d$political_orientation_t2)
d <- d[complete.cases(d[, c("pre", "post")]), ]

pearson <- cor.test(d$pre, d$post, method = "pearson")
spearman <- cor.test(d$pre, d$post, method = "spearman", exact = FALSE)
diff <- d$post - d$pre
paired_t <- t.test(d$post, d$pre, paired = TRUE)

cat("UT2 pre/post political orientation comparison\n")
cat("N complete:", nrow(d), "\n\n")

cat("Pearson r:", round(unname(pearson$estimate), 3),
    "p =", signif(pearson$p.value, 3),
    "95% CI [", round(pearson$conf.int[1], 3), ",",
    round(pearson$conf.int[2], 3), "]\n")

cat("Spearman rho:", round(unname(spearman$estimate), 3),
    "p =", signif(spearman$p.value, 3), "\n\n")

cat("Exact agreement:", round(mean(d$pre == d$post) * 100, 1), "%\n")
cat("Within 1 point:", round(mean(abs(diff) <= 1) * 100, 1), "%\n")
cat("Mean pre:", round(mean(d$pre), 2), "SD:", round(sd(d$pre), 2), "\n")
cat("Mean post:", round(mean(d$post), 2), "SD:", round(sd(d$post), 2), "\n")
cat("Mean change post - pre:", round(mean(diff), 2), "\n")
cat("Paired t-test: t(", round(unname(paired_t$parameter), 0), ") = ",
    round(unname(paired_t$statistic), 2),
    ", p = ", signif(paired_t$p.value, 3), "\n\n", sep = "")

b <- d[d$pre != 4 & d$post != 4, ]
b$pre_bin <- ifelse(b$pre <= 3, "Liberal", "Conservative")
b$post_bin <- ifelse(b$post <= 3, "Liberal", "Conservative")

tab <- table(Pre = b$pre_bin, Post = b$post_bin)
accuracy <- sum(diag(tab)) / sum(tab)
expected <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
kappa <- (accuracy - expected) / (1 - expected)

cat("Binary liberal/conservative comparison, excluding midpoint 4\n")
cat("N:", nrow(b), "\n")
print(tab)
cat("Accuracy:", round(accuracy * 100, 1), "%\n")
cat("Cohen's kappa:", round(kappa, 3), "\n")
