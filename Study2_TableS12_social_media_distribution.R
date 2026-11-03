#############################################
# Study 2, Table S12: Social media frequency distributions
# Survey-weighted percentages and unweighted valid-response counts.
#############################################
# Load analysis inputs
d <- read.csv("anes_w3.csv")
platforms <- c("Facebook", "Instagram", "Reddit", "TikTok", "Twitter", "YouTube")
headers <- c("Never", "Less than<br>once a month", "Once or twice<br>a month",
             "About once<br>a week", "A few times<br>per week", "Once a day",
             "More than<br>once a day")
# Facebook* uses its own seven ordered categories in these column positions.
# Its differing response labels are explained in the manuscript note.
# Calculate weighted percentages and unweighted counts for each platform.
# Include valid categories with finite, positive survey weights. Retain the
# check that each platform's percentages sum to 100.
rows <- vapply(platforms, function(platform) {
  variable <- paste0(tolower(platform), "_frequency")
  valid <- d[[variable]] %in% 1:7 & is.finite(d$w3weight) & d$w3weight > 0
  values <- d[[variable]][valid]
  weights <- d$w3weight[valid]
  percentages <- vapply(1:7, function(k) 100 * sum(weights[values == k]) / sum(weights), numeric(1))
  stopifnot(abs(sum(percentages) - 100) < 1e-8)
  label <- if (platform == "Facebook") "Facebook*" else platform
  paste0("<tr><th>", label, "</th><td>", format(sum(valid), big.mark = ",", trim = TRUE),
         "</td>", paste0(sprintf("<td>%.1f%%</td>", percentages), collapse = ""), "</tr>")
}, character(1))
# Table appearance (retain the submitted HTML and CSS settings)
css <- 'body{margin:0;background:white;color:#222;font-family:Times,"Times New Roman",serif}.sheet{padding:30px;box-sizing:border-box}table{border-collapse:collapse;table-layout:fixed;width:100%;font-size:25px;border-top:2px solid #333;border-bottom:2px solid #333}thead th{padding:18px 8px;border-bottom:1.5px solid #333;font-weight:bold;text-align:center;line-height:1.25}tbody th{font-weight:normal;text-align:left;padding:18px 12px}td{text-align:center;padding:18px 8px;font-variant-numeric:tabular-nums}'
# Render the table using webshot2
save_table <- function(body, width, filename) {
  html_file <- tempfile(fileext = ".html")
  on.exit(unlink(html_file), add = TRUE)
  writeLines(paste0('<!doctype html><meta charset="utf-8"><style>', css,
    '</style><div class="sheet" style="width:', width, 'px">', body, '</div>'), html_file)
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("Install webshot2 to render the table PNG.")
  }
    webshot2::webshot(html_file, file = filename, selector = ".sheet",
                     vwidth = width, zoom = 2)
  message("Saved ", filename)
}
# Assemble and save Table S12
body <- paste0('<table><colgroup><col style="width:170px"><col style="width:95px">',
               paste(rep('<col>', 7), collapse = ''), '</colgroup><thead><tr><th></th><th>N</th>',
               paste0('<th>', headers, '</th>', collapse = ''), '</tr></thead><tbody>',
               paste(rows, collapse = ''), '</tbody></table>')
save_table(body, 1800, "Plots/Study2_TableS12_social_media_distribution.png")
