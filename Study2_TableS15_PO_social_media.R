#############################################
# Study 2, Table S15: Political orientation and social media frequency
# Full-sample results; platforms in alphabetical order. No bootstrap is rerun.
#############################################
# Load analysis inputs
d <- read.csv('anes_w3.csv')
ms <- c(facebook_frequency='Facebook',instagram_frequency='Instagram',reddit_frequency='Reddit',tiktok_frequency='TikTok',twitter_frequency='Twitter',youtube_frequency='YouTube')
cs <- c('political_orientation','age','gender','race_ethnicity','income','education','w3weight')
fp <- function(p) ifelse(p<.001,'&lt;.001',fmt(p))
fci <- function(e,l,h,k=3) paste0(sprintf(paste0('%.',k,'f'),e),' [',sprintf(paste0('%.',k,'f'),l),', ',sprintf(paste0('%.',k,'f'),h),']')
td <- function(x,sig=FALSE) paste0('<td',ifelse(sig,' class="sig"',''),'>',x,'</td>')
tr <- function(label,cells) paste0('<tr><th class="stub">',label,'</th>',paste(cells,collapse=''),'</tr>')
headrow <- function(headers) paste0('<tr>',paste0('<th>',headers,'</th>',collapse=''),'</tr>')
# Table appearance (retain the submitted HTML and CSS settings)
css <- 'body{margin:0;color:#222;background:#fff;font-family:Times,"Times New Roman",serif}.sheet{padding:28px 30px;box-sizing:border-box}table{border-collapse:collapse;table-layout:fixed;width:100%;font-size:25px;border-top:2px solid #333;border-bottom:2px solid #333}thead th{font-weight:bold;padding:15px 10px;line-height:1.2;border-bottom:1.5px solid #333;text-align:center}td{padding:13px 12px;text-align:center;white-space:nowrap;font-variant-numeric:tabular-nums}.stub{padding:13px 14px;text-align:left;font-weight:normal}.sig{font-weight:bold}.platform th{text-align:left;font-weight:bold;padding:20px 14px 6px;font-size:26px}.freqhead th{padding:14px 6px;border-bottom:1px solid #666;text-align:center;line-height:1.2;font-size:23px}.spacer td{height:15px;padding:0}.sub{font-size:22px;font-weight:normal}.btable{font-size:23px}.btable td{padding:11px 5px}.btable .stub{padding:11px 10px 11px 25px}.acme td{padding:18px 14px;line-height:1.45}'
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
getdata <- function(vars){ d[complete.cases(d[,unique(c(vars,cs))]) & is.finite(d$w3weight) & d$w3weight>0,,drop=FALSE] }

# Fit H2 models and assemble table rows
# Fit six weighted proportional-odds models using complete cases and positive
# weights. Exponentiate coefficients and confidence limits to obtain odds
# ratios. Retain the convergence check and existing formatting.
rows <- c()
for(m in names(ms)){
 md <- getdata(m);md$mediator_value <- ordered(md[[m]],levels=1:7)
 fit <- suppressWarnings(MASS::polr(mediator_value ~ political_orientation+age+gender+race_ethnicity+income+education,data=md,weights=w3weight,Hess=TRUE,method='logistic',control=list(maxit=1000,reltol=1e-10)))
 stopifnot(fit$convergence==0)
 e <- coef(fit)['political_orientation'];se <- sqrt(vcov(fit)['political_orientation','political_orientation']);pv <- 2*pnorm(-abs(e/se))
 rows <- c(rows,tr(ms[[m]],c(td(fci(exp(e),exp(e-qnorm(.975)*se),exp(e+qnorm(.975)*se)),pv<.05),td(fp(pv),pv<.05))))
}
# Save Table S15
save_table(paste0('<table><colgroup><col style="width:35%"><col style="width:48%"><col></colgroup><thead>',headrow(c('','Odds ratio [95% CI]','<i>p</i>')),'</thead><tbody>',paste(rows,collapse=''),'</tbody></table>'),1100, "Plots/Study2_TableS15_PO_social_media.png")

