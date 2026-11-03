#############################################
# Study 2, Table S16: Social media frequency and wellbeing
# Full-sample results; platforms in alphabetical order. No bootstrap is rerun.
#############################################
# Load analysis inputs
d <- read.csv('anes_w3.csv')
# Saved main-analysis results are produced by Study2_analysis.R.
r <- read.csv('anes_monotonic_mediation_results.csv')
r <- r[r$sample=='all' & r$model_type=='mediation',]
ys <- c(loneliness='Loneliness (r)',anhedonia='Anhedonia (r)',depression='Depression (r)')
ms <- c(facebook_frequency='Facebook',instagram_frequency='Instagram',reddit_frequency='Reddit',tiktok_frequency='TikTok',twitter_frequency='Twitter',youtube_frequency='YouTube')
cs <- c('political_orientation','age','gender','race_ethnicity','income','education','w3weight')
fmt <- function(x,k=3) sub('^(-?)0\\.','\\1.',sprintf(paste0('%.',k,'f'),x))
fp <- function(p) ifelse(p<.001,'&lt; .001',paste0('= ',fmt(p)))
fci <- function(e,l,h,k=3) paste0(fmt(e,k),' [',fmt(l,k),', ',fmt(h,k),']')
td <- function(x,sig=FALSE) paste0('<td',ifelse(sig,' class="sig"',''),'>',x,'</td>')
tr <- function(label,cells) paste0('<tr><th class="stub">',label,'</th>',paste(cells,collapse=''),'</tr>')
# Table appearance (retain the submitted HTML and CSS settings)
css <- 'body{margin:0;color:#222;background:#fff;font-family:Times,"Times New Roman",serif}.sheet{padding:28px 30px;box-sizing:border-box}table{border-collapse:collapse;table-layout:fixed;width:100%;font-size:25px;border-top:2px solid #333;border-bottom:2px solid #333}thead th{font-weight:bold;padding:15px 10px;line-height:1.2;border-bottom:1.5px solid #333;text-align:center}td{padding:13px 12px;text-align:center;white-space:nowrap;font-variant-numeric:tabular-nums}.stub{padding:13px 14px;text-align:left;font-weight:normal}.sig{font-weight:bold}.platform th{text-align:left;font-weight:bold;padding:20px 14px 6px;font-size:26px}.freqhead th{padding:14px 6px;border-bottom:1px solid #666;text-align:center;line-height:1.2;font-size:23px}.spacer td{height:15px;padding:0}.sub{font-size:22px;font-weight:normal}.sig .sub{font-weight:bold}.btable{font-size:23px}.btable td{padding:11px 5px}.btable .stub{padding:11px 10px 11px 25px}.acme td{padding:18px 14px;line-height:1.45}'
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

# Fit H3 models and assemble table rows
# Compare frequency categories 2-7 with category 1: never for most platforms,
# less than once a month for Facebook. Use the same weighted categorical
# outcome models and complete cases as Study2_analysis.R.
# Check sample sizes and political-orientation coefficients against its
# saved main-analysis results.
heads <- c('Less than<br>once a month','Once or twice<br>a month','About once<br>a week','A few times<br>per week','Once a day','More than<br>once a day')
# Facebook* uses different response labels; columns show its ordered category
# contrasts in the same positions. The manuscript caption supplies the mapping.
rows <- paste0('<tr class="freqhead">',paste0('<th>',c('Wellbeing outcome',heads),'</th>',collapse=''),'</tr>')
for(m in names(ms)){
 label <- if(m=='facebook_frequency') 'Facebook*' else ms[[m]]
 rows <- c(rows,paste0('<tr class="platform"><th colspan="7">',label,'</th></tr>'))
 for(y in names(ys)){
 md <- getdata(c(y,m));md$mediator_value <- factor(md[[m]],levels=1:7);md$outcome_z <- as.numeric(scale(md[[y]]))
 fit <- lm(outcome_z ~ political_orientation+mediator_value+age+gender+race_ethnicity+income+education,data=md,weights=w3weight)
 terms <- paste0('mediator_value',2:7);co <- summary(fit)$coefficients[terms,];ci <- confint(fit)[terms,]
 check <- r[r$outcome==y & r$mediator==m,]
 stopifnot(nrow(md)==check$n,abs(coef(fit)['political_orientation']-check$c_prime_est)<1e-8)
 rows <- c(rows,tr(ys[[y]],vapply(1:6,function(i)td(paste0(fci(co[i,1],ci[i,1],ci[i,2]),'<br><span class="sub"><i>p</i> ',fp(co[i,4]),'</span>'),co[i,4]<.05),'')))
 }
}
# Save Table S16
save_table(paste0('<table class="btable"><colgroup><col style="width:235px">',paste(rep('<col>',6),collapse=''),'</colgroup><tbody>',paste(rows,collapse=''),'</tbody></table>'),1900, "Plots/Study2_TableS16_social_media_wellbeing.png")

