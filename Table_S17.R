#############################################
# Table S17: ACME mediation results
# Run from Analysis/. Writes only Plots/tables_s17.png.
# Full-sample results; platforms in alphabetical order. No bootstrap is rerun.
#############################################
r <- read.csv('anes_monotonic_mediation_results.csv')
r <- r[r$sample=='all' & r$model_type=='mediation',]
ys <- c(loneliness='Loneliness (r)',anhedonia='Anhedonia (r)',depression='Depression (r)')
ms <- c(facebook_frequency='Facebook',instagram_frequency='Instagram',reddit_frequency='Reddit',tiktok_frequency='TikTok',twitter_frequency='Twitter',youtube_frequency='YouTube')
cs <- c('political_orientation','age','gender','race_ethnicity','income','education','w3weight')
fmt <- function(x,k=3) sub('^(-?)0\\.','\\1.',sprintf(paste0('%.',k,'f'),x))
fp <- function(p) ifelse(p<.001,'&lt;.001',fmt(p))
fci <- function(e,l,h,k=3) paste0(fmt(e,k),' [',fmt(l,k),', ',fmt(h,k),']')
td <- function(x,sig=FALSE) paste0('<td',ifelse(sig,' class="sig"',''),'>',x,'</td>')
tr <- function(label,cells) paste0('<tr><th class="stub">',label,'</th>',paste(cells,collapse=''),'</tr>')
headrow <- function(headers) paste0('<tr>',paste0('<th>',headers,'</th>',collapse=''),'</tr>')
css <- 'body{margin:0;color:#222;background:#fff;font-family:Times,"Times New Roman",serif}.sheet{padding:28px 30px;box-sizing:border-box}table{border-collapse:collapse;table-layout:fixed;width:100%;font-size:25px;border-top:2px solid #333;border-bottom:2px solid #333}thead th{font-weight:bold;padding:15px 10px;line-height:1.2;border-bottom:1.5px solid #333;text-align:center}td{padding:13px 12px;text-align:center;white-space:nowrap;font-variant-numeric:tabular-nums}.stub{padding:13px 14px;text-align:left;font-weight:normal}.sig{font-weight:bold}.platform th{text-align:left;font-weight:bold;padding:20px 14px 6px;font-size:26px}.freqhead th{padding:14px 6px;border-bottom:1px solid #666;text-align:center;line-height:1.2;font-size:23px}.spacer td{height:15px;padding:0}.sub{font-size:22px;font-weight:normal}.btable{font-size:23px}.btable td{padding:11px 5px}.btable .stub{padding:11px 10px 11px 25px}.acme td{padding:18px 14px;line-height:1.45}'
save_table <- function(body, width, filename) {
  html_file <- tempfile(fileext = ".html")
  renderer <- tempfile(fileext = ".cjs")
  on.exit(unlink(c(html_file, renderer)), add = TRUE)
  writeLines(paste0('<!doctype html><meta charset="utf-8"><style>', css,
    '</style><div class="sheet" style="width:', width, 'px">', body, '</div>'), html_file)
  if (requireNamespace("webshot2", quietly = TRUE)) {
    webshot2::webshot(html_file, file = filename, selector = ".sheet",
                     vwidth = width, zoom = 2)
  } else {
    # Local fallback when webshot2 is unavailable; no additional output files.
    runtime <- file.path(path.expand("~"), ".cache/codex-runtimes/codex-primary-runtime/dependencies/node")
    node <- file.path(runtime, "bin/node")
    module <- file.path(runtime, "node_modules/playwright")
    chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    if (!file.exists(node) || !dir.exists(module) || !file.exists(chrome))
      stop("Install webshot2 to render the table PNG.")
    writeLines(c(
      "const {chromium}=require(process.argv[2]);",
      "(async()=>{const b=await chromium.launch({executablePath:process.argv[3],headless:true});",
      "try{const p=await b.newPage({viewport:{width:Number(process.argv[6]),height:1600},deviceScaleFactor:2});",
      "await p.goto(require('url').pathToFileURL(process.argv[4]).href);await p.evaluate(()=>document.fonts.ready);",
      "const clipped=await p.locator('td,th').evaluateAll(es=>es.some(e=>e.scrollWidth>e.clientWidth+2));",
      "if(clipped)throw Error('Table contains clipped cells');",
      "await p.locator('.sheet').screenshot({path:process.argv[5]});}finally{await b.close();}})().catch(e=>{console.error(e);process.exit(1)});"
    ), renderer)
    status <- system2(node, args = vapply(c(renderer, module, chrome,
      normalizePath(html_file), file.path(getwd(), filename), as.character(width)), shQuote, ""))
    if (status != 0) stop("Table rendering failed")
  }
  message("Saved ", filename)
}

# Main ACME: saved estimates/intervals/proportions, with no refitting.
rows <- c()
for(m in names(ms)){
 cells <- vapply(names(ys),function(y){
 a <- r[r$mediator==m & r$outcome==y,];stopifnot(nrow(a)==1)
 td(paste0(fci(a$acme_est,a$acme_ci_low,a$acme_ci_high,3),'<br>(',sprintf('%.2f',100*a$prop_mediated_est),'%)'),a$acme_ci_low>0|a$acme_ci_high<0)
 },'')
 rows <- c(rows,tr(ms[[m]],cells))
}
save_table(paste0('<table class="acme"><colgroup><col style="width:220px"><col><col><col></colgroup><thead>',headrow(c('Platform',unname(ys))),headrow(c('',rep('<span class="sub">ACME [95% CI]<br>(Prop. mediated)</span>',3))),'</thead><tbody>',paste(rows,collapse=''),'</tbody></table>'),1600, "Plots/tables_s17.png")

