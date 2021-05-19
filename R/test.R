library(blogdown)

?blogdown::new_site()

new_site(
  dir = ".",
  force = NA,
  install_hugo = TRUE,
  format = "yaml",
  sample = TRUE,
  theme = "sergioabreu-g/hugo-minimalist-spa",
  hostname = "github.com",
  theme_example = TRUE,
  empty_dirs = FALSE,
  to_yaml = TRUE,
  netlify = TRUE,
  .Rprofile = TRUE,
  serve = if (interactive()) "ask" else FALSE
)
