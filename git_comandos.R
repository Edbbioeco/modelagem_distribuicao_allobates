# Pacote ----

library(gert)

# Listando arquivos ----

gert::git_status() |>
  as.data.frame()

# Selecionando o arquivo ----

gert::git_add(list.files(pattern = "README.md")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("README")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_soft(ref = "HEAD~1")

gert::git_reset_hard(ref = "HEAD~1")
