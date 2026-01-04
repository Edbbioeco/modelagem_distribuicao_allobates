# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_add("git_comandos.R")

# Commitando ----

gert::git_commit("Script de comandos de git")

# Pushando ----

gert::git_push(remote = "origin")

# Pullando ----

gert::git_pull()
