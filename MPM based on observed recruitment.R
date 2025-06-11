# Trying creating mpm_list based on observed recruitment, not estimated one


mpm_list


RepTot


RepTot2 <- RepTot %>%
  mutate(id = paste0("Plot_", Plot, "_", year)) %>%
  mutate(U = map(id, ~ mpm_list[[.x]] %||% NA))

RepTot2


F_list <- RepTot2 %>% 
  mutate(
    fecundity = t1 / t0,  # fecundidade observada bruta
    fecundity = ifelse(is.nan(fecundity) | fecundity < 0, 0, fecundity), # segurança
    F_matrix = map(fecundity, ~ {
      mat <- matrix(0, nrow=2, ncol=2)
      mat[1,2] <- .x
      mat
    })
  )

F_mpm_list <- F_list %>%
  select(id, F_matrix) %>%
  mutate(id = as.character(id)) %>%
  { set_names(.$F_matrix, .$id) }

F_mpm_list


F_list$F_matrix

names(F_mpm_list) == names(mpm_list)

common_names <- intersect(names(mpm_list), names(F_mpm_list))

# 2. Subsetar e reordenar as listas
U_list_aligned <- mpm_list[common_names]
F_list_aligned <- F_mpm_list[common_names]


length(U_list_aligned)
length(F_list_aligned)

A_list <-Map(`+`, U_list_aligned, F_list_aligned)

A_list


RepTot2%>%
  mutate(Recruit=t1/t0)%>%
  select(Recruit,id)%>%view()


lapply(A_list,lambda)
lapply(A_list,lambda)%>%unlist%>%mean()
lapply(A_list,lambda)%>%unlist%>%sd()

A_list 
names(F_list$F_matrix)
F_list
RepTot2 <- RepTot %>%
  mutate(id = paste0("Plot_", Plot, "_", year)) %>%
  mutate(U = map(id, ~ mpm_list[[.x]] %||% NA))%>%view()