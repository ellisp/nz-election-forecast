


parties_ordered <- parties_df %>%
  mutate(party = gsub("M.ori", "Maori", Shortname)) %>%
  right_join(
    data_frame(
      party = c("National", "Labour", "NZ First", "Green", "Maori",
            "ACT", "United Future", "Mana", "Conservative")),
    by = "party")

save(parties, file = "shiny/parties.rda")
save(sims, file = "shiny/sims.rda")
save(parties_ordered, file = "shiny/parties_ordered.rda")

deployApp("shiny", appName = app_name, account = "ellisp")
