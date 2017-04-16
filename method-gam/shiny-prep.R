


parties_ordered <- parties_df %>%
  mutate(party = gsub("M.ori", "Maori", Shortname)) %>%
  right_join(
    data_frame(
      party = c("National", "Labour", "NZ First", "Green", "Maori",
            "ACT", "United Future", "Mana", "Conservative")),
    by = "party")

save(parties, file = "nz-election-2017/parties.rda")
save(sims, file = "nz-election-2017/sims.rda")
save(parties_ordered, file = "nz-election-2017/parties_ordered.rda")
