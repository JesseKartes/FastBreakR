# PBP Corrections
# This file contains manual corrections for play-by-play data.

# Corrections for home team descriptions
home_corrections <- tribble(
  ~game_id, ~eventnum, ~eventmsgtype, ~eventmsgactiontype, ~homedescription,
  "0022200094", "542", "3", "16", "Lee Free Throw Technical (5 PTS)",
  "0022100249", "705", "3", "18", "MISS J. Jackson Free Throw Flagrant 1 of 2",
  "0022100249", "707", "3", "19", "J. Jackson Free Throw Flagrant 2 of 2 (5 PTS)"
)

# Corrections for visitor team descriptions
visitor_corrections <- tribble(
  ~game_id, ~eventnum, ~eventmsgtype, ~eventmsgactiontype, ~visitordescription,
  "0022200583", "615", "3", "10", "MISS Plumlee Free Throw 1 of 1",
  "0022100295", "405", "6", "2", "Lopez S.FOUL (P3.T2) (J. Capers)",
  "0021800036", "319", "3", "28", "Bazemore Free Throw Flagrant 2 of 3 (7 PTS)",
  "0021800036", "320", "3", "29", "Bazemore Free Throw Flagrant 3 of 3 (8 PTS)"
)

# Rows to be deleted from the dataset
delete_rows <- tribble(
  ~game_id, ~eventnum,
  "0022200731", "648",
  "0022200741", "267"
)

# Missing starters for specific games and periods
missing_starters <- tribble(
  ~game_id, ~period, ~team_player, ~name_player,
  "0022401102", 6, "DEN", "Peyton Watson",
  "0022401162", 5, "UTA", "Jaden Springer",
  "0022400985", 5, "PHI", "Justin Edwards",
  "0022400903", 5, "LAL", "Dorian Finney-Smith",
  "0022400771", 5, "WAS", "Kyshawn George",
  "0022400762", 5, "SAC", "Zach LaVine",
  "0022400501", 5, "MIA", "Tyler Herro",
  "0022400290", 5, "HOU", "Dilon Brooks",
  "0022400230", 5, "TOR", "Gradey Dick",
  "0022400223", 5, "DET", "Tobias Harris",
  "0022300162", 5, "PHX", "Grayson Allen",
  "0022300300", 5, "MIL", "Malik Beasley",
  "0022300517", 5, "BOS", "Derrick White",
  "0022300710", 5, "TOR", "Scottie Barnes",
  "0022300893", 5, "DEN", "Aaron Gordon",
  "0022301147", 5, "MIA", "Tyler Herro",
  "0022200234", 2, "LAL", "Kendrick Nunn",
  "0022200025", 5, "MIN", "Jaden McDaniels",
  "0022200039", 5, "WAS", "Delon Wright",
  "0022200040", 5, "UTA", "Mike Conley",
  "0022200072", 5, "BOS", "Al Horford",
  "0022200117", 5, "NOP", "Naji Marshall",
  "0022200117", 5, "LAL", "Austin Reaves",
  "0022200325", 5, "DET", "Isaiah Stewart",
  "0022200440", 5, "DAL", "Tim Hardaway Jr.",
  "0022200519", 5, "CHI", "Zach LaVine",
  "0022200659", 5, "TOR", "Gary Trent Jr.",
  "0022200748", 5, "SAS", "Keita Bates-Diop",
  "0022200758", 5, "SAC", "Harrison Barnes",
  "0022200892", 5, "OKC", "Jalen Williams",
  "0022201007", 5, "MIA", "Max Strus",
  "0022201194", 5, "NOP", "CJ McCollum",
  "0022201205", 5, "ATL", "Saddiq Bey",
  "0022100041", 5, "CHA", "Gordon Hayward",
  "0022100291", 6, "LAL", "Malik Monk",
  "0022100353", 5, "PHI", "Danny Green",
  "0022100413", 5, "BKN", "Kessler Edwards",
  "0022100688", 3, "POR", "Robert Covington",
  "0022100860", 5, "OKC", "Darius Bazley",
  "0022100967", 5, "NOP", "Tony Snell",
  "0022000023", 5, "DET", "Delon Wright",
  "0022000100", 5, "IND", "Justin Holiday",
  "0022000120", 5, "DEN", "Gary Harris",
  "0022000440", 5, "MIN", "Anthony Edwards",
  "0022000465", 5, "NOP", "Lonzo Ball",
  "0022000485", 1, "DAL", "Dorian Finney-Smith",
  "0022000637", 5, "CHI", "Coby White",
  "0022000645", 5, "IND", "T.J. McConnell",
  "0022001012", 5, "WAS", "Raul Neto",
  "0022001064", 5, "CHA", "Jalen McDaniels",
  "0021900023", 5, "DEN", "Malik Beasley",
  "0021900120", 5, "MIN", "Treveon Graham",
  "0021900272", 5, "ATL", "De'Andre Hunter",
  "0021900409", 5, "WAS", "Ish Smith",
  "0021900502", 5, "GSW", "Damion Lee",
  "0021900550", 5, "OKC", "Terrance Ferguson",
  "0021900563", 5, "DET", "Tony Snell",
  "0021900696", 5, "SAC", "Harrison Barnes",
  "0021900787", 5, "ATL", "De'Andre Hunter",
  "0021900892", 5, "HOU", "Eric Gordon",
  "0021901281", 6, "DEN", "Monte Morris",
  "0021800143", 6, "CHI", "Justin Holiday",
  "0021800143", 6, "NYK", "Noah Vonleh",
  "0021800216", 5, "BOS", "Marcus Morris Sr.",
  "0021800276", 3, "DEN", "Juancho Hernangomez",
  "0021800371", 5, "BKN", "Joe Harris",
  "0021800565", 5, "HOU", "P.J. Tucker",
  "0021800619", 5, "OKC", "Terrance Ferguson",
  "0021800881", 5, "UTA", "Joe Ingles",
  "0021801070", 5, "MEM", "Bruno Caboclo",
  "0021801132", 5, "GSW", "Andre Iguodala",
  "0021801229", 5, "UTA", "Tyler Cavanaugh",
  "0021800569", 5, "CHI", "Wendell Carter Jr.",
  "0021701136", 5, "MIL", "Jason Terry",
  "0021701103", 5, "NYK", "Courtney Lee",
  "0021701103", 5, "CHA", "Dwayne Bacon",
  "0021700966", 5, "TOR", "Kyle Lowry",
  "0021700893", 5, "MIL", "Eric Bledsoe",
  "0021700692", 6, "NOP", "Darius Miller",
  "0021700635", 5, "NOP", "Ian Clark",
  "0021700607", 5, "NYK", "Michael Beasley",
  "0021700584", 2, "NOP", "Dante Cunningham",
  "0021700064", 5, "WAS", "Kelly Oubre Jr.",
  "0021600976", 5, "NOP", "Solomon Hill",
  "0021600559", 6, "POR", "Allen Crabbe",
  "0021600359", 5, "NOP", "Langston Galloway",
  "0021600270", 5, "OKC", "Andre Roberson",
  "0021600270", 5, "WAS", "Otto Porter Jr.",
  "0021600253", 4, "SAC", "Garrett Temple",
  "0021600049", 5, "MIA", "Dion Waiters",
  "0021501197", 5, "MIL", "Tyler Ennis",
  "0021500721", 5, "CLE", "JR Smith",
  "0021500674", 5, "SAC", "Rudy Gay",
  "0021500624", 5, "LAC", "Paul Pierce",
  "0021500587", 5, "PHI", "Hollis Thompson",
  "0021500523", 6, "DAL", "Wesley Matthews",
  "0021500515", 5, "MIA", "Goran Dragic",
  "0021500359", 5, "DET", "Marcus Morris Sr.",
  "0021500025", 5, "DET", "Kentavious Caldwell-Pope"
)

# Create corrections list
corrections <- list(
  home_corrections = home_corrections,
  visitor_corrections = visitor_corrections,
  delete_rows = delete_rows,
  missing_starters = missing_starters
)

# Save to R/sysdata.rda for internal use
save(corrections, file = "inst/internal/sysdata.rda")
