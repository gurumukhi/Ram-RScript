# Challenge: Diagnose a list of passwords
# Difficulty: 3

# given a vector containing values that look like passwords
# ...confirm each element of the vector is a valid password
# ...save the invalid passwords with a diagnostic message into a data.frame

# a valid password has:
# more than 10 characters
# at least one lowercase letter
# at least one uppercase letter
# at least one number
# at least one punctuation character

# load survey data
load(file = "RCodeChallenge.rda")
