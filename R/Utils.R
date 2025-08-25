

# FUNCTION: convert girth from inches to centimenters
convert_in_to_cm <- function(x) {
    x * 2.54
}

# FUNCTION: convert height from feet to meters
convert_ft_to_m <- function(x) {
    x * .3048
}

# FUNCTION: calculate volume in cubic meters
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter / 100)^2 * height
}