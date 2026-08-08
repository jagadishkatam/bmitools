#' calculate the bmi
#'
#' we are creating this function to derive the bmi with weight and height
#'
#' @param weight_kg numeric
#' @param height_cm numeric
#'
#' @return list with numeric BMI and category
#'  \describe{
#'    \item{bmi}{Numeric. BMI values are rounded to 1 decimal}
#'    \item{category}{character. }
#'      \code{'underwight'},
#'      \code{'normal weight'}
#'      \code{'overweight'}
#'      \code{'obese'}
#'  }
#'
#' @export
#'
#' @examples
#' calculate_bmi(95,168)
#'
#' @importFrom dplyr case_when
#'
calculate_bmi <- function(weight_kg, height_cm){
  if (height_cm <= 0) stop('Height must be positive')
  if (weight_kg <= 0) stop('weight must be positive')

  bmi <- weight_kg/(height_cm/100)^2

  category <- dplyr::case_when(
    bmi < 18.5 ~ 'Underweight',
    bmi < 25.0 ~ 'Normal Weight',
    bmi < 30.0 ~ 'Overweight',
    TRUE ~ 'obes',
  )

  return(list(bmi=round(bmi,1),category=category))
}
