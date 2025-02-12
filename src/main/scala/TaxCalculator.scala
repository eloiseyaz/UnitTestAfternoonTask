class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {

    val tax: Double = {

      if (income <= personalAllowance) {
        val allowanceTax: Double = income * personalAllowanceRate
        allowanceTax
      }

      else if (income <= basicRateLimit) {
        val allowanceTax: Double = personalAllowance * personalAllowanceRate
        val basicTax: Double = (income - personalAllowance) * basicRate
        allowanceTax + basicTax
      }

      else if (income <= higherRateLimit) {
        val allowanceTax: Double = personalAllowance * personalAllowanceRate
        val basicTax: Double = (basicRateLimit - personalAllowance) * basicRate
        val higherTax: Double = (income - basicRateLimit) * higherRate
        allowanceTax + basicTax + higherTax
      }

      else {
        val allowanceTax: Double = personalAllowance * personalAllowanceRate
        val basicTax: Double = (basicRateLimit - personalAllowance) * basicRate
        val higherTax: Double = (higherRateLimit - basicRateLimit) * higherRate
        val additionalTax: Double = (income - higherRateLimit) * additionalRate
        allowanceTax + basicTax + higherTax + additionalTax
      }
    }

    tax
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    income > basicRateLimit && income <= higherRateLimit
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {

    def leadingZeroes(numString: String): String = {
      if (numString.length == 1) "00" + numString
      else if (numString.length == 2) "0" + numString
      else numString
    }

    def formatLimit(limit: Int): String = {
      val preComma = (limit / 1000).toString
      val postComma = (limit % 1000).toString

      s"£$preComma,${leadingZeroes(postComma)}"
    }

    if (income <= personalAllowance) {
      formatLimit(personalAllowance)
    }

    else if (income <= basicRateLimit) {
      formatLimit(basicRateLimit)
    }

    else if (income <= higherRateLimit) {
      formatLimit(higherRateLimit)
    }

    else "No Limit"
  }

}
