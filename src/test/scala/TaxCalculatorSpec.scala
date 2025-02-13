import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {

      "the income is below the personal tax allowance" in {
        val result: Double = taxCalculator.calculateTax(5000)
        assert(result == 0)
      }

      "the income is equal to the personal tax allowance" in {
        val result: Double = taxCalculator.calculateTax(12570)
        assert(result == 0)
      }

      "the income is above the personal tax allowance but below the basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(40000)
        assert(result ==  5486)
      }

      "the income is equal to the basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(50270)
        assert(result == 7540)
      }

      "the income is above the basic rate limit but below the higher rate limit" in {
        val result1: Double = taxCalculator.calculateTax(100000)
        assert(result1 == 27432)
        val result2: Double = taxCalculator.calculateTax(110000)
        assert(result2 == 33432)
      }

      "the income is equal to the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(125140)
        assert(result == 42516)
      }

      "the income is above the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(200000)
        assert(result == 76203)
      }

    }
  }

  "TaxCalculator.isHigherRateTaxpayer" should {

    "return true" when {

      "the income is above the basic rate limit and below the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(100000)
        assert(result)
      }

      "the income is equal to the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125140)
        assert(result)
      }

    }

    "return false" when {

      "the income is below the basic rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(20000)
        assert(!result)
      }

      "the income is equal to the basic rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(50270)
        assert(!result)
      }

      "the income is above the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(500000)
        assert(!result)
      }

    }

  }

  "TaxCalculator.formattedCurrentTaxAllowance" should {

    "return a formatted string of the personal tax allowance" when {

      "the income is below the personal tax allowance" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(5000)
        val expected: String = "£12,570"
        assert(result == expected)
      }

      "the income is equal to the personal tax allowance" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(12570)
        val expected: String = "£12,570"
        assert(result == expected)
      }

    }

    "return a formatted string of the basic rate limit" when {

      "the income is above the personal allowance rate and below the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(20000)
        val expected: String = "£50,270"
        assert(result == expected)
      }

      "the income is equal to the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(50270)
        val expected: String = "£50,270"
        assert(result == expected)
      }

    }

    "return a formatted string of the higher rate limit" when {

      "the income is above the basic rate limit and below the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(100000)
        val expected: String = "£125,140"
        assert(result == expected)
      }

      "the income is equal to the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(125140)
        val expected: String = "£125,140"
        assert(result == expected)
      }

    }

    "return a string of \"No Limit\"" when {

      "the income is above the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(200000)
        val expected: String = "No Limit"
        assert(result == expected)
      }


    }
  }
}
