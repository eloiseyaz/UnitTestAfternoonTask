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

      "the income is above the personal tax allowance but below the basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(40000)
        val basicTax: Int = 6000
        assert(result == basicTax)
      }

      "the income is above the basic rate limit but below the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(100000)
        val basicTax: Int = 8000
        val higherTax: Int = 20000
        assert(result == basicTax + higherTax)
      }

      "the income is above the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(200000)
        val basicTax: Int = 8000
        val higherTax: Int = 30000
        val additionalTax: Int = 33750
        assert(result == basicTax + higherTax + additionalTax)
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
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125000)
        assert(result)
      }

    }

    "return false" when {

      "the income is below the basic rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(20000)
        assert(!result)
      }

      "the income is equal to the basic rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(50000)
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
        val expected: String = "£10,000"
        assert(result == expected)
      }

      "the income is equal to the personal tax allowance" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(10000)
        val expected: String = "£10,000"
        assert(result == expected)
      }

    }

    "return a formatted string of the basic rate limit" when {

      "the income is above the personal allowance rate and below the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(20000)
        val expected: String = "£50,000"
        assert(result == expected)
      }

      "the income is equal to the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(50000)
        val expected: String = "£50,000"
        assert(result == expected)
      }

    }

    "return a formatted string of the higher rate limit" when {

      "the income is above the basic rate limit and below the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(100000)
        val expected: String = "£125,000"
        assert(result == expected)
      }

      "the income is equal to the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(125000)
        val expected: String = "£125,000"
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
