package scalaads

trait BloomFilter[A] extends (A => Boolean) {

  def +(e: A): BloomFilter[A]

	/**
	 * False indicates that 'e' was definitely not added to this Bloom Filter, 
	 * true indicates that it probably was. The probability can be estimated
	 * using the falsePositiveRate() method.
	 */
  def contains(e: A): Boolean

  def apply(e: A) = contains(e)

	/**
	 * The number of bits in the bit array,
   * often called 'm' in the context of bloom filters.
   */
  def size: Int

  /**
   * A list of hash functions that are used to set and test the bits.
   * These hash functions map each added item to a random number
   * uniform over the range {1..size} for mathematical convenience.
	 * The number of hash functions is often called 'k' in the context of bloom filters.
   */
  def hashes: List[A => Int]

	/**
	 * Calculates the approximate probability of the contains() method returning
	 * true for an object that had not previously been inserted into the bloom
	 * filter. This is known as the "false positive probability".
	 */
  def falsePositiveRate: Double

}

object BloomFilter {



}

class SimpleBloomFilter[A](val bitArraySize: Int,
                           val expectedElements: Int,
                           val hashes: List[A => Int]) extends BloomFilter[A] {

  import scala.collection.mutable.BitSet

  private val k = hashes.size
  private val bits: BitSet = new BitSet(bitArraySize);

  def +(e: A): BloomFilter[A] = {
    hashes foreach { hash => bits += (hash(e).abs % size) }
    this
  }

  def contains(e: A) = hashes find { hash => !bits(hash(e).abs % size) } match {
    case Some(_) => false
    case None => true
  }

  def size = bits.capacity

  def falsePositiveRate = 
    Math.pow((1 - Math.exp(-k * expectedElements.toDouble	/ bitArraySize.toDouble)), k);

}

