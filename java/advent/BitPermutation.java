package advent;

public class BitPermutation {

    public static byte[] bitsSet(long l) {
	byte[] set = new byte[Long.bitCount(l)];
	int i = 0;
	for (byte b = 0; i < set.length; b++) {
	    if ((l & 1) == 1) {
		set[i++] = b;
	    }
	    l = l >> 1;
	}
	return set;
    }
}
