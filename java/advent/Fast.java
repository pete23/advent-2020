package advent;

import com.carrotsearch.hppc.*;

public class Fast {
    public static void turboAssign(LongLongHashMap m, long[] addresses, long value) {
	for (int i = 0; i < addresses.length; i++) {
	    m.put(addresses[i], value);
	}
    }
}
