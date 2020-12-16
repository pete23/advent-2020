package advent;

import com.carrotsearch.hppc.procedures.LongLongProcedure;

public class SumProcedure implements LongLongProcedure {

    long sum = 0;

    public void apply(long key, long value) {
	sum += value;
    }

    public long getSum() {
	return sum;
    }
}
