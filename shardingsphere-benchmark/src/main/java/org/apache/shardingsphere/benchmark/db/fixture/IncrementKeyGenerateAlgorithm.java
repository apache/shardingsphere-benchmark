package org.apache.shardingsphere.benchmark.db.fixture;

import lombok.Getter;
import lombok.Setter;
import org.apache.shardingsphere.sharding.spi.KeyGenerateAlgorithm;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

@Getter
@Setter
public class IncrementKeyGenerateAlgorithm implements KeyGenerateAlgorithm {
    private static final AtomicInteger SEQUENCE = new AtomicInteger(110);

    private Properties props = new Properties();

    @Override
    public Comparable<?> generateKey() {
        return SEQUENCE.incrementAndGet();
    }

    @Override
    public String getType() {
        return "INCREMENT";
    }

    @Override
    public void init() {

    }
}
