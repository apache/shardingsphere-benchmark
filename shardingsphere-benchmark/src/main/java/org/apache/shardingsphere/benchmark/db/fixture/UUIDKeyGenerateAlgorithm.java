package org.apache.shardingsphere.benchmark.db.fixture;

import lombok.Getter;
import lombok.Setter;
import org.apache.shardingsphere.sharding.spi.KeyGenerateAlgorithm;

import java.util.Properties;
import java.util.UUID;

@Getter
@Setter
public final class UUIDKeyGenerateAlgorithm implements KeyGenerateAlgorithm {

    private Properties props = new Properties();

    @Override
    public synchronized Comparable<?> generateKey() {
        return UUID.randomUUID().toString().replaceAll("-", "");
    }

    @Override
    public String getType() {
        return "UUID";
    }

    @Override
    public void init() {

    }
}
