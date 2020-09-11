package org.apache.shardingsphere.benchmark.db.shardingjdbc;

import org.apache.shardingsphere.driver.api.yaml.YamlShardingSphereDataSourceFactory;

import javax.sql.DataSource;
import java.io.*;
import java.sql.SQLException;


public class ShardingJDBCDataSourceFactory {

    private static final String FULLROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH = "/yaml/fullrouting/encrypt/shardingjdbc/config-shardingjdbc-fullrouting-encrypt.yaml";
    private static final String FULLROUTING_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH = "/yaml/fullrouting/masterslave/shardingjdbc/config-shardingjdbc-fullrouting-masterslave.yaml";
    private static final String FULLROUTING_SHARDING_SHARDINGJDBC_CONFIG_PATH = "/yaml/fullrouting/sharding/shardingjdbc/config-shardingjdbc-fullrouting-sharding.yaml";
    private static final String FULLROUTING_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH = "/yaml/fullrouting/sharding-masterslave-encrypt/shardingjdbc/config-shardingjdbc-fullrouting-sharding-masterslave-encrypt.yaml";
    private static final String RANGEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH = "/yaml/rangerouting/encrypt/shardingjdbc/config-shardingjdbc-rangerouting-encrypt.yaml";
    private static final String RANGEROUTING_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH = "/yaml/rangerouting/masterslave/shardingjdbc/config-shardingjdbc-rangerouting-masterslave.yaml";
    private static final String RANGEROUTING_SHARDING_SHARDINGJDBC_CONFIG_PATH = "/yaml/rangerouting/sharding/shardingjdbc/config-shardingjdbc-rangerouting-sharding.yaml";
    private static final String RANGEROUTING_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH = "/yaml/rangerouting/sharding-masterslave-encrypt/shardingjdbc/config-shardingjdbc-rangerouting-sharding-masterslave-encrypt.yaml";
    private static final String SINGLEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH = "/yaml/singlerouting/encrypt/shardingjdbc/config-shardingjdbc-singlerouting-encrypt.yaml";
    private static final String SINGLEROUTIN_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH = "/yaml/singlerouting/masterslave/shardingjdbc/config-shardingjdbc-singlerouting-masterslave.yaml";
    private static final String SINGLEROUTIN_SHARDING_SHARDINGJDBC_CONFIG_PATH = "/yaml/singlerouting/sharding/shardingjdbc/config-shardingjdbc-singlerouting-sharding.yaml";
    private static final String SINGLEROUTIN_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH = "/yaml/singlerouting/sharding-masterslave-encrypt/shardingjdbc/config-shardingjdbc-singlerouting-sharding-masterslave-encrypt.yaml";
    private static final String FULLROUTING_SMALLSHARDS_SHARDING_SHARDINGJDBC_CONFIG_PATH = "/yaml/fullrouting-smallshards/sharding/shardingjdbc/config-shardingjdbc-fullrouting-smallshards-sharding.yaml";
    private static final String FULLROUTING_SMALLSHARDS_SHARDING_MASTERSLAVE_ENCRYPT_SHARDINGJDBC_CONFIG_PATH = "/yaml/fullrouting-smallshards/sharding-masterslave-encrypt/shardingjdbc/config-shardingjdbc-fullrouting-smallshards-sharding-masterslave-encrypt.yaml";
    
    
    public static DataSource newInstance(ShardingConfigType shardingConfigType) throws IOException, SQLException {
        switch (shardingConfigType) {
            case FULLROUTING_ENCRYPT_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH));
            case FULLROUTING_MASTER_SLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH));
            case FULLROUTING_SHARDING_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_SHARDING_SHARDINGJDBC_CONFIG_PATH));
            case FULLROUTING_SHARDING_MASTERSLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH));
            case RANGEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(RANGEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH));
            case RANGEROUTING_MASTER_SLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(RANGEROUTING_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH));
            case RANGEROUTING_SHARDING_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(RANGEROUTING_SHARDING_SHARDINGJDBC_CONFIG_PATH));
            case RANGEROUTING_SHARDING_MASTERSLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(RANGEROUTING_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH));
            case SINGLEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(SINGLEROUTING_ENCRYPT_SHARDINGJDBC_CONFIG_PATH));
            case SINGLEROUTING_MASTER_SLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(SINGLEROUTIN_MASTERSLAVE_SHARDINGJDBC_CONFIG_PATH));
            case SINGLEROUTING_SHARDING_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(SINGLEROUTIN_SHARDING_SHARDINGJDBC_CONFIG_PATH));
            case FULLROUTING_SMALLSHARDS_SHARDING_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_SMALLSHARDS_SHARDING_SHARDINGJDBC_CONFIG_PATH));
            case FULLROUTING_SMALLSHARDS_SHARDING_MASTERSLAVE_ENCRYPT_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(FULLROUTING_SMALLSHARDS_SHARDING_MASTERSLAVE_ENCRYPT_SHARDINGJDBC_CONFIG_PATH));
            case SINGLEROUTING_SHARDING_MASTERSLAVE_SHARDINGJDBC_CONFIG:
                return YamlShardingSphereDataSourceFactory.createDataSource
                        (getFileContents(SINGLEROUTIN_SHARDING_SHARDINGMASTERSLAVEENCRYPT_CONFIG_PATH));
            default:
                throw new UnsupportedOperationException(shardingConfigType.name());
        }
    }

    private static File getFile(final String fileName) {
        return new File(ShardingJDBCDataSourceFactory.class.getResource(fileName).getFile());
    }

    public static byte[] getFileContents(String fileName) {
        byte[] yamlContentBytes = null;
        try {

            String line = "";
            StringBuilder yamlContent = new StringBuilder();

            InputStream in = ShardingJDBCDataSourceFactory.class.getResourceAsStream(fileName);
            BufferedReader br = new BufferedReader(new InputStreamReader(in));

            while ((line = br.readLine()) != null) {
                yamlContent.append(line).append("\n");
            }

            yamlContentBytes = yamlContent.toString().getBytes();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return yamlContentBytes;
    }

}

