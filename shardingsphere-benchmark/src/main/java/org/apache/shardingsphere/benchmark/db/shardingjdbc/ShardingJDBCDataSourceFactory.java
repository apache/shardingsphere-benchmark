package org.apache.shardingsphere.benchmark.db.shardingjdbc;

import org.apache.shardingsphere.shardingjdbc.api.yaml.YamlShardingDataSourceFactory;

import javax.sql.DataSource;
import java.io.*;
import java.sql.SQLException;

/**
 *  Sharding scenario which to create DataSource based on different yaml config.
 *
 */
public class ShardingJDBCDataSourceFactory {

    private static final String ENCRYPT_CONFIG_PATH = "/yaml/shardingjdbc/encrypt-config.yaml";

    private static final String MASTER_SLAVE_CONFIG_PATH = "/yaml/shardingjdbc/master-slave-config.yaml";

    private static final String MASTER_SLAVE_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/master-slave-sharding-config.yaml";

    private static final String SINGLE_DATABASE_SINGLE_TABLE_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/single-db-single-table-sharding-config.yaml";

    private static final String ALL_DATABASE_SINGLE_TABLE_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/all-db-single-table-sharding-config.yaml";

    private static final String ALL_DATABASE_SINGLE_TABLE_SHARDING_NEW_CONFIG_PATH = "/yaml/shardingjdbc/all-db-single-table-sharding-new-config.yaml";

    private static final String ALL_DATABASE_ALL_TABLE_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/all-db-all-table-sharding-config.yaml";

    private static final String MASTER_SLAVE_ENCRYPT_SHARDING_CONFIG_PATH = "/yaml/shardingjdbc/master-slave-encrypt-sharding-config.yaml";


    /**
     * Create datasource by configType
     *
     * @param shardingConfigType
     * @return
     */
    public static DataSource newInstance(ShardingConfigType shardingConfigType) throws IOException, SQLException {
        switch (shardingConfigType) {
            case SINGLE_DATABASE_SINGLE_TABLE_SHARDING_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(SINGLE_DATABASE_SINGLE_TABLE_SHARDING_CONFIG_PATH));

            case ALL_DATABASE_SINGLE_TABLE_SHARDING_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(ALL_DATABASE_SINGLE_TABLE_SHARDING_CONFIG_PATH));

            case ALL_DATABASE_SINGLE_TABLE_SHARDING_NEW_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(ALL_DATABASE_SINGLE_TABLE_SHARDING_NEW_CONFIG_PATH));

            case ALL_DATABASE_ALL_TABLE_SHARDING_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(ALL_DATABASE_ALL_TABLE_SHARDING_CONFIG_PATH));

            case ENCRYPT_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(ENCRYPT_CONFIG_PATH));

            case MASTER_SLAVE_ENCRYPT_SHARDING_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(MASTER_SLAVE_ENCRYPT_SHARDING_CONFIG_PATH));

            case MASTER_SLAVE_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(MASTER_SLAVE_CONFIG_PATH));

            case MASTER_SLAVE_SHARDING_CONFIG:
                return YamlShardingDataSourceFactory.createDataSource
                        (getFileContents(MASTER_SLAVE_SHARDING_CONFIG_PATH));

            default:
                throw new UnsupportedOperationException(shardingConfigType.name());
        }
    }


    /**
     * Get yaml file by fileName.
     *
     * @param fileName
     * @return
     */
    private static File getFile(final String fileName) {
        return new File(ShardingJDBCDataSourceFactory.class.getResource(fileName).getFile());
    }


    /**
     * Get yaml file content bytes by file name.
     *
     * @param fileName
     * @return
     */
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

            yamlContentBytes =  yamlContent.toString().getBytes();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return yamlContentBytes;
    }

}
