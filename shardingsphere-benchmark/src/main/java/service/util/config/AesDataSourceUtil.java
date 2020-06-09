package service.util.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import service.api.entity.Iou;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class AesDataSourceUtil {
    private static final String USER_NAME = "####";
    private static final String DEFAULT_SCHEMA = "test";
    private static final Map<String, DataSource> datasourceMap = new HashMap<>();

    /**
     * create data source
     * @param dataSourceName
     * @param host
     * @param port
     * @param password
     */
    public static void createDataSource(final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(USER_NAME);
        config.setPassword(password);
        config.setMaximumPoolSize(200);
        config.addDataSourceProperty("useServerPrepStmts", Boolean.TRUE.toString());
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", 250);
        config.addDataSourceProperty("prepStmtCacheSqlLimit", 2048);
        config.addDataSourceProperty("useLocalSessionState", Boolean.TRUE.toString());
        config.addDataSourceProperty("rewriteBatchedStatements", Boolean.TRUE.toString());
        config.addDataSourceProperty("cacheResultSetMetadata", Boolean.TRUE.toString());
        config.addDataSourceProperty("cacheServerConfiguration", Boolean.TRUE.toString());
        config.addDataSourceProperty("elideSetAutoCommits", Boolean.TRUE.toString());
        config.addDataSourceProperty("maintainTimeStats", Boolean.FALSE.toString());
        config.addDataSourceProperty("netTimeoutForStreamingResults", 0);
        DataSource dataSource = new HikariDataSource(config);
        Properties properties = new Properties();
      //  EncryptRuleConfiguration ruleConfiguration = createEncryptRuleConfiguration();
      //  try {
       //     dataSource = EncryptDataSourceFactory.createDataSource(dataSource, ruleConfiguration, properties);
       // } catch (final SQLException ignored) {
       // }
        datasourceMap.put(dataSourceName, dataSource);
    }
    
    /**
     * get datasource
     * @param dataSourceName
     * @return datasource
     */
    public static DataSource getDataSource(final String dataSourceName) {
        return datasourceMap.get(dataSourceName);
    }

    /**
     * create default schema
     * @param dataSourceName
     */
    public static void createSchema(final String dataSourceName) {
        String sql = "CREATE DATABASE " + dataSourceName;
        try (Connection connection = getDataSource(DEFAULT_SCHEMA).getConnection();
             Statement statement = connection.createStatement()) {
            statement.execute(sql);
        } catch (final SQLException ignored) {
        }
    }

    /**
     * insert demo data
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void insertDemo(final String sql, String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setInt(2, 1);
            preparedStatement.setString(3, "##-####");
            preparedStatement.setString(4, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ignored) {
        }
    }

    /**
     * for select
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static List<Iou> getIou(final String sql, String datasource) throws SQLException {
        List<Iou> result = new LinkedList<>();
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql);
             ResultSet resultSet = preparedStatement.executeQuery()) {
            while (resultSet.next()) {
                Iou iou = new Iou();
                iou.setK(resultSet.getInt(2));
                result.add(iou);
            }
        }
        return result;
    }

    /**
     * for update
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static int updateStmt(final String sql, String datasource) throws SQLException{
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1,"##-#####");
            preparedStatement.setString(2,"##-#####");
            preparedStatement.setInt(3,1);
            preparedStatement.setInt(4,1);
            return preparedStatement.executeUpdate();
        }
    }

    /**
     * for insert performance
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void insertIou(final String sql, String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * clean up environment
     * @param sql
     * @param datasource
     * @throws SQLException
     */
    public static void clean(final String sql, String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.executeUpdate();
        }
    }

    /**
     * for delete performance
     * @param sql
     * @param datasource
     * @return
     * @throws SQLException
     */
    public static int deleteIou(final String sql, String datasource) throws SQLException {
        try (Connection connection = getDataSource(datasource).getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1,1);
            preparedStatement.setInt(2,1);
            return preparedStatement.executeUpdate();
        }
    }

}
