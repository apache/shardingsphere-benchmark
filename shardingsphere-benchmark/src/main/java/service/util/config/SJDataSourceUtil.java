/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package service.util.config;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import service.api.entity.Iou;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

/**
 * datasource utils for sharding jdbc.
 * @author nancyzrh
 */
public class SJDataSourceUtil {
    private static final String DEFAULT_SCHEMA = "test";
    
    public static DataSource createDataSource(final String usrName, final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(usrName);
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
        return dataSource;
    }
    
    /**
     * insert data for update.
     * @param sql stmt
     * @param datasource datasource from service
     * @throws SQLException ex
     */
    public static void insertDemo(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
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
     * for select stmt.
     * @param sql stmt
     * @param dataSource datasource from service
     * @throws SQLException ex
     */
    public static void getSelect(final String sql, final DataSource dataSource) throws SQLException {
        List<Iou> result = new LinkedList<>();
        try (Connection connection = dataSource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql);
             ResultSet resultSet = preparedStatement.executeQuery()) {
            while (resultSet.next()) {
                Iou iou = new Iou();
                iou.setK(resultSet.getInt(2));
                result.add(iou);
            }
        }
    }
    
    /**
     * for update stmt.
     * @param sql stmt
     * @param datasource datasource from service
     * @return res
     * @throws SQLException ex
     */
    public static int updateStmt(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setString(1, "##-#####");
            preparedStatement.setString(2, "##-#####");
            preparedStatement.setInt(3, 1);
            preparedStatement.setInt(4, 1);
            return preparedStatement.executeUpdate();
        }
    }
    
    /**
     * for delete stmt.
     * @param sql stmt
     * @param datasource datasource from service
     * @return res
     * @throws SQLException ex
     */
    public static int delete(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setInt(2, 1);
            return preparedStatement.executeUpdate();
        }
    }
    
    /**
     * for clean up.
     * @param sql stmt
     * @param datasource datasource
     * @throws SQLException ex
     */
    public static void clean(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.executeUpdate();
        }
    }
    
    /**
     * for insert stmt.
     * @param sql stmt
     * @param datasource datasource
     * @throws SQLException ex
     */
    public static void insert(final String sql, final DataSource datasource) throws SQLException {
        try (Connection connection = datasource.getConnection();
             java.sql.PreparedStatement preparedStatement = connection.prepareStatement(sql)) {
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.execute();
        } catch (final SQLException ex) {
            ex.printStackTrace();
        }
    }
    
}
