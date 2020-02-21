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

import javax.sql.DataSource;
import java.sql.*;
import java.util.HashMap;
import java.util.Map;

/**
 * datasource utils for MySQL Jdbc.
 */
public class DataSourceUtil {
    private static final Map<String, DataSource> datasourceMap = new HashMap<>();
    
    /**
     * create data source for jdbc
     * @param userName user name
     * @param dataSourceName datasource name
     * @param host host
     * @param port port
     * @param password pwd
     */
    public static void createDataSource(final String userName, final String dataSourceName, final String host, final int port, final String password) {
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        config.setJdbcUrl(String.format("jdbc:mysql://%s:%s/%s?useSSL=false&serverTimezone=UTC", host, port, dataSourceName));
        config.setUsername(userName);
        config.setPassword(password);
        config.setMaximumPoolSize(200);
        config.setConnectionTimeout(30000);
        config.setIdleTimeout(60000);
        config.setMaxLifetime(1800000);
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
        datasourceMap.put(dataSourceName, dataSource);
    }
    
    /**
     * get datasource from name.
     * @param dataSourceName input name
     * @return datasource
     */
    public static DataSource getDataSource(final String dataSourceName) {
        return datasourceMap.get(dataSourceName);
    }
    
    /**
     * for master slave compare
     * @param datasource
     * @throws SQLException
     */
    public static void  writeReadOp(final String datasource) throws SQLException {
        String sql = "INSERT INTO ssperf (k,c,pad) VALUES (?,?,?)";
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            Long id = result.getLong(1);
            sql = "select count(id) from ssperf";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.executeQuery();
            sql = "select max(id) from ssperf ignore index(`PRIMARY`)";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.executeQuery();
            sql = "delete from ssperf where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1, 1);
            preparedStatement.setLong(2, id);
            preparedStatement.executeUpdate();
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
        }
    }
    
    public static void  writeOp(final String datasource) throws SQLException {
        String sql = "INSERT INTO ssperf (k,c,pad) VALUES (?,?,?)";
        Connection connection = null;
        try {
            connection = getDataSource(datasource).getConnection();
            PreparedStatement preparedStatement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, 1);
            preparedStatement.setString(2, "##-####");
            preparedStatement.setString(3, "##-####");
            preparedStatement.executeUpdate();
            ResultSet result = preparedStatement.getGeneratedKeys();
            result.next();
            Long id = result.getLong(1);
            sql = "update ssperf set c=?,pad =? where id=? and k=1";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setString(1, "##-#####");
            preparedStatement.setString(2, "##-#####");
            preparedStatement.setLong(3, id);
            preparedStatement.executeUpdate();
            sql = "delete from ssperf where k=? and id=?";
            preparedStatement = connection.prepareStatement(sql);
            preparedStatement.setInt(1, 1);
            preparedStatement.setLong(2, id);
            preparedStatement.executeUpdate();
            preparedStatement.close();
            result.close();
        }catch (final SQLException ex) {
            ex.printStackTrace();
        } finally {
            connection.close();
        }
        
    }
    
}

