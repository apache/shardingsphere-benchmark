package test;

import javax.sql.DataSource;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.LinkedList;

import com.zaxxer.hikari.HikariDataSource;
import org.apache.shardingsphere.driver.api.yaml.YamlShardingSphereDataSourceFactory;
import org.apache.shardingsphere.infra.yaml.config.YamlRootRuleConfigurations;
import org.apache.shardingsphere.infra.yaml.config.YamlRuleConfiguration;
import org.apache.shardingsphere.infra.yaml.engine.YamlEngine;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import sjperf.v3.SQLStatement;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;


public class DatasourceTest {

    public static void main(String[] args){

        DataSource  ds = null;
        try {
            ds = SJPerfDataSourceOp.CreateMSDataSource();
            String SELECT_SQL_MASTER_SLAVE = SQLStatement.SELECT_SQL_SHARDING.getValue();
            System.out.println(SELECT_SQL_MASTER_SLAVE);
            SJPerfDataSourceUtil.getSelect(SELECT_SQL_MASTER_SLAVE, ds);
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }
}
