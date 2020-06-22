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
import org.apache.shardingsphere.shardingjdbc.api.yaml.YamlMasterSlaveDataSourceFactory;
import org.apache.shardingsphere.shardingjdbc.api.yaml.YamlShardingDataSourceFactory;
import service.util.config.sjperf.SJPerfDataSourceOp;
import service.util.config.sjperf.SJPerfDataSourceUtil;
import sjperf.v3.SQLStatement;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;


public class DatasourceTest {

    public static void main(String[] args){

        DataSource  ds = null;
        //String fileName = "/yaml/shardingjdbc/master-slave-config.yaml";
        String fileName ="/yaml/shardingjdbc/master-slave-config.yaml";
        File  yamlFile = new File(DatasourceTest.class.getResource(fileName).getFile());
        String sql = "SELECT id,k from sbtest where id=1 and k=1";

        try {
            //ds = YamlShardingDataSourceFactory.createDataSource(yamlFile);
            System.out.println(yamlFile);
            //ds = YamlMasterSlaveDataSourceFactory.createDataSource(yamlFile);
            ds = YamlShardingDataSourceFactory.createDataSource(yamlFile);
            String SELECT_SQL_MASTER_SLAVE = SQLStatement.SELECT_SQL_MASTER_SLAVE.getValue();
            //ds = SJPerfDataSourceOp.CreateEncryptDataSource();
            Connection connection = ds.getConnection();
            System.out.println(sql);
            PreparedStatement ps = connection.prepareStatement(sql);
            ResultSet resultSet = ps.executeQuery();
            while (resultSet.next()) {
                System.out.println("id: " + resultSet.getInt(1) + " , " + "k: " + resultSet.getInt(2));
            }

            connection.close();

        } catch (SQLException throwables) {
            throwables.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
