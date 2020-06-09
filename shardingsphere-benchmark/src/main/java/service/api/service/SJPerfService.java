package service.api.service;

import javax.sql.DataSource;
import java.sql.SQLException;

public class SJPerfService {
    public final DataSource dataSource;

    public SJPerfService (DataSource dataSource) throws SQLException {
        this.dataSource = dataSource;
    }
}
