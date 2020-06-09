package service.api.service;

import java.sql.SQLException;

public interface CommonService {

    void initEnvironment();

    void cleanEnvironment();

    void processSuccess(boolean isRangeSharding, String sql) throws SQLException;

    void processFailure();

    void printData(boolean isRangeSharding, String sql) throws SQLException;
}
