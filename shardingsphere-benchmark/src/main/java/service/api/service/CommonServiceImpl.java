package service.api.service;

import service.api.entity.Order;
import service.api.entity.OrderItem;
import service.api.repository.OrderItemRepository;
import service.api.repository.OrderRepository;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public abstract class CommonServiceImpl implements CommonService {

    @Override
    public void initEnvironment() {
        getOrderRepository().createTableIfNotExists();
        getOrderItemRepository().createTableIfNotExists();
        getOrderRepository().truncateTable();
        getOrderItemRepository().truncateTable();
        insertData();
    }

    @Override
    public void cleanEnvironment() {
        getOrderRepository().dropTable();
        getOrderItemRepository().dropTable();
    }

    @Override
    public void processSuccess(final boolean isRangeSharding, String sql) throws SQLException {
        printData(isRangeSharding, sql);
    }

    @Override
    public void processFailure() {
        insertData();
        throw new RuntimeException("Exception occur for transaction test.");
    }

    private List<Long> insertData() {
        List<Long> result = new ArrayList<>(10);
        for (int i = 1; i <= 10; i++) {
            Order order = newOrder();
            order.setUserId(i);
            order.setStatus("INSERT_TEST");
            getOrderRepository().insert(order);
            OrderItem item = newOrderItem();
            item.setOrderId(order.getOrderId());
            item.setUserId(i);
            item.setStatus("INSERT_TEST");
            getOrderItemRepository().insert(item);
            result.add(order.getOrderId());
        }
        return result;
    }

    private void deleteData(final List<Long> orderIds) {
        for (Long each : orderIds) {
            getOrderRepository().delete(each);
            getOrderItemRepository().delete(each);
        }
    }

    @Override
    public void printData(final boolean isRangeSharding, String sql) throws SQLException {
        if (isRangeSharding) {
            printDataRange();
        } else {
            printDataAll(sql);
        }
    }

    private void printDataRange() throws SQLException {
        for (Object each : getOrderRepository().selectRange()) {
        }
        for (Object each : getOrderItemRepository().selectRange()) {
        }
    }

    private void printDataAll(String sql) throws SQLException {
        for (Object each : getOrderRepository().selectAll(sql)) {
        }
    }

    protected abstract OrderRepository getOrderRepository();

    protected abstract OrderItemRepository getOrderItemRepository();

    protected abstract Order newOrder();

    protected abstract OrderItem newOrderItem();
}
