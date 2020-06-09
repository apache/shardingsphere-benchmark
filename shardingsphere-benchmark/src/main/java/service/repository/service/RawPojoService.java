package service.repository.service;

import service.api.entity.Order;
import service.api.entity.OrderItem;
import service.api.repository.OrderItemRepository;
import service.api.repository.OrderRepository;
import service.api.service.CommonServiceImpl;
import service.repository.jdbc.JDBCOrderItemRepositoryImpl;
import service.repository.jdbc.JDBCOrderRepositoryImpl;

public class RawPojoService extends CommonServiceImpl {

    private final OrderRepository orderRepository;

    private final OrderItemRepository orderItemRepository;

    public RawPojoService(final JDBCOrderRepositoryImpl orderRepository, final JDBCOrderItemRepositoryImpl orderItemRepository) {
        this.orderRepository = orderRepository;
        this.orderItemRepository = orderItemRepository;
    }

    @Override
    protected OrderRepository getOrderRepository() {
        return orderRepository;
    }

    @Override
    protected OrderItemRepository getOrderItemRepository() {
        return orderItemRepository;
    }

    @Override
    protected Order newOrder() {
        return new Order();
    }

    @Override
    protected OrderItem newOrderItem() {
        return new OrderItem();
    }
}

