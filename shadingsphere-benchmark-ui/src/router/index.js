import Vue from 'vue'
import Router from 'vue-router'
Vue.use(Router)

export const constantRouterMap = [
  {
    path: '*',
    redirect: '/overview'
  },
  {
    path: '/overview',
    component: () => import('@/views/overview/index'),
    hidden: true
  },
  {
    path: '/mysql-vs-sharding',
    component: () => import('@/views/mysql-vs-sharding/index'),
    hidden: true
  },
  {
    path: '/sharding-proxy-master-slave',
    component: () => import('@/views/sharding-proxy-master-slave/index'),
    hidden: true
  },
  {
    path: '/sharding-proxy-master-slave-sharding',
    component: () =>
      import('@/views/sharding-proxy-master-slave-sharding/index'),
    hidden: true
  },
  {
    path: '/sharding-proxy-single-database-single-table',
    component: () =>
      import('@/views/sharding-proxy-single-database-single-table/index'),
    hidden: true
  },
  {
    path: '/shardingjdbc-vs-shardingproxy-encrypt',
    component: () =>
      import('@/views/shardingjdbc-vs-shardingproxy-encrypt/index'),
    hidden: true
  },
  {
    path: '/shardingjdbc-vs-shardingproxy-sharding-encrypt',
    component: () =>
      import('@/views/shardingjdbc-vs-shardingproxy-sharding-encrypt/index'),
    hidden: true
  }
]

export default new Router({
  scrollBehavior: () => ({ y: 0 }),
  routes: constantRouterMap
})
