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
    component: () => import('@/views/overview'),
    hidden: true
  },
  {
    path: '/encrypt_sharding_test',
    component: () => import('@/views/encrypt_sharding_test'),
    hidden: true
  },
  {
    path: '/encrypt_test',
    component: () => import('@/views/encrypt_test'),
    hidden: true
  },
  {
    path: '/loss_test',
    component: () => import('@/views/loss_test'),
    hidden: true
  },
  {
    path: '/master_slave_sharding_test',
    component: () => import('@/views/master_slave_sharding_test'),
    hidden: true
  },
  {
    path: '/master_slave_test',
    component: () => import('@/views/master_slave_test'),
    hidden: true
  },
  {
    path: '/sharding_test',
    component: () => import('@/views/sharding_test'),
    hidden: true
  },
  {
    path: '/promotion_test',
    component: () => import('@/views/promotion_test'),
    hidden: true
  }
]

const router = new Router({
  scrollBehavior: () => ({ y: 0 }),
  routes: constantRouterMap
})

export default router
