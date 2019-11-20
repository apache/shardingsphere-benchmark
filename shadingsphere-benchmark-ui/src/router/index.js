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
    path: '/overview_detail/:id',
    component: () => import('@/views/overview_detail'),
    hidden: true
  }
]

const router = new Router({
  scrollBehavior: () => ({ y: 0 }),
  routes: constantRouterMap
})

export default router
