import Vue from 'vue'
import App from './App'
import store from './store'
import router from './router'
import './assets/css/css.css'

import iView from 'iview'
import 'iview/dist/styles/iview.css'

Vue.config.productionTip = false

Vue.use(iView)

new Vue({
  router,
  store,
  render: h => h(App)
}).$mount('#app')
