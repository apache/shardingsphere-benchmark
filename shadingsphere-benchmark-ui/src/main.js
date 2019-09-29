import Vue from 'vue'
import App from './App'
import router from './router'

import iView from 'iview'
import 'iview/dist/styles/iview.css'

Vue.config.productionTip = false

Vue.use(iView)

new Vue({
  router,
  render: h => h(App)
}).$mount('#app')
