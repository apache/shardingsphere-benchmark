import Vue from 'vue'
import Vuex from 'vuex'

import * as actions from './actions'
import modules from './modules/index'

Vue.use(Vuex)

export default new Vuex.Store({
  actions,
  modules,
  strict: false
})
