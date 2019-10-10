import clone from 'lodash/clone'
import * as types from '../mutation-types'

const state = {
  activeName: 'overview'
}

const mutations = {
  [types.SET_MENU](state, params) {
    state.activeName = clone(params)
  }
}

export default {
  state,
  mutations
}
