import clone from 'lodash/clone'
import * as types from '../mutation-types'

const state = {
  activeName: 'overview',
  fileData: []
}

const mutations = {
  [types.SET_MENU](state, params) {
    state.activeName = clone(params)
  },
  [types.SET_FILE_DATA](state, params) {
    state.fileData = clone(params)
  }
}

export default {
  state,
  mutations
}
