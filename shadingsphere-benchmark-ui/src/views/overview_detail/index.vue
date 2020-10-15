<!--
  - Licensed to the Apache Software Foundation (ASF) under one or more
  - contributor license agreements.  See the NOTICE file distributed with
  - this work for additional information regarding copyright ownership.
  - The ASF licenses this file to You under the Apache License, Version 2.0
  - (the "License"); you may not use this file except in compliance with
  - the License.  You may obtain a copy of the License at
  -
  -     http://www.apache.org/licenses/LICENSE-2.0
  -
  - Unless required by applicable law or agreed to in writing, software
  - distributed under the License is distributed on an "AS IS" BASIS,
  - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  - See the License for the specific language governing permissions and
  - limitations under the License.
  -->
<template>
  <Row :gutter="16" style="background:#eee; padding:20px">
    <Col
      v-for="(value, name, index) in series"
      :key="index"
      :xs="24"
      :sm="24"
      :md="24"
      :lg="24"
      :xl="24"
      :xxl="12"
      class="col-item"
    >
      <Card :bordered="false">
        <p slot="title" style="font-size: 18px">{{ name.toLowerCase() }}</p>
        <v-chart :options="getOptions(name)" />
        <Table
          :columns="columns"
          :data="[desc[name]]"
          size="large"
          style="margin-top: 20px;"
          border
        />
      </Card>
    </Col>
    <div v-show="loading" class="spin-container">
      <Spin fix>
        <Icon type="ios-loading" size="18" class="spin-icon-load" />
        <div>Loading</div>
      </Spin>
    </div>
    <Modal :title="titleRule" v-model="modal" width="600">
      <Input :rows="20" v-model="rules" type="textarea" readonly />
      <div slot="footer"></div>
    </Modal>
  </Row>
</template>

<script>
import ECharts from 'vue-echarts'
import 'echarts/lib/chart/line'
import 'echarts/lib/component/tooltip'
import 'echarts/lib/component/legend'
import 'echarts/lib/component/title'
import apis from '../../utils/page-apis'
import { mountedMixin } from '../../utils/mixin'
import { getLineOptions } from '../../utils/line'

import Vue from 'vue'
import { Modal, Spin, Col, Icon, Card, Row, Table, Button, Input } from 'iview'
Vue.component('Modal', Modal)
Vue.component('Spin', Spin)
Vue.component('Col', Col)
Vue.component('Icon', Icon)
Vue.component('Card', Card)
Vue.component('Row', Row)
Vue.component('Table', Table)
Vue.component('Button', Button)
Vue.component('Input', Input)

export default {
  name: 'OverviewDetailTest',
  components: {
    'v-chart': ECharts
  },
  mixins: [mountedMixin],
  data() {
    return {
      data: []
    }
  },
  watch: {
    '$store.state.global.fileData': {
      handler(val) {
        this.loading = true
        const fileName = this.$store.state.global.fileData[
          location.hash.split(`#/overview_detail/`)[1]
        ]
        apis.getOverviewDetail(fileName).then(res => {
          this.formatData(res)
          this.loading = false
        })
      },
      deep: true
    }
  },
  mounted() {
    if (!this.$store.state.global.fileData.length) return
    this.loading = true
    const fileName = this.$store.state.global.fileData[
      location.hash.split(`#/overview_detail/`)[1]
    ]
    apis.getOverviewDetail(fileName).then(res => {
      this.formatData(res)
      this.loading = false
    })
  },
  methods: {
    getOptions(name) {
      return getLineOptions(name, this.xAxis, this.legend, this.series)
    }
  }
}
</script>
<style>
.echarts {
  width: 100%;
  height: 400px;
}
.col-item {
  margin-bottom: 16px;
}
.ivu-form-item {
  margin-bottom: 10px !important;
}
.ivu-form-item-label {
  font-size: 15px !important;
}
.ivu-form-item-content {
  font-size: 14px !important;
}
</style>
